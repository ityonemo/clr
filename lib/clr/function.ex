defmodule Clr.Function do
  @moduledoc false

  # stores evaluated AIR functions in an ets table for retrieval.
  # if you make a request to evaluate an AIR function, the server will check if
  # it has already been evaluated.  If it hasn't, then, it will trigger evaluation
  # and store the result in the ets table.

  use GenServer

  require Logger

  alias Clr.Air.Lvalue
  alias Clr.Block

  def start_link(opts) do
    name = Keyword.get(opts, :name, __MODULE__)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  @type waiter_id :: {name :: term, args :: []}
  @type future_info :: {pid, reference}
  @type waiters :: %{optional(waiter_id) => {future_info, [pid]}}
  @spec init([]) :: {:ok, waiters, :hibernate}

  def init(opts) do
    name = Keyword.get(opts, :name, __MODULE__)
    # allow for analyzer dependency injection here.
    analyzer = Keyword.get(opts, :analyzer, Block)
    Process.put(:analyzer, analyzer)
    Process.put(Clr.Function.TableName, name)
    Process.flag(:trap_exit, true)

    :ets.new(name, [:named_table, :set, :public])
    {:ok, %{}, :hibernate}
  end

  @type block_mapper :: (Block.t() -> Block.t())

  @spec evaluate(term, Clr.slot(), [Clr.type()], [Clr.slot() | nil], Clr.type()) ::
          {:future, reference} | {Clr.type(), block_mapper}
  def evaluate(function_name, at, args, arg_slots, ret_type) do
    Logger.debug(
      "evaluation of #{Lvalue.as_string(function_name)} requested by #{inspect(self())}."
    )

    case :ets.lookup(table_name(), {function_name, args}) do
      [{_, {result, resolver}}] ->
        Logger.debug("evaluation of #{Lvalue.as_string(function_name)} found in table.")
        {result, &resolver.(&1, at, arg_slots)}

      [] ->
        GenServer.call(table_name(), {:evaluate, function_name, at, args, arg_slots, ret_type})
    end
  end

  @type future :: {:future, reference}
  @spec evaluate_impl(
          term,
          Clr.slot(),
          [Clr.type()],
          [Clr.slot()],
          Clr.type(),
          GenServer.from(),
          waiters
        ) ::
          {:reply, future, waiters}
  defp evaluate_impl(function_name, at, args, arg_slots, ret_type, {req_pid, _ref}, waiters) do
    waiter_id_key = {function_name, args}
    table_name = table_name()

    case Map.fetch(waiters, waiter_id_key) do
      {:ok, {{pid, future_ref} = future_info, pids}} ->
        Logger.debug(
          "evaluation of function #{Lvalue.as_string(function_name)} running on #{inspect(pid)}, ref: #{inspect(future_ref)}"
        )

        {:reply, {:future, future_ref},
         Map.replace!(waiters, waiter_id_key, {future_info, [req_pid | pids]})}

      :error ->
        # we might be doing a dependency injection to the analyzer module.
        analyzer = Process.get(:analyzer)

        # do the evaluation here.  Response is obtained as a Task.async response.
        future =
          Task.async(fn ->
            Logger.debug("evaluation of #{Lvalue.as_string(function_name)} started.")
            if !Clr.debug_prefix(), do: Logger.disable(self())

            function = Clr.Air.get(function_name)

            %{return: return} =
              block =
              function
              |> Block.new(args, ret_type)
              |> analyzer.analyze(function.code)

            resolver = Block.make_call_resolver(block)

            :ets.insert(table_name, {waiter_id_key, {block, resolver}})

            Logger.debug("evaluation of #{Lvalue.as_string(function_name)} completed.")

            {:ok, return, &resolver.(&1, at, arg_slots)}
          end)

        Logger.debug(
          "evaluation of function #{Lvalue.as_string(function_name)} running on #{inspect(future.pid)}, ref: #{inspect(future.ref)}"
        )

        future_info = {future.pid, future.ref}
        {:reply, {:future, future.ref}, Map.put(waiters, waiter_id_key, {future_info, [req_pid]})}
    end
  end

  def debug_get_table(function_name, args) do
    [{_, result}] = :ets.lookup(table_name(), {function_name, args})
    result
  end

  def debug_get_table(), do: :ets.tab2list(table_name())

  @spec await(reference) :: {:ok, {Clr.type(), block_mapper}}
  def await(ref) when is_reference(ref) do
    Logger.debug("awaiting result of #{inspect(ref)} on #{inspect(self())}")

    receive do
      {^ref, {:ok, type, lambda}} when is_function(lambda, 1) -> {:ok, {type, lambda}}
      {^ref, {:error, _} = error} -> error
      other -> raise inspect(other)
    end
  end

  def debug_insert_result(function_name, args, result) do
    :ets.insert(table_name(), {{function_name, args}, result})
  end

  def handle_call({:evaluate, function_name, at, args, arg_slots, ret_type}, from, waiters),
    do: evaluate_impl(function_name, at, args, arg_slots, ret_type, from, waiters)

  def handle_info({ref, _result} = response, waiters) when is_reference(ref) do
    function_call =
      Enum.find_value(waiters, fn
        {{name, _} = function_call, {{_task_pid, ^ref}, pids}} ->
          # clean up the DOWN message
          receive do
            {:DOWN, ^ref, :process, _, _reason} -> :ok
          end

          # forward the result to the pids
          Enum.each(pids, fn pid ->
            Logger.debug(
              "sending result to #{Lvalue.as_string(name)} to #{inspect(pid)}, ref: #{inspect(ref)}"
            )

            send(pid, response)
          end)

          function_call

        _ ->
          nil
      end)

    {:noreply, Map.delete(waiters, function_call)}
  end

  def handle_info({:DOWN, ref, :process, task_pid, reason}, waiters) do
    function_call =
      Enum.find_value(waiters, fn
        {function_call, {{^task_pid, ^ref}, pids}} ->
          Enum.each(pids, &send(&1, {ref, {:error, reason}}))
          function_call

        _ ->
          nil
      end)

    {:noreply, Map.delete(waiters, function_call)}
  end

  def handle_info({:EXIT, _pid, _}, waiters), do: {:noreply, waiters}

  def handle_info(info, waiters) do
    Logger.warning("unexpected message received by function server: #{inspect(info)}")
    {:noreply, waiters}
  end

  # common utility functions
  def table_name do
    Process.get(Clr.Function.TableName, __MODULE__)
  end
end
