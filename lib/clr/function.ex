defmodule Clr.Function do
  @moduledoc false

  # stores evaluated AIR functions in an ets table for retrieval.
  # if you make a request to evaluate an AIR function, the server will check if
  # it has already been evaluated.  If it hasn't, then, it will trigger evaluation
  # and store the result in the ets table.

  use GenServer

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
    analyzer = Keyword.get(opts, :analyzer, __MODULE__)
    Process.put(:analyzer, analyzer)
    Process.put(Clr.Function.TableName, name)
    Process.flag(:trap_exit, true)

    :ets.new(name, [:named_table, :set, :public])
    {:ok, %{}, :hibernate}
  end

  # when you "evaluate" a function, you are asking the server to
  # "evaluate" the function.  
  alias Clr.Block

  @type block_mapper :: (Block.t() -> Block.t())

  @spec evaluate(term, [Clr.meta()], [Clr.slot()]) ::
          {:future, reference} | {Clr.type(), block_mapper}
  def evaluate(function_name, args, arg_slots) do
    # TODO: remove the next clause
    Enum.each(args, &(is_map(&1) or raise("did not get metadata")))

    case :ets.lookup(table_name(), {function_name, args}) do
      [{_, {result, remapper}}] -> {result, &remapper.(&1, arg_slots)}
      [] -> GenServer.call(table_name(), {:evaluate, function_name, args, arg_slots})
    end
  end

  @type future :: {:future, reference}
  @spec evaluate_impl(term, [Clr.type()], [Clr.slot()], GenServer.from(), waiters) ::
          {:reply, future, waiters}
  defp evaluate_impl(function_name, args, arg_slots, {pid, _ref}, waiters) do
    waiter_id_key = {function_name, args}
    table_name = table_name()

    case Map.fetch(waiters, waiter_id_key) do
      {:ok, {{_pid, future_ref} = future_info, pids}} ->
        {:reply, {:future, future_ref},
         Map.replace!(waiters, waiter_id_key, {future_info, [pid | pids]})}

      :error ->
        # we might be doing a dependency injection to the analyzer module.
        analyzer = Process.get(:analyzer)

        # do the evaluation here.  Response is obtained as a Task.async response.
        future =
          Task.async(fn ->
            if !Clr.debug_prefix(), do: Logger.disable(self())

            with {:ok, block} <- analyzer.do_evaluate(function_name, args),
                 {:ok, block} <- Block.flush_awaits(block) do
              # obtain the block remapper and store it in the table.
              remapper = Block.call_meta_adder(block)
              :ets.insert(table_name, {waiter_id_key, {block.return, remapper}})

              # return the result, with the arg slots remapped.
              {:ok, block.return, &remapper.(&1, arg_slots)}
            end
          end)

        future_info = {future.pid, future.ref}

        {:reply, {:future, future.ref}, Map.put(waiters, waiter_id_key, {future_info, [pid]})}
    end
  end

  def debug_get_table(function_name, args) do
    [{_, result}] = :ets.lookup(table_name(), {function_name, args})

    result
  end

  @spec await(reference) :: {:ok, {Clr.type(), block_mapper}}
  def await(ref) when is_reference(ref) do
    receive do
      {^ref, {:ok, type, lambda}} when is_function(lambda, 1) -> {:ok, {type, lambda}}
      {^ref, {:error, _} = error} -> error
      other -> raise inspect(other)
    end
  end

  def debug_insert_result(function_name, args, result) do
    :ets.insert(table_name(), {{function_name, args}, result})
  end

  def handle_call({:evaluate, function_name, args, arg_slots}, from, waiters),
    do: evaluate_impl(function_name, args, arg_slots, from, waiters)

  def handle_info({ref, result} = response, waiters) when is_reference(ref) do
    function_call =
      Enum.find_value(waiters, fn
        {function_call, {{_task_pid, ^ref}, pids}} ->
          # clean up the DOWN message
          receive do
            {:DOWN, ^ref, :process, _, _reason} -> :ok
          end

          # forward the result to the pids
          Enum.each(pids, &send(&1, response))
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

  def handle_info(_, waiters), do: {:noreply, waiters}

  # common utility functions
  def table_name do
    Process.get(Clr.Function.TableName, __MODULE__)
  end
end
