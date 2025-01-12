defmodule Clr.Analysis do
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
    Process.put(Clr.Analysis.TableName, name)
    Process.flag(:trap_exit, true)

    :ets.new(name, [:named_table, :set, :public])
    {:ok, %{}, :hibernate}
  end

  def evaluate(function_name, args) do
    case :ets.lookup(table_name(), {function_name, args}) do
      [{_, result}] -> result
      [] -> GenServer.call(table_name(), {:evaluate, function_name, args})
    end
  end

  defp evaluate_impl(function_name, args, {pid, _ref}, waiters) do
    waiter_id_key = {function_name, args}

    case Map.fetch(waiters, waiter_id_key) do
      {:ok, {{_pid, future_ref} = future_info, pids}} ->
        {:reply, {:future, future_ref},
         Map.replace!(waiters, waiter_id_key, {future_info, [pid | pids]})}

      :error ->
        # do the evaluation here.  Response is obtained as a Task.async response.
        analyzer = Process.get(:analyzer)

        future =
          Task.async(fn ->
            if !Clr.debug_prefix(), do: Logger.disable(self())

            with {:ok, analysis} <- analyzer.do_evaluate(function_name, args),
                 {:ok, analysis} <- process_awaited(analysis) do
              {:ok, {analysis.return, analysis.reqs}}
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

  def await(ref) when is_reference(ref) do
    receive do
      {^ref, {_, lambda} = type_lambda} when is_function(lambda, 1) -> {:ok, type_lambda}
      {^ref, {:error, _} = error} -> error
      other -> raise inspect(other)
    end
  end

  def debug_insert_result(function_name, args, result) do
    :ets.insert(table_name(), {{function_name, args}, result})
  end

  def handle_call({:evaluate, function_name, args}, from, waiters),
    do: evaluate_impl(function_name, args, from, waiters)

  def handle_info({ref, result} = response, waiters) when is_reference(ref) do
    function_call =
      Enum.find_value(waiters, fn
        {function_call, {{_task_pid, ^ref}, pids}} ->
          # clean up the DOWN message
          receive do
            {:DOWN, ^ref, :process, _, _reason} -> :ok
          end

          # stash the result in the table.
          :ets.insert(table_name(), {function_call, result})

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
    Process.get(Clr.Analysis.TableName, __MODULE__)
  end

  ## FUNCTION EVALUATION

  defstruct [:name, :args, :reqs, :row, :col, :return, awaits: [], slots: %{}]

  # TODO: go through and rename "lines" to "slots"
  @type slot_spec :: {term, keyword}

  @type t :: %__MODULE__{
          name: term,
          args: [term],
          reqs: [keyword],
          row: non_neg_integer(),
          col: non_neg_integer(),
          return: term,
          awaits: [{pid, reference}],
          slots: %{optional(non_neg_integer()) => slot_spec}
        }

  alias Clr.Air.Instruction

  def put_type(analysis, slot, type) do
    if match?({_, %Clr.Analysis{}}, type), do: raise("eepers")
    %{analysis | slots: Map.put(analysis.slots, slot, type)}
  end

  def put_future(analysis, future) do
    %{analysis | awaits: [future | analysis.awaits]}
  end

  @spec fetch!(t, non_neg_integer()) :: {term, t}
  def fetch!(analysis, slot) do
    case Map.fetch!(analysis.slots, slot) do
      {:future, future} ->
        case await(future) do
          {type, requirements} ->
            {type, put_requirements(analysis, requirements)}
        end

      type ->
        {type, analysis}
    end
  end

  defp put_requirements(_analysis, _requirements) do
    raise "unimplemented"
  end

  def fetch_arg!(analysis, index) do
    Enum.at(analysis.args, index) || raise "Invalid argument index #{index}"
  end

  def update_arg!(analysis, index, transformation) do
    Map.update!(analysis, :args, &List.update_at(&1, index, transformation))
  end

  def update_req!(analysis, index, transformation) do
    Map.update!(analysis, :reqs, &List.update_at(&1, index, transformation))
  end

  # this private function is made public for testing.
  def do_evaluate(function_name, arguments) do
    function_name
    |> Clr.Air.Server.get()

    raise "foobar"
  end

  @spec process_awaited(t) :: {:ok, t} | {:error, Exception.t()}

  def process_awaited(%{awaits: []} = analysis), do: {:ok, analysis}

  def process_awaited(%{awaits: [head | rest]} = analysis) do
    case await({:future, head}) do
      {:error, _} = error ->
        error

      {:ok, _result} ->
        process_awaited(%{analysis | awaits: rest})
    end
  end
end
