defmodule Clr.Air.Instruction.RetSafe do
  use Clr.Air.Instruction

  defstruct [:val]

  require Pegasus
  require Clr.Air

  Clr.Air.import(~w[argument slotref cs lparen rparen type lvalue literal]a)

  Pegasus.parser_from_string(
    "ret_safe <- 'ret_safe' lparen argument rparen",
    ret_safe: [export: true, post_traverse: :ret_safe]
  )

  def ret_safe(rest, [value, "ret_safe"], context, _slot, _bytes) do
    {rest, [%__MODULE__{val: value}], context}
  end

  def analyze(%{val: {:lvalue, _} = lvalue}, _dst_slot, analysis) do
    %{analysis | return: {:TypeOf, lvalue}}
  end

  def analyze(%{val: {:literal, type, _}}, _dst_slot, analysis) do
    %{analysis | return: type}
  end

  def analyze(%{val: {src_slot, _}}, _dst_slot, analysis) do
    # get the type of the value.
    case Map.fetch!(analysis.slots, src_slot) do
      {:ptr, _, _, opts} = return_type ->
        if opts[:stack] == analysis.name do
          raise Clr.StackPtrEscape,
            function: Clr.Air.Lvalue.as_string(analysis.name),
            row: analysis.row,
            col: analysis.col
        else
          %{analysis | return: return_type}
        end

      return_type ->
        %{analysis | return: return_type}
    end
  end
end
