defmodule Clr.Air.Instruction.StoreSafe do
  defstruct [:loc, :val]

  require Pegasus
  require Clr.Air

  Clr.Air.import(~w[argument type literal lvalue slotref cs lparen rparen]a)

  Pegasus.parser_from_string(
    "store_safe <- 'store_safe' lparen (slotref / literal) cs argument rparen",
    store_safe: [export: true, post_traverse: :store_safe]
  )

  def store_safe(rest, [value, loc, "store_safe"], context, _slot, _bytes) do
    {rest, [%__MODULE__{val: value, loc: loc}], context}
  end

  use Clr.Air.Instruction

  def analyze(%__MODULE__{loc: {slot, _}, val: {:literal, _type, :undefined}}, _slot, analysis) do
    %{analysis | slots: Map.update!(analysis.slots, slot, &set_undefined/1)}
  end

  def analyze(_, _, analysis), do: analysis

  defp set_undefined({:ptr, count, type, opts}) do
    {:ptr, count, type, Keyword.put(opts, :undefined, true)}
  end
end
