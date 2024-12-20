defmodule Clr.Air.Instruction.BoolOr do
  defstruct [:lhs, :rhs]

  require Pegasus
  require Clr.Air

  Clr.Air.import(Clr.Air.Base, ~w[lineref cs lparen rparen]a)
  Clr.Air.import(Clr.Air.Lvalue, [:lvalue])
  Clr.Air.import(Clr.Air.Literal, [:literal])

  Pegasus.parser_from_string(
    "bool_or <- 'bool_or' lparen lineref cs (lineref / lvalue / literal) rparen",
    bool_or: [export: true, post_traverse: :bool_or]
  )

  def bool_or(rest, [rhs, lhs, "bool_or"], context, _line, _bytes) do
    {rest, [%__MODULE__{lhs: lhs, rhs: rhs}], context}
  end
end
