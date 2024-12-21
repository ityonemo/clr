defmodule Clr.Air.Instruction.CmpNeq do
  defstruct [:lhs, :rhs]

  require Pegasus
  require Clr.Air

  Clr.Air.import(Clr.Air.Base, ~w[lineref cs lparen rparen]a)
  Clr.Air.import(Clr.Air.Literal, [:literal])
  Clr.Air.import(Clr.Air.Lvalue, [:lvalue])

  Pegasus.parser_from_string(
    "cmp_neq <- 'cmp_neq' lparen (literal / lineref) cs (literal / lvalue / lineref) rparen",
    cmp_neq: [export: true, post_traverse: :cmp_neq]
  )

  def cmp_neq(rest, [rhs, lhs, "cmp_neq"], context, _line, _bytes) do
    {rest, [%__MODULE__{lhs: lhs, rhs: rhs}], context}
  end
end
