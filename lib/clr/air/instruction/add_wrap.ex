defmodule Clr.Air.Instruction.AddWrap do
  defstruct [:lhs, :rhs]

  require Pegasus
  require Clr.Air

  Clr.Air.import(Clr.Air.Base, ~w[lineref cs lparen rparen]a)
  Clr.Air.import(Clr.Air.Literal, [:literal])
  Clr.Air.import(Clr.Air.Lvalue, [:lvalue])

  Pegasus.parser_from_string(
    "add_wrap <- 'add_wrap' lparen lineref cs (lineref / lvalue / literal) rparen",
    add_wrap: [export: true, post_traverse: :add_wrap]
  )

  def add_wrap(rest, [rhs, lhs, "add_wrap"], context, _line, _bytes) do
    {rest, [%__MODULE__{lhs: lhs, rhs: rhs}], context}
  end
end
