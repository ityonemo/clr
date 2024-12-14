defmodule Clr.Air.Instruction.SubWrap do
  defstruct [:lhs, :rhs]

  require Pegasus
  require Clr.Air

  Clr.Air.import(Clr.Air.Base, ~w[lineref name cs lparen rparen]a)

  Pegasus.parser_from_string(
    "sub_wrap <- 'sub_wrap' lparen lineref cs (lineref / name) rparen",
    sub_wrap: [export: true, post_traverse: :sub_wrap]
  )

  def sub_wrap(rest, [rhs, lhs, "sub_wrap"], context, _line, _bytes) do
    {rest, [%__MODULE__{lhs: lhs, rhs: rhs}], context}
  end
end
