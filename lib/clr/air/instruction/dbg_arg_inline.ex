defmodule Clr.Air.Instruction.DbgArgInline do
  defstruct [:val, :key]

  require Pegasus
  require Clr.Air

  Clr.Air.import(Clr.Air.Base, ~w[lineref cs dquoted lparen rparen]a)
  Clr.Air.import(Clr.Air.Lvalue, [:lvalue])
  Clr.Air.import(Clr.Air.Literal, [:literal])

  Pegasus.parser_from_string(
    "dbg_arg_inline <- 'dbg_arg_inline' lparen (literal / lvalue / lineref) cs dquoted rparen",
    dbg_arg_inline: [export: true, post_traverse: :dbg_arg_inline]
  )

  def dbg_arg_inline(rest, [key, value, "dbg_arg_inline"], context, _line, _bytes) do
    {rest, [%__MODULE__{val: value, key: key}], context}
  end
end
