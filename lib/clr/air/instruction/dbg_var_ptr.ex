defmodule Clr.Air.Instruction.DbgVarPtr do
  defstruct [:line, :val]

  require Pegasus
  require Clr.Air

  Clr.Air.import(Clr.Air.Base, ~w[lineref cs dquoted lparen rparen]a)
  Clr.Air.import(Clr.Air.Literal, [:literal])

  Pegasus.parser_from_string(
    "dbg_var_ptr <- 'dbg_var_ptr' lparen (lineref / literal) cs dquoted rparen",
    dbg_var_ptr: [export: true, post_traverse: :dbg_var_ptr]
  )

  def dbg_var_ptr(rest, [value, line, "dbg_var_ptr"], context, _line, _bytes) do
    {rest, [%__MODULE__{val: value, line: line}], context}
  end
end
