defmodule Clr.Air.Instruction.IntFromPtr do
  defstruct [:val]

  require Pegasus
  require Clr.Air

  Clr.Air.import(Clr.Air.Base, ~w[lineref cs dquoted lparen rparen]a)
  Clr.Air.import(Clr.Air.Literal, [:literal])

  Pegasus.parser_from_string(
    "int_from_ptr <- 'int_from_ptr' lparen (literal / lineref) rparen",
    int_from_ptr: [export: true, post_traverse: :int_from_ptr]
  )

  def int_from_ptr(rest, [value, "int_from_ptr"], context, _line, _bytes) do
    {rest, [%__MODULE__{val: value}], context}
  end
end
