defmodule Clr.Air.Instruction.PtrElemVal do
  defstruct [:line, :val]

  require Pegasus
  require Clr.Air

  Clr.Air.import(Clr.Air.Base, ~w[lineref cs lparen rparen]a)
  Clr.Air.import(Clr.Air.Lvalue, [:lvalue])

  Pegasus.parser_from_string(
    "ptr_elem_val <- 'ptr_elem_val' lparen lineref cs (lineref / lvalue) rparen",
    ptr_elem_val: [export: true, post_traverse: :ptr_elem_val]
  )

  defp ptr_elem_val(rest, [val, line, "ptr_elem_val"], context, _line, _byte) do
    {rest, [%__MODULE__{line: line, val: val}], context}
  end
end
