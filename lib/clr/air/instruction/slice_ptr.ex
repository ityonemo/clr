defmodule Clr.Air.Instruction.SlicePtr do
  defstruct [:type, :src]

  require Pegasus
  require Clr.Air

  Clr.Air.import(Clr.Air.Base, ~w[lineref cs lparen rparen langle rangle]a)

  Clr.Air.import(Clr.Air.Type, ~w[type]a)

  Pegasus.parser_from_string(
    "slice_ptr <- 'slice_ptr' lparen type cs lineref rparen",
    slice_ptr: [export: true, post_traverse: :slice_ptr]
  )

  defp slice_ptr(rest, [src, type, "slice_ptr"], context, _line, _bytes) do
    {rest, [%__MODULE__{type: type, src: src}], context}
  end
end
