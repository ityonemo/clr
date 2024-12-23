defmodule Clr.Air.Instruction.SetUnionTag do
  defstruct [:loc, :val]

  require Pegasus
  require Clr.Air

  Clr.Air.import(~w[lineref cs lparen rparen type literal]a)

  Pegasus.parser_from_string(
    "set_union_tag <- 'set_union_tag' lparen lineref cs literal rparen",
    set_union_tag: [export: true, post_traverse: :set_union_tag]
  )

  def set_union_tag(rest, [val, loc, "set_union_tag"], context, _line, _bytes) do
    {rest, [%__MODULE__{loc: loc, val: val}], context}
  end
end
