defmodule Clr.Air.Instruction.Controls do
  require Pegasus
  require Clr.Air

  Clr.Air.import(~w[argument cs slotref lparen rparen type lvalue literal clobbers space codeblock]a)

  Pegasus.parser_from_string(
    """
    controls <- return / block
    """,
    controls: [export: true]
  )

  # returns

  Pegasus.parser_from_string(
    """
    return <- ret_ptr
    ret_ptr <- 'ret_ptr' lparen type rparen
    """,
    ret_ptr: [export: true, post_traverse: :ret_ptr]
  )

  defmodule RetPtr do
    defstruct [:type]
  end

  def ret_ptr(rest, [value, "ret_ptr"], context, _slot, _bytes) do
    {rest, [%RetPtr{type: value}], context}
  end

  defmodule Block do
    defstruct [:type, :code, clobbers: []]
  end

  Pegasus.parser_from_string(
    """
    block <- block_str lparen type cs codeblock (space clobbers)? rparen
    block_str <- 'block'
    """,
    block: [post_traverse: :block],
    block_str: [ignore: true]
  )

  def block(rest, [codeblock, type], context, _slot, _bytes) do
    {rest, [%Block{type: type, code: codeblock}], context}
  end

  def block(rest, [{:clobbers, clobbers}, codeblock, type], context, _slot, _bytes) do
    {rest, [%Block{type: type, code: codeblock, clobbers: clobbers}], context}
  end
end
