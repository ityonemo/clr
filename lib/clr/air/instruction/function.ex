defmodule Clr.Air.Instruction.Function do
  require Pegasus
  require Clr.Air
  alias Clr.Block
  alias Clr.Type
  alias Clr.Function

  Clr.Air.import(~w[type int cs dquoted lparen rparen argument fn_literal lbrack rbrack slotref]a)

  Pegasus.parser_from_string(
    "function <- arg / call / ret_info / ret / frame_addr ",
    function: [export: true]
  )

  defmodule Arg do
    # The first N instructions in the main block must be one arg instruction per
    # function parameter. This makes function parameters participate in
    # liveness analysis without any special handling.
    # Uses the `arg` field.

    defstruct [:type, :name]

    use Clr.Air.Instruction

    def slot_type(_, slot, block) do
      {Enum.at(block.args, slot) || raise("unreachable"), block}
    end
  end

  Pegasus.parser_from_string(
    """
    arg <- arg_str lparen type (cs dquoted)? rparen
    arg_str <- 'arg'
    """,
    arg: [post_traverse: :arg],
    arg_str: [ignore: true]
  )

  def arg(rest, [type], context, _loc, _byte) do
    {rest, [%Arg{type: type}], context}
  end

  def arg(rest, [name, type], context, _loc, _byte) do
    {rest, [%Arg{type: type, name: name}], context}
  end

  defmodule Call do
    # Function call.
    # Result type is the return type of the function being called.
    # Uses the `pl_op` field with the `Call` payload. operand is the callee.
    # Triggers `resolveTypeLayout` on the return type of the callee.

    use Clr.Air.Instruction

    defstruct [:fn, :args, :opt]

    def slot_type(_, _, block), do: {:future, block}

    def analyze(call, slot, block, _config) do
      case call.fn do
        {:literal, {:fn, _args, ret_type, _meta}, {:function, function_name}} ->
          # we also need the context of the current function.

          {args_slots, block} =
            Enum.map_reduce(call.args, block, fn
              {:literal, arg_type, _}, block ->
                {{Type.from_air(arg_type), nil}, block}

              {slot, _}, block ->
                {arg_type, block} = Block.fetch_up!(block, slot)
                {{arg_type, slot}, block}
            end)

          {args, slots} = Enum.unzip(args_slots)

          block.function
          |> merge_name(function_name)
          |> Function.evaluate(slot, args, slots, Type.from_air(ret_type))
          |> case do
            {:future, ref} ->
              {:halt, Block.put_await(block, slot, ref)}

            {:ok, _result} ->
              {:halt, block}
          end
      end
    end

    def merge_name({:lvalue, lvalue}, function_name) do
      {:lvalue, List.replace_at(lvalue, -1, function_name)}
    end
  end

  Pegasus.parser_from_string(
    """
    call <- call_str (always_tail / never_tail / never_inline)? 
      lparen (fn_literal / slotref) cs lbrack (argument (cs argument)*)? rbrack rparen
    call_str <- 'call'
    always_tail <- '_always_tail'
    never_tail <- '_never_tail'
    never_inline <- '_never_inline'
    """,
    call: [post_traverse: :call],
    call_str: [ignore: true],
    always_tail: [token: :always_tail],
    never_tail: [token: :never_tail],
    never_inline: [token: :never_inline]
  )

  @call_opts ~w[always_tail never_tail never_inline]a

  def call(rest, args, context, _, _) do
    case Enum.reverse(args) do
      [opt, fun | args] when opt in @call_opts ->
        {rest, [%Call{fn: fun, args: args, opt: opt}], context}

      [fun | args] ->
        {rest, [%Call{fn: fun, args: args}], context}
    end
  end

  defmodule Ret do
    # Return a value from a function.
    # Result type is always noreturn; no instructions in a block follow this one.
    # Uses the `un_op` field.
    # Triggers `resolveTypeLayout` on the return type.
    #
    # if it's `mode: :load`
    #
    # This instruction communicates that the function's result value is pointed to by
    # the operand. If the function will pass the result by-ref, the operand is a
    # `ret_ptr` instruction. Otherwise, this instruction is equivalent to a `load`
    # on the operand, followed by a `ret` on the loaded value.
    # Result type is always noreturn; no instructions in a block follow this one.
    # Uses the `un_op` field.
    # Triggers `resolveTypeLayout` on the return type.

    defstruct [:src, :mode]

    use Clr.Air.Instruction

    def slot_type(%{src: {:lvalue, _} = lvalue}, _dst_slot, block) do
      {:noreturn, Block.put_return(block, {:TypeOf, lvalue})}
    end

    def slot_type(%{src: {:literal, type, _}}, _dst_slot, block) do
      {:noreturn, Block.put_return(block, Type.from_air(type))}
    end

    def slot_type(%{src: {slot, _}}, _dst_slot, block) when is_integer(slot) do
      block
      |> Block.fetch!(slot)
      |> then(&Block.put_return(block, &1))
      |> then(&{:noreturn, &1})
    end
  end

  Pegasus.parser_from_string(
    """
    ret <- ret_str (safe / load)? lparen argument rparen
    ret_str <- 'ret'
    safe <- '_safe'
    load <- '_load'
    """,
    ret: [post_traverse: :ret],
    ret_str: [ignore: true],
    safe: [token: :safe],
    load: [token: :load]
  )

  def ret(rest, [value | rest_args], context, _loc, _bytes) do
    mode =
      case rest_args do
        [:safe] -> :safe
        [:load] -> :load
        [] -> nil
      end

    {rest, [%Ret{src: value, mode: mode}], context}
  end

  Pegasus.parser_from_string(
    """
    ret_info <- ret_ptr / ret_addr
    ret_ptr <- ret_ptr_str lparen type rparen
    ret_ptr_str <- 'ret_ptr'
    """,
    ret_ptr: [post_traverse: :ret_ptr],
    ret_ptr_str: [ignore: true]
  )

  defmodule RetPtr do
    # If the function will pass the result by-ref, this instruction returns the
    # result pointer. Otherwise it is equivalent to `alloc`.
    # Uses the `ty` field.

    defstruct [:type]

    use Clr.Air.Instruction

    def slot_type(%{type: type}, _, block), do: {Type.from_air(type), block}

    def analyze(_, _, _, _) do
      raise "implement saving the return pointer slot into the block here."
    end
  end

  def ret_ptr(rest, [value], context, _loc, _bytes) do
    {rest, [%RetPtr{type: value}], context}
  end

  defmodule RetAddr do
    # Yields the return address of the current function.
    # Uses the `no_op` field.
    defstruct []

    use Clr.Air.Instruction

    def slot_type(_, _, block) do
      {{:ptr, :one, block.return, %{}}, block}
    end
  end

  Pegasus.parser_from_string(
    """
    ret_addr <- ret_addr_str
    ret_addr_str <- 'ret_addr()'
    """,
    ret_addr: [post_traverse: :ret_addr],
    ret_addr_str: [ignore: true]
  )

  def ret_addr(rest, [], context, _, _) do
    {rest, [%RetAddr{}], context}
  end

  defmodule FrameAddr do
    # Implements @frameAddress builtin.
    # Uses the `no_op` field.
    defstruct []

    use Clr.Air.Instruction

    def slot_type(_, _, block) do
      {{:usize, %{}}, block}
    end
  end

  Pegasus.parser_from_string(
    """
    frame_addr <- frame_addr_str
    frame_addr_str <- 'frame_addr()'
    """,
    frame_addr: [post_traverse: :frame_addr],
    frame_addr_str: [ignore: true]
  )

  def frame_addr(rest, [], context, _, _) do
    {rest, [%FrameAddr{}], context}
  end
end
