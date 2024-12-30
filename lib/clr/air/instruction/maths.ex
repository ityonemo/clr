defmodule Clr.Air.Instruction.Maths do
  require Pegasus
  require Clr.Air

  Pegasus.parser_from_string(
    """
    maths <- binary_instruction / unary_type_instruction / overflow_instruction
    """,
    maths: [export: true]
  )

  Clr.Air.import(~w[argument type lineref literal lvalue cs lparen rparen]a)

  # binary instructions

  defmodule Binary do
    defstruct ~w[lhs rhs op mode]a
  end

  Pegasus.parser_from_string(
    """
    binary_instruction <- binary_op lparen argument cs argument rparen

    binary_op <- add_op / sub_op / mul_op / div_op / rem / mod /
                 min / max / shl / shr /
                 bit_and / bit_or / xor /
                 bool_or

    add_op <- add (sat / wrap)?
    add <- 'add'
    sub_op <- sub (sat / wrap)?
    sub <- 'sub'
    mul_op <- mul (sat / wrap)?
    mul <- 'mul'
    div_op <- div (exact / trunc)
    div <- 'div'
    rem <- 'rem'
    mod <- 'mod'
    min <- 'min'
    max <- 'max'
    shl <- 'shl'
    shr <- 'shr'
    bit_and <- 'bit_and'
    bit_or <- 'bit_or'
    xor <- 'xor'
    bool_or <- 'bool_or'

    mode <- sat / wrap
    sat <- '_sat'
    wrap <- '_wrap'
    exact <- '_exact'
    trunc <- '_trunc'
    """,
    binary_instruction: [post_traverse: :binary_instruction],
    add: [token: :add],
    sub: [token: :sub],
    sub_sat: [token: :sub_sat],
    sub_wrap: [token: :sub_wrap],
    mul: [token: :mul],
    mul_sat: [token: :mul_sat],
    mul_wrap: [token: :mul_wrap],
    div: [token: :div],
    rem: [token: :rem],
    mod: [token: :mod],
    min: [token: :min],
    max: [token: :max],
    shl: [token: :shl],
    shr: [token: :shr],
    bit_and: [token: :bit_and],
    bit_or: [token: :bit_or],
    xor: [token: :xor],
    bool_or: [token: :bool_or],
    sat: [token: :sat],
    wrap: [token: :wrap],
    exact: [token: :exact],
    trunc: [token: :trunc]
  )

  @modes ~w[sat wrap exact trunc]a
  def binary_instruction(rest, [rhs, lhs, mode, op], context, _line, _bytes)
      when mode in @modes do
    {rest, [%Binary{lhs: lhs, rhs: rhs, op: op, mode: mode}], context}
  end

  def binary_instruction(rest, [rhs, lhs, op], context, _line, _bytes) do
    {rest, [%Binary{lhs: lhs, rhs: rhs, op: op}], context}
  end

  # Unary + Type operations

  defmodule UnaryTyped do
    defstruct ~w[operand op type]a
  end

  Pegasus.parser_from_string(
    """
    unary_type_instruction <- unary_type_op lparen type cs argument rparen

    unary_type_op <- not / abs / clz / byte_swap / bit_reverse

    abs <- 'abs'
    not <- 'not'
    clz <- 'clz'
    byte_swap <- 'byte_swap'
    bit_reverse <- 'bit_reverse'
    """,
    unary_type_instruction: [post_traverse: :unary_type_instruction],
    not: [token: :not],
    abs: [token: :abs],
    clz: [token: :clz],
    byte_swap: [token: :byte_swap],
    bit_reverse: [token: :bit_reverse]
  )

  def unary_type_instruction(rest, [operand, type, op], context, _line, _bytes) do
    {rest, [%UnaryTyped{operand: operand, type: type, op: op}], context}
  end

  # Overflow operations

  defmodule Overflow do
    use Clr.Air.Instruction
    alias Clr.Analysis
    defstruct ~w[op type lhs rhs]a
    def analyze(%{type: type}, line, analysis), do: Analysis.put_type(analysis, line, type)
  end

  Pegasus.parser_from_string(
    """
    overflow_instruction <- overflow_op with_overflow lparen type cs argument cs argument rparen
    with_overflow <- '_with_overflow'
    overflow_op <- add / sub / mul / shl
    """,
    with_overflow: [ignore: true],
    overflow_instruction: [post_traverse: :overflow_instruction]
  )

  def overflow_instruction(rest, [rhs, lhs, type, op], context, _line, _bytes) do
    {rest, [%Overflow{lhs: lhs, rhs: rhs, type: type, op: op}], context}
  end
end
