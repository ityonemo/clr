defmodule Clr.Air.Instruction.Tests do
  alias Clr.Air

  require Pegasus
  require Air

  Pegasus.parser_from_string(
    """
    tests <- compare_instruction / is_instruction / cmp_lt_errors_len / error_set_has_value
    """,
    tests: [export: true]
  )

  Air.import(~w[argument type slotref literal lvalue cs lparen rparen]a)

  # compares

  defmodule Compare do
    use Clr.Air.Instruction

    defstruct ~w[lhs rhs op optimized]a

    # TODO: resolve the lhs and rhs types
    def slot_type(_, _, block) do
      {{:bool, %{}}, block}
    end
  end

  Pegasus.parser_from_string(
    """
    compare_instruction <- cmp_prefix compare_op lparen argument cs argument rparen

    cmp_prefix <- 'cmp_'

    compare_op <- (eq / gte / gt / lte / lt / neq) optimized?

    neq <- 'neq'
    lt <- 'lt'
    lte <- 'lte'
    eq <- 'eq'
    gt <- 'gt'
    gte <- 'gte'
    optimized <- '_optimized'
    """,
    compare_instruction: [post_traverse: :compare_instruction],
    cmp_prefix: [ignore: true],
    neq: [token: :neq],
    lt: [token: :lt],
    lte: [token: :lte],
    eq: [token: :eq],
    gt: [token: :gt],
    gte: [token: :gte],
    optimized: [token: :optimized]
  )

  def compare_instruction(rest, [rhs, lhs, op], context, _loc, _bytes) do
    {rest, [%Compare{lhs: lhs, rhs: rhs, op: op}], context}
  end

  def compare_instruction(rest, [rhs, lhs, :optimized, op], context, _loc, _bytes) do
    {rest, [%Compare{lhs: lhs, rhs: rhs, op: op, optimized: true}], context}
  end

  defmodule Is do
    defstruct ~w[operand op]a

    use Clr.Air.Instruction

    # TODO: resolve the operand types
    def slot_type(_, _, block) do
      {{:bool, %{}}, block}
    end
  end

  Pegasus.parser_from_string(
    """
    is_instruction <- is_prefix is_op lparen argument rparen

    is_prefix <- 'is_'

    is_op <- err_ptr / err / non_err_ptr / non_err / non_null_ptr / non_null / null_ptr / null / named_enum_value

    null <- 'null'
    non_null <- 'non_null'
    null_ptr <- 'null_ptr'
    non_null_ptr <- 'non_null_ptr'
    err <- 'err'
    non_err <- 'non_err'
    err_ptr <- 'err_ptr'
    non_err_ptr <- 'non_err_ptr'
    named_enum_value <- 'named_enum_value'
    """,
    is_instruction: [post_traverse: :is_instruction],
    is_prefix: [ignore: true],
    null: [token: :null],
    non_null: [token: :non_null],
    null_ptr: [token: :null_ptr],
    non_null_ptr: [token: :non_null_ptr],
    err: [token: :err],
    non_err: [token: :non_err],
    err_ptr: [token: :err_ptr],
    non_err_ptr: [token: :non_err_ptr],
    named_enum_value: [token: :named_enum_value]
  )

  def is_instruction(rest, [operand, op], context, _loc, _bytes) do
    {rest, [%Is{operand: operand, op: op}], context}
  end

  defmodule CmpLtErrorsLen do
    defstruct ~w[src]a

    use Clr.Air.Instruction

    def slot_type(_, _, block), do: {{:bool, %{}}, block}
  end

  Pegasus.parser_from_string(
    """
    cmp_lt_errors_len <- cmp_lt_errors_len_str lparen slotref rparen
    cmp_lt_errors_len_str <- 'cmp_lt_errors_len'
    """,
    cmp_lt_errors_len: [post_traverse: :cmp_lt_errors_len],
    cmp_lt_errors_len_str: [ignore: true]
  )

  def cmp_lt_errors_len(rest, [src], context, _loc, _bytes) do
    {rest, [%CmpLtErrorsLen{src: src}], context}
  end

  Air.ty_op(:error_set_has_value, ErrorSetHasValue)
end
