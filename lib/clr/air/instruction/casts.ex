defmodule Clr.Air.Instruction.Casts do
  alias Clr.Air

  require Pegasus
  require Air

  Pegasus.parser_from_string(
    """
    casts <- bitcast / int_from_ptr / int_from_bool / intcast / trunc /
      optional_payload_ptr_set / optional_payload_ptr / optional_payload /
      wrap_optional / unwrap_errunion_payload_ptr / unwrap_errunion_payload / 
      unwrap_errunion_err_ptr / unwrap_errunion_err / errunion_payload_ptr_set /
      wrap_errunion_err / wrap_errunion_payload / int_from_float / float_from_int /
      addrspace_cast / fpext / fptrunc
    """,
    casts: [export: true]
  )

  Air.import(~w[argument lvalue type slotref cs lparen rparen literal]a)

  Air.ty_op(:bitcast, Bitcast)

  Air.un_op(:int_from_ptr, IntFromPtr)

  Air.un_op(:int_from_bool, IntFromBool)

  Air.ty_op(:intcast, Intcast)

  Air.ty_op(:trunc, Trunc)

  Air.ty_op(:optional_payload, OptionalPayload)

  Air.ty_op(:optional_payload_ptr, OptionalPayloadPtr)

  Air.ty_op(:optional_payload_ptr_set, OptionalPayloadPtrSet)

  Air.ty_op(:wrap_optional, WrapOptional)

  Air.ty_op(:unwrap_errunion_payload, UnwrapErrunionPayload)

  Air.ty_op(:unwrap_errunion_err, UnwrapErrunionErr)

  Air.ty_op(:unwrap_errunion_payload_ptr, UnwrapErrunionPayloadPtr)

  Air.ty_op(:unwrap_errunion_err_ptr, UnwrapErrunionErrPtr)

  Air.ty_op(:errunion_payload_ptr_set, ErrunionPayloadPtrSet)

  Air.ty_op(:wrap_errunion_payload, WrapErrunionPayload)

  Air.ty_op(:wrap_errunion_err, WrapErrunionErr)

  defmodule IntFromFloat do
    defstruct [:type, :src, optimized: false]
  end

  Pegasus.parser_from_string(
    """
    int_from_float <- int_from_float_str optimized? lparen type cs argument rparen
    int_from_float_str <- 'int_from_float'
    optimized <- '_optimized'
    """,
    int_from_float: [post_traverse: :int_from_float],
    int_from_float_str: [ignore: true],
    optimized: [token: :optimized]
  )

  def int_from_float(rest, [src, type | rest_args], context, _loc, _bytes) do
    optimized =
      case rest_args do
        [] -> false
        [:optimized] -> true
      end

    {rest, [%IntFromFloat{src: src, type: type, optimized: optimized}], context}
  end

  Air.ty_op(:float_from_int, FloatFromInt)

  Air.ty_op(:fpext, Fpext)

  Air.ty_op(:fptrunc, Fptrunc)

  Air.unimplemented(:addrspace_cast)
end
