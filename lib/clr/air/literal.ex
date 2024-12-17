defmodule Clr.Air.Literal do
  require Pegasus
  require Clr.Air

  Clr.Air.import(
    Clr.Air.Base,
    ~w[identifier int cs space dot lparen rparen langle rangle lbrack rbrack lbrace rbrace squoted dstring]a
  )

  Clr.Air.import(Clr.Air.Lvalue, [:lvalue])

  Clr.Air.import(Clr.Air.Type, ~w[type fn_type ptr_type]a)

  Pegasus.parser_from_string(
    """
    # literals are type + value
    literal <- int_literal / fn_literal / map_literal / other_literal

    int_literal <- langle type cs (int / sizeof / alignof) rangle
    fn_literal <- langle fn_type cs lvalue rangle
    map_literal <- langle type cs struct_value rangle
    other_literal <- langle type cs convertible rangle

    convertible <- as / string_value / struct_ptr / lvalue

    as <- '@as' lparen ptr_type cs value rparen
    value <- ptrcast / lvalue
    ptrcast <- '@ptrCast' lparen lvalue rparen

    string_value <- dstring (indices)?
    indices <- lbrack int '..' int rbrack

    # special builtin functions
    sizeof <- '@sizeOf' lparen type rparen
    alignof <- '@alignOf' lparen type rparen

    struct_ptr <- '&' struct_value range?

    struct_value <- dot lbrace (space struct_part (cs struct_part)* space)? rbrace
    struct_part <- struct_kv / struct_v
    struct_kv <- dot identifier space eq space struct_v
    struct_v <- struct_value / (dot identifier) / lvalue / int

    range <- lbrack int '..' int rbrack

    # private
    eq <- "="
    """,
    literal: [parser: true, export: true, post_traverse: :literal],
    string_value: [post_traverse: :string_value],
    alignof: [post_traverse: :alignof],
    sizeof: [post_traverse: :sizeof],
    as: [post_traverse: :as],
    ptrcast: [post_traverse: :ptrcast],
    struct_ptr: [post_traverse: :struct_ptr],
    struct_value: [post_traverse: :struct_value],
    struct_kv: [post_traverse: :struct_kv],
    range: [post_traverse: :range]
  )

  defp literal(rest, [value, type], context, _line, _bytes) do
    {rest, [{:literal, type, value}], context}
  end

  defp struct_ptr(rest, args, context, _line, _bytes) do
    case args do
      [range, value, "&"] -> {rest, [{:structptr, value, range}], context}
      [value, "&"] -> {rest, [{:structptr, value}], context}
    end
  end

  defp string_value(rest, [to, "..", from, string], context, _line, _bytes) do
    {rest, [{:string, string, from..to}], context}
  end

  defp sizeof(rest, [type, "@sizeOf"], context, _line, _bytes) do
    {rest, [{:sizeof, type}], context}
  end

  defp alignof(rest, [type, "@alignOf"], context, _line, _bytes) do
    {rest, [{:alignof, type}], context}
  end

  defp as(rest, [value, type, "@as"], context, _line, _bytes) do
    {rest, [{:as, type, value}], context}
  end

  defp ptrcast(rest, [name, "@ptrCast"], context, _line, _bytes) do
    {rest, [{:ptrcast, name}], context}
  end

  defp struct_value(rest, args, context, _line, _bytes) do
    {rest, [{:struct, Enum.reverse(args)}], context}
  end

  defp struct_kv(rest, [value, "=", key], context, _line, _bytes) do
    {rest, [{key, value}], context}
  end

  defp range(rest, [to, "..", from], context, _line, _bytes) do
    {rest, [from..to], context}
  end

  def parse(str) do
    case literal(str) do
      {:ok, [result], "", _context, _line, _bytes} -> result
    end
  end
end
