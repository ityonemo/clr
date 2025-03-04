defmodule Clr.Air.Base do
  # basic parser and parser combinators for AIR parsing

  require Pegasus

  Pegasus.parser_from_string(
    """
    # slot numbers
    slotref <- clobber / keep
    clobbers <- clobber (space clobber)*
    keep <- percent int space?
    clobber <- percent int bang

    # strings and names
    squoted <- singleq identifier singleq
    dquoted <- doubleq identifier doubleq
    dstring <- doubleq [^"]* doubleq

    identifier <- ('@'? alpha alnum*) / ('@' dstring)
    alpha <- [a-zA-Z_]
    alnum <- [a-zA-Z0-9_]

    float <- '-'? [0-9]+ '.' [0-9]+
    int <- '-'? [0-9]+

    # this is convenient because it occurs all over the place
    cs <- comma space

    # basic tokens
    singleq <- [']
    doubleq <- ["]
    comma <- ','
    space <- '\s'
    colon <- ':'
    dot <- '.'
    lparen <- '('
    rparen <- ')'
    langle <- '<'
    rangle <- '>'
    lbrace <- '{'
    rbrace <- '}'
    lbrack <- '['
    rbrack <- ']'
    fatarrow <- '=>'
    newline <- '\s'* '\n'
    equals <- "="
    null <- 'null'
    undefined <- 'undefined'
    elision <- "..."

    # debug
    notnewline <- [^\n]*

    # private non-exported tokens.
    percent <- '%'
    bang <- '!'
    """,
    slotref: [export: true],
    clobbers: [export: true, post_traverse: :clobbers],
    keep: [post_traverse: :keep],
    clobber: [post_traverse: :clobber],
    squoted: [export: true],
    dquoted: [export: true],
    dstring: [export: true, collect: true],
    identifier: [export: true, collect: true, post_traverse: :identifier],
    float: [export: true, collect: true, post_traverse: :float],
    int: [export: true, collect: true, post_traverse: :int],
    cs: [export: true],
    singleq: [ignore: true, export: true],
    doubleq: [ignore: true, export: true],
    comma: [ignore: true, export: true],
    space: [ignore: true, export: true],
    colon: [ignore: true, export: true],
    dot: [ignore: true, export: true],
    lparen: [ignore: true, export: true],
    rparen: [ignore: true, export: true],
    langle: [ignore: true, export: true],
    rangle: [ignore: true, export: true],
    lbrace: [ignore: true, export: true],
    rbrace: [ignore: true, export: true],
    lbrack: [ignore: true, export: true],
    rbrack: [ignore: true, export: true],
    fatarrow: [ignore: true, export: true],
    newline: [ignore: true, export: true],
    notnewline: [export: true, collect: true],
    percent: [ignore: true],
    bang: [ignore: true],
    equals: [export: true, ignore: true],
    null: [export: true, token: :null],
    undefined: [export: true, token: :undefined],
    elision: [export: true, token: :...]
  )

  defp float(rest, [value], context, _loc, _bytes), do: {rest, [String.to_float(value)], context}

  defp int(rest, [value], context, _loc, _bytes), do: {rest, [String.to_integer(value)], context}

  defp keep(rest, [slot], context, _loc, _bytes), do: {rest, [{slot, :keep}], context}

  defp identifier(rest, args, context, _loc, _bytes) do
    case args do
      [identifier] -> {rest, [identifier], context}
      [identifier, "@"] -> {rest, [identifier], context}
    end
  end

  defp clobber(rest, [slot], context, _loc, _bytes), do: {rest, [{slot, :clobber}], context}

  defp clobbers(rest, clobbers, context, _loc, _bytes) do
    clobbers =
      clobbers
      |> Enum.map(fn {slot, :clobber} -> slot end)
      |> Enum.sort()

    {rest, [{:clobbers, clobbers}], context}
  end
end
