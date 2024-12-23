defmodule Clr.Air.Instruction.Reduce do
  defstruct [:loc, :op]

  require Pegasus
  require Clr.Air

  Clr.Air.import(Clr.Air.Base, ~w[lineref name cs lparen rparen]a)
  Clr.Air.import(Clr.Air.Type, ~w[type]a)
  Clr.Air.import(Clr.Air.Lvalue, [:lvalue])
  Clr.Air.import(Clr.Air.Literal, [:literal])

  Pegasus.parser_from_string(
    """
    reduce <- 'reduce' lparen lineref cs op rparen

    op <- add / sub / or
    add <- 'Add'
    sub <- 'Sub'
    or <- 'Or'

    mode <- seq_cst
    seq_cst <- 'seq_cst'
    """,
    reduce: [export: true, post_traverse: :reduce],
    add: [token: :add],
    sub: [token: :sub],
    or: [token: :or],
    seq_cst: [token: :seq_cst]
  )

  def reduce(rest, [op, loc, "reduce"], context, _line, _bytes) do
    {rest, [%__MODULE__{op: op, loc: loc}], context}
  end
end
