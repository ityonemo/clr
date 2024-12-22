defmodule Clr.Air.Instruction.AtomicRmw do
  defstruct [:loc, :val, :op, :mode]

  require Pegasus
  require Clr.Air

  Clr.Air.import(Clr.Air.Base, ~w[lineref name cs lparen rparen]a)
  Clr.Air.import(Clr.Air.Type, ~w[type]a)
  Clr.Air.import(Clr.Air.Lvalue, [:lvalue])
  Clr.Air.import(Clr.Air.Literal, [:literal])

  Pegasus.parser_from_string(
    """
    atomic_rmw <- 'atomic_rmw' lparen literal cs lvalue cs op cs mode rparen

    op <- add / sub
    add <- 'Add'
    sub <- 'Sub'

    mode <- seq_cst
    seq_cst <- 'seq_cst'
    """,
    atomic_rmw: [export: true, post_traverse: :atomic_rmw],
    add: [token: :add],
    sub: [token: :sub],
    seq_cst: [token: :seq_cst]
  )

  def atomic_rmw(rest, [mode, op, val, loc, "atomic_rmw"], context, _line, _bytes) do
    {rest, [%__MODULE__{mode: mode, op: op, loc: loc, val: val}], context}
  end
end
