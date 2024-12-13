defmodule Clr.Air.Instruction.Add do
  defstruct [:lhs, :rhs]

  def initialize([lhs, rhs]), do: %__MODULE__{lhs: lhs, rhs: rhs}
end
