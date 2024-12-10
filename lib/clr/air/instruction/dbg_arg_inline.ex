defmodule Clr.Air.Instruction.DbgArgInline do
  @behaviour Clr.Air.Instruction

  defstruct [:pair, :name, :unused]

  def initialize([type, value, name]) do
    %__MODULE__{pair: {type, value}, name: name}
  end
end
