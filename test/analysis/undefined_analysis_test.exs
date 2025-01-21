defmodule ClrTest.Function.UndefinedAnalysisTest do
  use ExUnit.Case, async: true

  alias Clr.Analysis.Undefined
  alias Clr.Analysis.Undefined.Use
  alias Clr.Air.Function
  alias Clr.Block

  import ClrTest.TestAnalysis
  import Clr.Air.Lvalue

  setup do
    block =
      %Function{name: ~l"foo.bar"}
      |> Block.new([])
      |> Map.put(:loc, {47, 47})

    {:ok, config: %Undefined{}, block: block}
  end

  alias Clr.Air.Instruction.Mem.Store

  test "when you store undefined", %{config: config, block: block} do
    assert {:u, 8, %{undefined: %{function: "foo.bar", loc: {47, 47}}}} =
             block
             |> Block.put_type(47, {:u, 8, %{}})
             |> then(
               &Undefined.analyze(%Store{loc: {47, :keep}, src: ~l"undefined"}, 0, &1, config)
             )
             |> then(fn {:halt, {:void, new_block}} -> new_block end)
             |> Block.get_meta(47)
  end

  alias Clr.Air.Instruction.Mem.Load

  test "when you load undefined", %{config: config, block: block} do
    assert_raise Use, fn ->
      block
      |> Block.put_type(42, {:u, 8, %{undefined: %{function: "foo.bar", loc: {42, 42}}}})
      |> then(&Undefined.analyze(%Load{type: {:u, 8, %{}}, src: {42, :keep}}, 0, &1, config))
    end
  end
end
