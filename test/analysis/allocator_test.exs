defmodule ClrTest.Analysis.AllocatorTest do
  use ExUnit.Case, async: true

  alias Clr.Analysis.Allocator
  alias Clr.Analysis.Allocator.UseAfterFree
  alias Clr.Analysis.Allocator.DoubleFree
  alias Clr.Air.Function
  alias Clr.Air.Instruction.Function.Call
  alias Clr.Block

  import Clr.Air.Lvalue

  setup do
    block =
      %Function{name: ~l"foo.bar"}
      |> Block.new([])
      |> Map.put(:loc, {47, 47})

    {:ok, config: %Allocator{}, block: block}
  end

  @allocator {:fn, [~l"mem.Allocator"],
              {:errorable, ["OutOfMemory"], {:ptr, :one, {:lvalue, ["u8"]}, []}}, []}
  @create_literal {:literal, @allocator, {:function, "create__anon_2535"}}
  @c_allocator_literal {:literal, ~l"mem.Allocator",
                        %{
                          "ptr" => :undefined,
                          "vtable" => {:lvalue, ["heap", "c_allocator_vtable"]}
                        }}

  describe "when you do an allocation" do
    test "it marks with correct metadata", %{config: config, block: block} do
      assert {:halt, {type, _block}} =
               Allocator.analyze(
                 %Call{fn: @create_literal, args: [@c_allocator_literal]},
                 0,
                 {%{}, block},
                 config
               )

      assert {:errorable, _,
              {:ptr, :one, {:u, 8, %{undefined: %{function: ~l"foo.bar", loc: {47, 47}}}},
               %{
                 heap: %{
                   function: ~l"foo.bar",
                   loc: {47, 47},
                   vtable: ~l"heap.c_allocator_vtable"
                 }
               }}, %{}} = type
    end
  end

  describe "when you run a deallocation" do
  end
end
