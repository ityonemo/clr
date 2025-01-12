defmodule ClrTest.Function.BlockAnalysisTest do
  use ExUnit.Case, async: true

  alias Clr.Air.Function
  alias Clr.Block
  import Clr.Air.Lvalue

  defp run_analysis(code, args \\ [], preload \\ %{}) do
    %Function{name: ~l"foo.bar"}
    |> Block.new(args)
    |> Map.replace!(:slots, preload)
    |> Block.analyze(code)
  end

  describe "generic instructions" do
    test "a keep instruction gets the instruction put into the types slot." do
      emptymap = %{}

      Mox.expect(ClrTest.InstructionHandler, :analyze, fn _, line, block ->
        Block.put_type(block, line, :foobar)
      end)

      assert %{slots: %{0 => {:foobar, ^emptymap}}} =
               run_analysis(%{{0, :keep} => %ClrTest.Instruction{}})
    end

    test "a clobber instruction does not change the state." do
      empty_map = %{}
      assert %{slots: ^empty_map} = run_analysis(%{{0, :clobber} => %ClrTest.Instruction{}})
    end

    test "a subsequent instruction gets the types passed" do
      empty_map = %{}

      ClrTest.InstructionHandler
      |> Mox.expect(:analyze, fn _, line, analysis ->
        Block.put_type(analysis, line, :foobar)
      end)
      |> Mox.expect(:analyze, fn _, line, %{slots: %{0 => {:foobar, ^empty_map}}} = analysis ->
        Block.put_type(analysis, line, :barbaz)
      end)

      assert %{slots: %{0 => {:foobar, ^empty_map}, 1 => {:barbaz, ^empty_map}}} =
               run_analysis(%{
                 {0, :keep} => %ClrTest.Instruction{},
                 {1, :keep} => %ClrTest.Instruction{}
               })
    end
  end

  test "alloc function" do
    meta = %{stack: ~l"foo.bar"}

    assert %{slots: %{0 => {{:ptr, :one, ~l"u32", []}, ^meta}}} =
             run_analysis(%{
               {0, :keep} => %Clr.Air.Instruction.Alloc{
                 type: {:ptr, :one, {:lvalue, ["u32"]}, []}
               }
             })
  end

  test "load function" do
    assert %{slots: %{0 => :u32}} =
             run_analysis(
               %{
                 {0, :keep} => %Clr.Air.Instruction.Load{type: :u32, loc: {47, :keep}}
               },
               [],
               %{47 => {{:ptr, :one, :u32, []}, []}}
             )
  end

  describe "maths functions" do
    test "overflow functions" do
      assert %{slots: %{0 => {:struct, [~l"u32", ~l"u1"]}}} =
               run_analysis(%{
                 {0, :keep} => %Clr.Air.Instruction.Maths.Overflow{
                   op: :add,
                   type: {:struct, [~l"u32", ~l"u1"]},
                   lhs: {5, :clobber},
                   rhs: {:literal, ~l"u32", 1}
                 }
               })
    end
  end

  test "struct_field_val function" do
    Mox.expect(ClrTest.InstructionHandler, :analyze, fn _, line, analysis ->
      Block.put_type(analysis, line, {:struct, [~l"u32", ~l"u1"]})
    end)

    assert %{slots: %{1 => ~l"u1"}} =
             run_analysis(%{
               {0, :keep} => %ClrTest.Instruction{},
               {1, :keep} => %Clr.Air.Instruction.StructFieldVal{src: {0, :keep}, index: 1}
             })
  end

  test "boolean comparison function" do
    assert %{slots: %{0 => ~l"bool"}} =
             run_analysis(%{
               {0, :keep} => %Clr.Air.Instruction.Tests.Compare{
                 lhs: {8, :clobber},
                 rhs: {:literal, ~l"u1", 0},
                 op: :eq
               }
             })
  end

  describe "ret_safe function" do
    test "returns an lvalue when it's an lvalue" do
      assert %{return: {:TypeOf, ~l"foo.bar.value"}} =
               run_analysis(%{
                 {0, :keep} => %Clr.Air.Instruction.RetSafe{val: ~l"foo.bar.value"}
               })
    end

    test "fails on pointer types which escape a stack pointer" do
      Mox.expect(ClrTest.InstructionHandler, :analyze, fn _, line, analysis ->
        Block.put_type(analysis, line, {:ptr, :one, ~l"u32", []}, %{stack: ~l"foo.bar"})
      end)

      assert_raise Clr.StackPtrEscape,
                   "Stack pointer escape detected in function `foo.bar` at 0:1",
                   fn ->
                     run_analysis(%{
                       {0, :clobber} => %Clr.Air.Instruction.DbgStmt{row: 0, col: 1},
                       {1, :keep} => %ClrTest.Instruction{},
                       {2, :clobber} => %Clr.Air.Instruction.RetSafe{val: {1, :clobber}}
                     })
                   end
    end
  end
end
