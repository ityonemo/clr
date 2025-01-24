defmodule ClrTest.Analysis.Instruction.CastsTest do
  use ExUnit.Case, async: true

  alias Clr.Air.Instruction
  alias Clr.Air.Function
  alias Clr.Block

  import Clr.Air.Lvalue

  setup do
    block =
      %Function{name: ~l"foo.bar"}
      |> Block.new([])
      |> Map.put(:loc, {47, 47})

    {:ok, block: block, config: %Instruction{}}
  end

  test "bitcast"

  test "int_from_ptr"

  test "int_from_bool"

  test "intcast"

  test "trunc"

  test "optional_payload"

  test "optional_payload_ptr"

  test "optional_payload_ptr_set"

  test "wrap_optional"

  test "unwrap_errunion_payload"

  test "unwrap_errunion_err"

  test "unwrap_errunion_payload_ptr"

  test "unwrap_errunion_err_ptr"

  test "errunion_payload_ptr_set"

  describe "wrap_errunion_payload" do
    alias Clr.Air.Instruction.Casts.WrapErrunionPayload
    test "makes default type when it's not a slotref", %{block: block} do
      assert {{:errorable, [~l"OutOfMemory"], {:u, 8, %{}}, %{}}, _} =
        Instruction.slot_type(
          %WrapErrunionPayload{type: {:errorable, [~l"OutOfMemory"], ~l"u8"}, src: ~l"some.constant"},
          block
        )
    end

    test "correctly transfers child metadata", %{block: block} do
      block = Block.put_type(block, 0, {:u, 8, %{}}, foo: :bar)

      assert {{:errorable, [~l"OutOfMemory"], {:u, 8, %{foo: :bar}}, %{}}, _} =
               Instruction.slot_type(
                 %WrapErrunionPayload{type: {:errorable, [~l"OutOfMemory"], ~l"u8"}, src: {0, :keep}},
                 block
               )
    end
  end

  alias Clr.Air.Instruction.Casts.WrapErrunionErr

  test "wrap_errunion_err", %{block: block} do
    assert {{:errorable, [~l"OutOfMemory"], :void, %{}}, _} =
             Instruction.slot_type(
               %WrapErrunionErr{type: {:errorable, [~l"OutOfMemory"], :void}, src: {0, :keep}},
               block
             )
  end

  describe "int_from_float" do
    alias Clr.Air.Instruction.Casts.IntFromFloat

    test "makes default type when it's not a slotref", %{block: block} do
      assert {{:i, 32, %{}}, _} =
               Instruction.slot_type(%IntFromFloat{type: ~l"i32", src: ~l"some.constant"}, block)
    end

    test "transfers type metadata", %{block: block} do
      block = Block.put_type(block, 0, {:f, 32, %{foo: :bar}})

      assert {{:i, 32, %{foo: :bar}}, _} =
               Instruction.slot_type(%IntFromFloat{type: ~l"i32", src: {0, :keep}}, block)
    end
  end

  describe "float_from_int" do
    alias Clr.Air.Instruction.Casts.FloatFromInt

    test "correctly makes the type when it's not a slotref", %{block: block} do
      assert {{:f, 32, %{}}, _} =
               Instruction.slot_type(%FloatFromInt{type: ~l"f32", src: ~l"some.constant"}, block)
    end

    test "correctly transfers type metadata", %{block: block} do
      block = Block.put_type(block, 0, {:i, 32, %{foo: :bar}})

      assert {{:f, 32, %{foo: :bar}}, _} =
               Instruction.slot_type(%FloatFromInt{type: ~l"f32", src: {0, :keep}}, block)
    end
  end

  describe "fpext" do
    alias Clr.Air.Instruction.Casts.Fpext

    test "correctly makes the type when it's not a slotref", %{block: block} do
      assert {{:f, 64, %{}}, _} =
               Instruction.slot_type(%Fpext{type: ~l"f64", src: ~l"some.constant"}, block)
    end

    test "correctly transfers type metadata", %{block: block} do
      block = Block.put_type(block, 0, {:f, 32, %{foo: :bar}})

      assert {{:f, 64, %{foo: :bar}}, _} =
               Instruction.slot_type(%Fpext{type: ~l"f64", src: {0, :keep}}, block)
    end
  end

  describe "fptrunc" do
    alias Clr.Air.Instruction.Casts.Fptrunc

    test "correctly makes the type when it's not a slotref", %{block: block} do
      assert {{:f, 16, %{}}, _} =
               Instruction.slot_type(%Fptrunc{type: ~l"f16", src: ~l"some.constant"}, block)
    end

    test "correctly transfers type metadata", %{block: block} do
      block = Block.put_type(block, 0, {:f, 16, %{foo: :bar}})

      assert {{:f, 16, %{foo: :bar}}, _} =
               Instruction.slot_type(%Fptrunc{type: ~l"f16", src: {0, :keep}}, block)
    end
  end
end
