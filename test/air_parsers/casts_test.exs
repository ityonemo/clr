defmodule ClrTest.AirParsers.CastsTest do
  use ExUnit.Case, async: true

  import Clr.Air.Lvalue

  alias Clr.Air.Instruction
  alias Clr.Air.Instruction.Casts.Bitcast

  alias ClrTest.TestAir

  test "bitcast" do
    assert %Bitcast{type: {:ptr, :many, ~l"elf.Elf64_auxv_t", []}, src: {65, :clobber}} =
             Instruction.parse("bitcast([*]elf.Elf64_auxv_t, %65!)")
  end

  alias Clr.Air.Instruction.Casts.IntFromPtr

  test "int_from_ptr" do
    assert %IntFromPtr{src: {:literal, ptrtyp, {:as, ptrtyp, {:ptrcast, ~l"__init_array_end"}}}} =
             Instruction.parse(
               "int_from_ptr(<[*]*const fn () callconv(.c) void, @as([*]*const fn () callconv(.c) void, @ptrCast(__init_array_end))>)"
             )
  end

  alias Clr.Air.Instruction.Casts.IntFromBool

  test "int_from_bool" do
    assert %IntFromBool{src: {218, :clobber}} = Instruction.parse("int_from_bool(%218!)")
  end

  alias Clr.Air.Instruction.Casts.Fptrunc

  test "fptrunc" do
    assert %Fptrunc{type: ~l"f16", src: {12, :clobber}} = Instruction.parse("fptrunc(f16, %12!)")
  end

  alias Clr.Air.Instruction.Casts.Fpext

  test "fpext" do
    assert %Fpext{type: ~l"f64", src: {9, :clobber}} = Instruction.parse("fpext(f64, %9!)")
  end

  alias Clr.Air.Instruction.Casts.Intcast

  test "intcast" do
    assert %Intcast{type: ~l"usize", src: {0, :keep}} =
             Instruction.parse("intcast(usize, %0)")
  end

  alias Clr.Air.Instruction.Casts.Trunc

  test "trunc" do
    assert %Trunc{type: ~l"usize", src: {0, :keep}} =
             Instruction.parse("trunc(usize, %0)")
  end

  alias Clr.Air.Instruction.Casts.OptionalPayload

  test "optional_payload" do
    assert %OptionalPayload{type: ~l"void", src: {19, :keep}} =
             Instruction.parse("optional_payload(void, %19)")
  end

  alias Clr.Air.Instruction.Casts.OptionalPayloadPtr

  test "optional_payload_ptr" do
    assert %OptionalPayloadPtr{
             type: {:ptr, :one, {:lvalue, ["debug", "SelfInfo"]}, []},
             src:
               {:literal, {:ptr, :one, {:optional, {:lvalue, ["debug", "SelfInfo"]}}, []},
                {:lvalue, ["debug", "self_debug_info"]}}
           } =
             Instruction.parse(
               "optional_payload_ptr(*debug.SelfInfo, <*?debug.SelfInfo, debug.self_debug_info>)"
             )
  end

  test "optional_payload_ptr_set" do
    assert %OptionalPayloadPtr{
             type: {:ptr, :one, {:lvalue, ["debug", "SelfInfo"]}, []},
             src:
               {:literal, {:ptr, :one, {:optional, {:lvalue, ["debug", "SelfInfo"]}}, []},
                {:lvalue, ["debug", "self_debug_info"]}}
           } =
             Instruction.parse(
               "optional_payload_ptr_set(*debug.SelfInfo, <*?debug.SelfInfo, debug.self_debug_info>)"
             )
  end

  alias Clr.Air.Instruction.Casts.WrapOptional

  test "wrap_optional" do
    assert %WrapOptional{type: {:optional, ~l"usize"}, src: {0, :keep}} =
             Instruction.parse("wrap_optional(?usize, %0)")
  end

  alias Clr.Air.Instruction.Casts.UnwrapErrunionPayload

  test "unwrap_errunion_payload" do
    assert %UnwrapErrunionPayload{type: ~l"usize", src: {0, :keep}} =
             Instruction.parse("unwrap_errunion_payload(usize, %0)")
  end

  alias Clr.Air.Instruction.Casts.UnwrapErrunionErr

  test "unwrap_errunion_err" do
    assert %UnwrapErrunionErr{type: {:errorset, ["Unexpected"]}, src: {0, :keep}} =
             Instruction.parse("unwrap_errunion_err(error{Unexpected}, %0)")
  end

  alias Clr.Air.Instruction.Casts.UnwrapErrunionPayloadPtr

  test "unwrap_errunion_payload_ptr" do
    assert %UnwrapErrunionPayloadPtr{
             type: {:ptr, :one, {:lvalue, ["u8"]}, [const: true]},
             src: {6, :clobber}
           } =
             Instruction.parse("unwrap_errunion_payload_ptr(*const u8, %6!)")
  end

  alias Clr.Air.Instruction.Casts.UnwrapErrunionErrPtr

  test "unwrap_errunion_err_ptr" do
    assert %UnwrapErrunionErrPtr{
             type: {:errorset, ~w[InvalidBuffer EndOfBuffer Overflow]},
             src: {259, :clobber}
           } =
             Instruction.parse(
               "unwrap_errunion_err_ptr(error{Overflow,EndOfBuffer,InvalidBuffer}, %259!)"
             )
  end

  alias Clr.Air.Instruction.Casts.ErrunionPayloadPtrSet

  test "errunion_payload_ptr_set" do
    assert %ErrunionPayloadPtrSet{
             type: {:ptr, :one, ~l"debug.Dwarf.EntryHeader", []},
             src: {0, :keep}
           } =
             Instruction.parse("errunion_payload_ptr_set(*debug.Dwarf.EntryHeader, %0)")
  end

  alias Clr.Air.Instruction.Casts.WrapErrunionPayload

  test "wrap_errunion_payload" do
    assert %WrapErrunionPayload{
             type: {:errorunion, ["Unexpected"], ~l"os.linux.rlimit"},
             src: {16, :clobber}
           } =
             Instruction.parse("wrap_errunion_payload(error{Unexpected}!os.linux.rlimit, %16!)")
  end

  alias Clr.Air.Instruction.Casts.WrapErrunionErr

  test "wrap_errunion_err" do
    assert %WrapErrunionErr{
             type: {:errorunion, ["Unexpected"], ~l"os.linux.rlimit"},
             src: {16, :clobber}
           } =
             Instruction.parse("wrap_errunion_err(error{Unexpected}!os.linux.rlimit, %16!)")
  end

  alias Clr.Air.Instruction.Casts.IntFromFloat

  test "int_from_float" do
    assert %IntFromFloat{type: ~l"u8", src: {9, :keep}} =
             Instruction.parse("int_from_float(u8, %9)")
  end

  alias Clr.Air.Instruction.Casts.FloatFromInt

  test "float_from_int" do
    assert %FloatFromInt{type: ~l"f32", src: {12, :keep}} =
             Instruction.parse("float_from_int(f32, %12)")
  end

  test "addrspace_cast" do
    TestAir.assert_unimplemented(:addrspace_cast)
  end
end
