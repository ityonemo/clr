defmodule ClrTest.Air.TypeTest do
  use ExUnit.Case, async: true

  alias Clr.Air.Type

  import Clr.Air.Lvalue

  test "basic type" do
    assert ~l"i32" = Type.parse("i32")
  end

  describe "basic pointer types" do
    test "single pointer" do
      assert {:ptr, :one, ~l"i32", []} = Type.parse("*i32")
    end

    test "many pointer" do
      assert {:ptr, :many, ~l"i32", []} = Type.parse("[*]i32")
    end

    test "slice pointer" do
      assert {:ptr, :slice, ~l"i32", []} = Type.parse("[]i32")
    end
  end

  describe "special pointer augmentations" do
    test "allowzero" do
      assert {:ptr, :one, ~l"i32", allowzero: true} = Type.parse("*allowzero i32")
    end

    test "volatile" do
      assert {:ptr, :one, ~l"i32", volatile: true} = Type.parse("*volatile i32")
    end
  end

  describe "sentinel pointer types" do
    test "manypointer" do
      assert {:ptr, :many, ~l"i32", sentinel: 0} = Type.parse("[*:0]i32")
    end

    test "slice pointer" do
      assert {:ptr, :slice, ~l"i32", sentinel: 0} = Type.parse("[:0]i32")
    end
  end

  describe "const pointer types" do
    test "single pointer" do
      assert {:ptr, :one, ~l"i32", const: true} = Type.parse("*const i32")
    end

    test "many pointer" do
      assert {:ptr, :many, ~l"i32", const: true} = Type.parse("[*]const i32")
    end

    test "slice pointer" do
      assert {:ptr, :slice, ~l"i32", const: true} = Type.parse("[]const i32")
    end
  end

  describe "optional pointer types" do
    test "single pointer" do
      assert {:ptr, :one, ~l"i32", optional: true} = Type.parse("?*i32")
    end

    test "const pointer" do
      assert {:ptr, :one, ~l"i32", opts} = Type.parse("?*const i32")
      assert opts[:optional]
      assert opts[:const]
    end

    test "pointer to a function" do
      assert {:ptr, :many, {:fn, [], ~l"void", [callconv: :c]}, []} =
               Type.parse("[*]*const fn () callconv(.c) void")
    end
  end

  test "optional generic type" do
    assert {:optional, ~l"i32"} = Type.parse("?i32")
  end

  test "function pointer type with callconv" do
    assert {:ptr, :many, {:fn, [], ~l"void", [callconv: :c]}, []} =
             Type.parse("[*]*const fn () callconv(.c) void")
  end

  test "function type with callconv" do
    assert {:fn, [~l"Target.Cpu.Arch"], ~l"bool", [callconv: :inline]} =
             Type.parse("fn (Target.Cpu.Arch) callconv(.@\"inline\") bool")
  end

  test "function type with noalias" do
    assert {:fn, [{:noalias, {:ptr, :one, ~l"usize", []}}, {:noalias, {:ptr, :one, ~l"u8", []}}],
            ~l"u8",
            []} =
             Type.parse("fn (noalias *usize, noalias *u8) u8")
  end

  test "const * function type" do
    assert {:fn, [], ~l"noreturn", [callconv: :naked]} =
             Type.parse("*const fn () callconv(.naked) noreturn")
  end

  test "function type with elision" do
    assert {:fn,
            [
              ~l"c_int",
              {:ptr, :many, ~l"u8", const: true, sentinel: 0},
              ~l"os.linux.O__struct8594",
              :...
            ], ~l"c_int",
            callconv: :c} =
             Type.parse(
               "fn (c_int, [*:0]const u8, os.linux.O__struct8594, ...) callconv(.c) c_int"
             )
  end

  test "array type" do
    assert {:array, 8, ~l"i32", []} = Type.parse("[8]i32")
  end

  test "array type with sentinel" do
    assert {:array, 8, ~l"i32", sentinel: 0} = Type.parse("[8:0]i32")
  end

  test "aligned slice type" do
    assert {:ptr, :slice, ~l"u8", alignment: 4096} = Type.parse("[]align(4096) u8")
  end

  test "enum literal type" do
    assert :enum_literal = Type.parse("@Type(.enum_literal)")
  end

  test "comptime" do
    assert {:comptime, :enum_literal} = Type.parse("comptime @Type(.enum_literal)")
  end

  test "anonymous struct type" do
    assert {:struct, [~l"u32", ~l"u1"]} = Type.parse("struct { u32, u1 }")
  end

  test "error type" do
    assert {:errorunion, ["Unexpected"], ~l"os.linux.rlimit"} =
             Type.parse("error{Unexpected}!os.linux.rlimit")
  end

  test "anyerror type" do
    assert {:errorunion, :any, ~l"os.linux.rlimit"} =
             Type.parse("anyerror!os.linux.rlimit")
  end

  test "error union type" do
    assert {:errorset, ["Invalid", "Unexpected"]} =
             Type.parse("error{Unexpected,Invalid}")
  end

  test "lvalue error union type" do
    assert {:errorunion, ~l"foo.bar", ~l"baz"} =
             Type.parse("foo.bar!baz")
  end

  test "atomic value allowed as function call" do
    assert {:lvalue, [{:comptime_call, ~l"atomic.Value", [~l"u8"]}]} =
             Type.parse("atomic.Value(u8)")
  end

  test "generic comptime function call type" do
    assert {:lvalue,
            [
              {:comptime_call, ~l"io.GenericWriter",
               [~l"fs.File", {:errorset, _}, {:function, "write"}]}
            ]} =
             Type.parse(
               "io.GenericWriter(fs.File,error{Unexpected,DiskQuota,FileTooBig,InputOutput,NoSpaceLeft,DeviceBusy,InvalidArgument,AccessDenied,BrokenPipe,SystemResources,OperationAborted,NotOpenForWriting,LockViolation,WouldBlock,ConnectionResetByPeer,ProcessNotFound},(function 'write'))"
             )
  end

  test "typeinfo call (call with dereference)" do
    assert {:lvalue, [{:comptime_call, ~l"@typeInfo", [~l"foo"]}, "child"]} =
             Type.parse("@typeInfo(foo).child")
  end

  test "complex generic type" do
    assert {:lvalue,
            [
              {:comptime_call, ~l"@typeInfo",
               [{:lvalue, [{:comptime_call, ~l"@TypeOf", [~l"fmt.format__anon_3497"]}]}]},
              "return_type",
              :unwrap_optional
            ]} = Type.parse("@typeInfo(@TypeOf(fmt.format__anon_3497)).return_type.?")
  end

  test "nested pointer type" do
    assert {:ptr, :many, {:ptr, :many, {:lvalue, ["u8"]}, [sentinel: 0]}, []} =
             Type.parse("[*][*:0]u8")
  end

  test "sub_pointer type" do
    assert {:ptr, :one, {:lvalue, ["u8"]}, [alignment: {4, 0, 4}]} =
             Type.parse("*align(4:0:4) u8")
  end

  test "sub_pointer type with four alignment" do
    assert {:ptr, :one, {:lvalue, ["u8"]}, [alignment: {4, 0, 4, 0}]} =
             Type.parse("*align(4:0:4:0) u8")
  end

  test "vector type" do
    # note.  the vector type is special in that the definition has a comma in the comptime call.
    assert {:ptr, :one, {:lvalue, [{:vector, ~l"u8", 16}]}, []} = Type.parse("*@Vector(16, u8)")
  end
end
