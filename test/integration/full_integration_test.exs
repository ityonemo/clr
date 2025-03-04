defmodule ClrTest.FullIntegrationTest do
  use ExUnit.Case, async: true

  @moduletag :integration

  alias Clr.Zig.Parser

  @file_colon Path.relative_to_cwd(__ENV__.file) <> ":"

  def assert_errors_with(msg, file) do
    file
    |> then(&Path.join(__DIR__, &1))
    |> Parser.load_parse()

    if match?([_, @file_colon <> _], System.argv()) do
      Application.put_env(:clr, :debug_prefix, Path.basename(file, ".zig"))
    end

    assert_raise Mix.Error, msg, fn ->
      file
      |> then(&["run", Path.join("test/integration", &1)])
      |> Mix.Tasks.Clr.run()
    end
  end

  describe "undefined" do
    test "value used" do
      assert_errors_with(
        """
        Use of undefined value in function undefined_value_use.main (test/integration/undefined/undefined_value_use.zig:3:5).
        Value was set undefined in function undefined_value_use.main (test/integration/undefined/undefined_value_use.zig:2:5)
        """,
        "undefined/undefined_value_use.zig"
      )
    end

    test "value passed and used" do
      assert_errors_with(
        """
        Use of undefined value in function undefined_value_passed.deref_ptr (test/integration/undefined/undefined_value_passed.zig:2:3).
        Value was set undefined in function undefined_value_passed.main (test/integration/undefined/undefined_value_passed.zig:6:5)
        """,
        "undefined/undefined_value_passed.zig"
      )
    end
  end

  describe "stack_pointer" do
    test "escaping parameter pointer" do
      assert_errors_with(
        """
        Escape of stack pointer in function param_ptr_escape.escaped_param_ptr (test/integration/stack_pointer/param_ptr_escape.zig:2:3).
        Value was created in function param_ptr_escape.escaped_param_ptr (test/integration/stack_pointer/param_ptr_escape.zig:1, argument 0)
        """,
        "stack_pointer/param_ptr_escape.zig"
      )
    end

    test "escaping stack variable pointer" do
      assert_errors_with(
        """
        Escape of stack pointer in function stack_ptr_escape.escaped_ptr (test/integration/stack_pointer/stack_ptr_escape.zig:4:3).
        Value was created in function stack_ptr_escape.escaped_ptr (test/integration/stack_pointer/stack_ptr_escape.zig:2:3)
        """,
        "stack_pointer/stack_ptr_escape.zig"
      )
    end
  end

  describe "allocator" do
    test "use after free" do
      assert_errors_with(
        """
        Use after free detected in function use_after_free.main (test/integration/allocator/use_after_free.zig:7:5).
        Pointer was freed in function use_after_free.main (test/integration/allocator/use_after_free.zig:6:18)
        """,
        "allocator/use_after_free.zig"
      )
    end

    test "stack free" do
      assert_errors_with(
        """
        Stack memory attempted to be freed by allocator `heap.c_allocator_vtable in function stack_free.main (test/integration/allocator/stack_free.zig:5:18).
        Pointer was created in function stack_free.main (test/integration/allocator/stack_free.zig:4:5)
        """,
        "allocator/stack_free.zig"
      )
    end

    test "double free" do
      assert_errors_with(
        """
        Double free detected in function double_free.main (test/integration/allocator/double_free.zig:6:18).
        Previously freed in function double_free.main (test/integration/allocator/double_free.zig:5:18)
        """,
        "allocator/double_free.zig"
      )
    end

    test "mismatched allocator" do
      assert_errors_with(
        """
        Heap memory attempted to be freed by `heap.c_allocator_vtable` in function mismatched_allocator.main (test/integration/allocator/mismatched_allocator.zig:6:19).
        Originally allocated by `heap.PageAllocator.vtable`
        """,
        "allocator/mismatched_allocator.zig"
      )
    end

    test "call with freed" do
      assert_errors_with(
        """
        Function call `do_nothing` in function call_with_freed.main (test/integration/allocator/call_with_freed.zig:10:23) was passed a deleted pointer (argument 0).
        Pointer was freed in function call_with_freed.main (test/integration/allocator/call_with_freed.zig:9:18)
        """,
        "allocator/call_with_freed.zig"
      )
    end

    test "return a freed pointer" do
      assert_errors_with(
        """
        Function return in function returns_freed.returns_freed (test/integration/allocator/returns_freed.zig:6:5) returned a freed pointer.
        Pointer was freed at function returns_freed.returns_freed (test/integration/allocator/returns_freed.zig:5:18)
        """,
        "allocator/returns_freed.zig"
      )
    end

    test "free_after_transfer" do
      assert_errors_with(
        """
        Double free detected in function free_after_transfer.main (test/integration/allocator/free_after_transfer.zig:10:18).
        Previously freed in function free_after_transfer.function_deletes (test/integration/allocator/free_after_transfer.zig:4:18)
        Pointer was transferred to free_after_transfer.function_deletes
        """,
        "allocator/free_after_transfer.zig"
      )
    end

    @tag :skip
    test "transfer to global"

    @tag :skip
    test "linked list on heap"

    # leaks
    test "leaked allocation" do
      assert_errors_with(
        """
        Function return at function leaks.main (test/integration/allocator/leaks.zig:5:3) leaked memory allocated in function leaks.main (test/integration/allocator/leaks.zig:4:23)
        """,
        "allocator/leaks.zig"
      )
    end

    test "leaked allocation from function return" do
      assert_errors_with(
        """
        Function return at function leaked_from_function_return.main (test/integration/allocator/leaked_from_function_return.zig:9:3) leaked memory allocated in function leaked_from_function_return.allocate_for_me (test/integration/allocator/leaked_from_function_return.zig:4:22)
        """,
        "allocator/leaked_from_function_return.zig"
      )
    end
  end

  describe "units" do
    test "mismatch" do
      assert_errors_with(
        """
        Mismatched units found in function unit_conflict.main (test/integration/units/unit_conflict.zig:27:15).
        Left hand side: m/s
        Right hand side: ft/s
        """,
        "units/unit_conflict.zig"
      )
    end
  end
end
