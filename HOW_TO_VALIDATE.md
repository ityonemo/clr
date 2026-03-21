# How to Validate Vendor Projects with CLR

This document describes the scheme for validating third-party Zig projects using CLR's static analysis.

## Overview

Vendor validation involves:
1. Adding the project as a git submodule in `vendor/`
2. Creating a `clr_wrapper.zig` file that exercises key patterns
3. Running CLR analysis to verify no false positives

## Directory Structure

```
vendor/
├── validate/           # File format validator
│   ├── clr_wrapper.zig # CLR test wrapper
│   └── ...
├── forestmq/           # Message queue library
│   ├── clr_wrapper.zig # CLR test wrapper
│   └── ...
```

## Step 1: Add as Submodule

```bash
git submodule add https://github.com/user/project vendor/project
```

## Step 2: Create clr_wrapper.zig

Create a `clr_wrapper.zig` file in the vendor project root that:
- Imports and uses the library's key components
- Exercises common memory patterns (allocators, containers, error handling)
- Has a `main()` function that returns `u8` for CLR analysis
- Uses patterns that CLR tracks: GPA, arena, hashmap, slices, error unions

Example template:

```zig
const std = @import("std");
// Import library modules
const lib = @import("src/lib.zig");

pub fn main() u8 {
    // Use GPA for memory tracking
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Exercise library functionality
    var instance = lib.init(allocator) catch return 1;
    defer instance.deinit();

    // Test key operations
    instance.doSomething() catch return 1;

    return 0;
}
```

## Step 3: Run Validation

```bash
./run_one.sh vendor/project/clr_wrapper.zig
```

## Current Vendor Projects

### validate
- **Purpose**: File format detection and validation
- **Patterns**: Arena allocator, file I/O, error handling
- **Wrapper**: `vendor/validate/clr_wrapper.zig`

### forestmq
- **Purpose**: Message queue implementation
- **Patterns**: GPA allocator, StringHashMap, circular buffer, errdefer
- **Wrapper**: `vendor/forestmq/clr_wrapper.zig` (TODO)

## Key Patterns to Exercise

When creating a wrapper, try to exercise these CLR-tracked patterns:

1. **Allocator lifecycle**: init/deinit with GPA or Arena
2. **Container operations**: HashMap, ArrayList with proper cleanup
3. **Error handling**: try/catch, errdefer for cleanup
4. **Pointer tracking**: Struct fields containing pointers
5. **Slice operations**: alloc/free, dupe
6. **Optional handling**: Null checks before unwrap

## Troubleshooting

### False Positives

If CLR reports errors on correct code:
1. Run `./run_one.sh` to generate the `.air.zig` file
2. Inspect the generated analyzer to understand the instruction sequence
3. File an issue or create a minimal reproduction in `test/cases/`

### Compilation Errors

If the wrapper fails to compile with CLR:
1. Check if the library uses patterns CLR doesn't support yet (see LIMITATIONS.md)
2. Simplify the wrapper to isolate the problematic pattern
3. Consider creating a simpler wrapper that avoids unsupported features

## Adding a New Vendor Project

Checklist:
- [ ] Add submodule: `git submodule add <url> vendor/<name>`
- [ ] Create `vendor/<name>/clr_wrapper.zig`
- [ ] Verify with `./run_one.sh vendor/<name>/clr_wrapper.zig`
- [ ] Commit wrapper changes
