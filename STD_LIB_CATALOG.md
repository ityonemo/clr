# Zig Standard Library Shim Catalog

This document catalogs every public function in the Zig standard library, categorizing each as:
- **passthru** - No special handling needed, normal analysis works
- **shim** - Needs custom handler to work correctly with analysis
- **forbidden** - Should not be called directly (already wrapped or unsafe for analysis)

## Category Definitions

### Passthru
Functions that work correctly with normal AIR analysis:
- Pure functions (math, string operations)
- Type operations and introspection
- Functions that delegate to already-shimmed interfaces
- Functions with no side effects relevant to memory/safety analysis

### Shim
Functions that need custom handlers because they:
- Use `@ptrToInt`/`@intToPtr` that break refinement tracking
- Return uninitialized memory
- Have side effects that need tracking (allocation, I/O, state mutation)
- Perform operations that create/destroy resources
- Use inline assembly or direct syscalls internally

### Forbidden
Functions that should not be called directly:
- Low-level syscalls (mmap, munmap) - use Allocator interface instead
- Thread operations (spawn, etc.) - concurrency not supported yet
- Inline assembly functions
- Direct kernel interfaces
- Functions wrapped by higher-level interfaces

### Ignored
Functions that are completely ignored by analysis (**IMPLEMENTED** via runtime call filters):
- Debugging and logging utilities (std.debug, std.log)
- I/O side effects that don't affect memory safety
- These functions are recognized and skipped - no refinement tracking needed

---

## Already Implemented

### Runtime Call Filters (memory_safety.zig)
| Function | Status | Notes |
|----------|--------|-------|
| `std.mem.Allocator.create` | shim | Tracks allocation lifecycle |
| `std.mem.Allocator.destroy` | shim | Validates free, detects double-free |
| `std.mem.Allocator.alloc` | shim | Slice allocation tracking |
| `std.mem.Allocator.free` | shim | Validates free, detects mismatched allocator |
| `std.mem.Allocator.resize` | shim | Handles in-place resize |
| `std.mem.Allocator.realloc` | shim | Old slice marked freed |
| `std.mem.Allocator.remap` | shim | Old slice marked freed |
| `std.mem.Allocator.dupe` | shim | Creates new allocation |
| `std.mem.Allocator.dupeZ` | shim | Creates new allocation with sentinel |
| `std.mem.Allocator.alignedAlloc` | shim | Aligned slice allocation |
| `std.heap.ArenaAllocator.init` | shim | Arena lifecycle start |
| `std.heap.ArenaAllocator.deinit` | shim | Arena lifecycle end, frees all |
| `std.heap.ArenaAllocator.allocator` | shim | Returns allocator handle |

### Ignored Functions (runtime call filters)
| Function | Status | Notes |
|----------|--------|-------|
| `std.debug.*` | ignored | All debug functions skipped |
| `std.log.*` | ignored | All logging functions skipped |
| `std.fmt.format` | ignored | Formatting (no memory safety impact) |
| `std.fmt.bufPrint` | ignored | Buffer format (no memory safety impact) |
| `std.mem.eql` | ignored | Memory comparison (no memory safety impact) |
| `std.mem.startsWith` | ignored | Prefix check (no memory safety impact) |
| `std.mem.endsWith` | ignored | Suffix check (no memory safety impact) |

---

## Planned: Allocator Lifecycle Safety

Expand init/deinit tracking to all allocator types as part of memory safety analysis.

### Lifecycle Tracking (all allocators)

All allocators will track:
- **Use-after-deinit** - Accessing allocator after deinit called
- **Double-deinit** - Calling deinit twice
- **Allocation-after-deinit** - Allocating after deinit called

### Allocator-Specific Semantics

**ArenaAllocator / FixedBufferAllocator** (bulk-free semantics):
- Allocations don't need individual `free` calls
- All allocations implicitly freed on `deinit`
- No leak detection at deinit (already implemented for Arena)

**GeneralPurposeAllocator / MemoryPool / others** (explicit-free semantics):
- Allocations MUST be freed before `deinit`
- Leak detection at deinit time (static equivalent of GPA's runtime leak reporting)
- Missing `free` before `deinit` is an error

### Implementation Notes

- GPA.init/deinit need shim handlers (currently only `.allocator()` is implemented)
- MemoryPool.init/deinit need shim handlers
- Need to track which allocator "owns" each allocation for leak attribution

---

## std.mem

Memory utilities and allocator interface.

| Function | Category | Reason |
|----------|----------|--------|
| `Allocator.create` | shim | **IMPLEMENTED** - allocation tracking |
| `Allocator.destroy` | shim | **IMPLEMENTED** - free tracking |
| `Allocator.alloc` | shim | **IMPLEMENTED** - slice allocation |
| `Allocator.alignedAlloc` | shim | **IMPLEMENTED** - aligned allocation |
| `Allocator.allocSentinel` | shim | **IMPLEMENTED** - sentinel allocation |
| `Allocator.free` | shim | **IMPLEMENTED** - free tracking |
| `Allocator.resize` | shim | **IMPLEMENTED** - resize tracking |
| `Allocator.realloc` | shim | **IMPLEMENTED** - realloc tracking |
| `Allocator.remap` | shim | **IMPLEMENTED** - remap tracking |
| `Allocator.dupe` | shim | **IMPLEMENTED** - dupe tracking |
| `Allocator.dupeZ` | shim | **IMPLEMENTED** - dupeZ tracking |
| `Allocator.shrink` | shim | Needs tracking (resize variant) |
| `eql` | ignored | **IMPLEMENTED** - no memory safety impact |
| `startsWith` | ignored | **IMPLEMENTED** - no memory safety impact |
| `endsWith` | ignored | **IMPLEMENTED** - no memory safety impact |
| `indexOf` | passthru | Pure search |
| `indexOfScalar` | passthru | Pure search |
| `indexOfAny` | passthru | Pure search |
| `indexOfNone` | passthru | Pure search |
| `lastIndexOf` | passthru | Pure search |
| `lastIndexOfScalar` | passthru | Pure search |
| `count` | passthru | Pure counting |
| `containsAtLeast` | passthru | Pure search |
| `replace` | passthru | In-place modification (caller owns buffer) |
| `replaceScalar` | passthru | In-place modification |
| `copy` | passthru | Copies to caller-owned buffer |
| `copyBackwards` | passthru | Copies to caller-owned buffer |
| `set` | passthru | Sets caller-owned buffer |
| `zeroes` | passthru | Returns zero-initialized value |
| `zeroInit` | passthru | Zero-initializes type |
| `order` | passthru | Pure comparison |
| `lessThan` | passthru | Pure comparison |
| `min` | passthru | Pure math |
| `max` | passthru | Pure math |
| `reverse` | passthru | In-place modification |
| `rotate` | passthru | In-place modification |
| `swap` | passthru | Pure swap |
| `span` | passthru | Slice view creation |
| `sliceTo` | passthru | Slice view creation |
| `trim` | passthru | Slice view creation |
| `trimLeft` | passthru | Slice view creation |
| `trimRight` | passthru | Slice view creation |
| `tokenizeAny` | passthru | Iterator creation |
| `tokenizeScalar` | passthru | Iterator creation |
| `tokenizeSequence` | passthru | Iterator creation |
| `splitAny` | passthru | Iterator creation |
| `splitScalar` | passthru | Iterator creation |
| `splitSequence` | passthru | Iterator creation |
| `splitBackwardsAny` | passthru | Iterator creation |
| `splitBackwardsScalar` | passthru | Iterator creation |
| `splitBackwardsSequence` | passthru | Iterator creation |
| `window` | passthru | Iterator creation |
| `join` | passthru | Takes Allocator, tracking via interface |
| `joinZ` | passthru | Takes Allocator, tracking via interface |
| `concat` | passthru | Takes Allocator, tracking via interface |
| `concatWithSentinel` | passthru | Takes Allocator, tracking via interface |
| `bytesAsSlice` | shim | Pointer cast, breaks refinement tracking |
| `sliceAsBytes` | shim | Pointer cast, breaks refinement tracking |
| `bytesToValue` | shim | Pointer cast, breaks refinement tracking |
| `toBytes` | shim | Pointer cast, breaks refinement tracking |
| `asBytes` | shim | Pointer cast, breaks refinement tracking |
| `alignInSlice` | shim | Pointer arithmetic |
| `alignInBytes` | shim | Pointer arithmetic |
| `isAligned` | passthru | Pure check |
| `isAlignedLog2` | passthru | Pure check |
| `alignForward` | passthru | Pure arithmetic |
| `alignForwardLog2` | passthru | Pure arithmetic |
| `alignBackward` | passthru | Pure arithmetic |
| `alignBackwardLog2` | passthru | Pure arithmetic |
| `readInt` | passthru | Pure read (caller owns slice) |
| `readIntNative` | passthru | Pure read |
| `readIntForeign` | passthru | Pure read |
| `readIntLittle` | passthru | Pure read |
| `readIntBig` | passthru | Pure read |
| `writeInt` | passthru | Writes to caller-owned buffer |
| `writeIntNative` | passthru | Writes to caller-owned buffer |
| `writeIntForeign` | passthru | Writes to caller-owned buffer |
| `writeIntLittle` | passthru | Writes to caller-owned buffer |
| `writeIntBig` | passthru | Writes to caller-owned buffer |
| `nativeTo` | passthru | Pure conversion |
| `bigToNative` | passthru | Pure conversion |
| `littleToNative` | passthru | Pure conversion |
| `nativeToLittle` | passthru | Pure conversion |
| `nativeToBig` | passthru | Pure conversion |
| `byteSwap` | passthru | Pure conversion |

---

## std.heap

Heap allocators and memory management.

### Comptime Allocators

| Function | Category | Reason |
|----------|----------|--------|
| `page_allocator` | shim | **IMPLEMENTED** - comptime allocator, tracked via Allocator interface |
| `raw_c_allocator` | shim | **IMPLEMENTED** - comptime allocator, tracked via Allocator interface |
| `c_allocator` | shim | **IMPLEMENTED** - comptime allocator, tracked via Allocator interface |

### ArenaAllocator

| Function | Category | Reason |
|----------|----------|--------|
| `ArenaAllocator.init` | shim | **IMPLEMENTED** - lifecycle tracking |
| `ArenaAllocator.deinit` | shim | **IMPLEMENTED** - lifecycle tracking |
| `ArenaAllocator.allocator` | shim | **IMPLEMENTED** - returns handle |
| `ArenaAllocator.reset` | shim | Frees all arena allocations |
| `ArenaAllocator.queryCapacity` | passthru | Pure query |
| `ArenaAllocator.resize` | forbidden | Internal vtable impl, use Allocator interface |
| `ArenaAllocator.alloc` | forbidden | Internal vtable impl, use Allocator interface |
| `ArenaAllocator.free` | forbidden | Internal vtable impl, use Allocator interface |

### GeneralPurposeAllocator

| Function | Category | Reason |
|----------|----------|--------|
| `GeneralPurposeAllocator.init` | shim | Allocator lifecycle |
| `GeneralPurposeAllocator.deinit` | shim | Allocator lifecycle, leak detection |
| `GeneralPurposeAllocator.allocator` | shim | **IMPLEMENTED** - returns handle |
| `GeneralPurposeAllocator.alloc` | forbidden | Internal vtable impl, uses @ptrFromInt |
| `GeneralPurposeAllocator.resize` | forbidden | Internal vtable impl, uses @ptrFromInt |
| `GeneralPurposeAllocator.free` | forbidden | Internal vtable impl, uses @ptrFromInt |

### FixedBufferAllocator

| Function | Category | Reason |
|----------|----------|--------|
| `FixedBufferAllocator.init` | passthru | Just stores buffer reference |
| `FixedBufferAllocator.allocator` | shim | **IMPLEMENTED** - returns handle |
| `FixedBufferAllocator.reset` | passthru | Resets internal offset |
| `FixedBufferAllocator.alloc` | forbidden | Internal vtable impl, use Allocator interface |
| `FixedBufferAllocator.resize` | forbidden | Internal vtable impl, use Allocator interface |
| `FixedBufferAllocator.free` | forbidden | Internal vtable impl, use Allocator interface |

### MemoryPool

| Function | Category | Reason |
|----------|----------|--------|
| `MemoryPool.init` | shim | Pool lifecycle, may allocate backing |
| `MemoryPool.deinit` | shim | Pool lifecycle, frees backing |
| `MemoryPool.create` | shim | Pool allocation |
| `MemoryPool.destroy` | shim | Pool free |
| `MemoryPool.alloc` | forbidden | Internal vtable impl, use Allocator interface |
| `MemoryPool.resize` | forbidden | Internal vtable impl, use Allocator interface |
| `MemoryPool.free` | forbidden | Internal vtable impl, use Allocator interface |
| `MemoryPoolAligned.init` | shim | Pool lifecycle |
| `MemoryPoolAligned.deinit` | shim | Pool lifecycle |
| `MemoryPoolExtra.init` | shim | Pool lifecycle |
| `MemoryPoolExtra.deinit` | shim | Pool lifecycle |

### PageAllocator

| Function | Category | Reason |
|----------|----------|--------|
| `PageAllocator.alloc` | forbidden | Internal vtable impl, uses @ptrFromInt |
| `PageAllocator.resize` | forbidden | Internal vtable impl, uses @ptrFromInt |
| `PageAllocator.free` | forbidden | Internal vtable impl, uses @ptrFromInt |

### SmpAllocator

| Function | Category | Reason |
|----------|----------|--------|
| `SmpAllocator.alloc` | forbidden | Internal vtable impl, uses @ptrFromInt |
| `SmpAllocator.resize` | forbidden | Internal vtable impl, uses @ptrFromInt |
| `SmpAllocator.free` | forbidden | Internal vtable impl, uses @ptrFromInt |

### WasmAllocator

| Function | Category | Reason |
|----------|----------|--------|
| `WasmAllocator.alloc` | forbidden | Internal vtable impl, uses @ptrFromInt |
| `WasmAllocator.resize` | forbidden | Internal vtable impl, uses @ptrFromInt |
| `WasmAllocator.free` | forbidden | Internal vtable impl, uses @ptrFromInt |

### ThreadSafeAllocator

| Function | Category | Reason |
|----------|----------|--------|
| `ThreadSafeAllocator.init` | passthru | Just wraps another allocator with mutex |
| `ThreadSafeAllocator.allocator` | shim | **IMPLEMENTED** - returns handle |

### StackFallbackAllocator

| Function | Category | Reason |
|----------|----------|--------|
| `StackFallbackAllocator.init` | passthru | Just stores buffer and fallback allocator |
| `StackFallbackAllocator.allocator` | shim | **IMPLEMENTED** - returns handle |

### LoggingAllocator

| Function | Category | Reason |
|----------|----------|--------|
| `LoggingAllocator.init` | passthru | Just wraps another allocator |
| `LoggingAllocator.allocator` | shim | **IMPLEMENTED** - returns handle |

### ScopedAllocator

| Function | Category | Reason |
|----------|----------|--------|
| `ScopedAllocator.init` | shim | Allocator lifecycle |
| `ScopedAllocator.deinit` | shim | Allocator lifecycle |

---

## std.math

Mathematical functions - almost all are pure and passthru.

| Function | Category | Reason |
|----------|----------|--------|
| `add` | passthru | Pure arithmetic |
| `sub` | passthru | Pure arithmetic |
| `mul` | passthru | Pure arithmetic |
| `divFloor` | passthru | Pure arithmetic |
| `divCeil` | passthru | Pure arithmetic |
| `divTrunc` | passthru | Pure arithmetic |
| `divExact` | passthru | Pure arithmetic |
| `mod` | passthru | Pure arithmetic |
| `rem` | passthru | Pure arithmetic |
| `shl` | passthru | Pure bitwise |
| `shlExact` | passthru | Pure bitwise |
| `shlWithOverflow` | passthru | Pure bitwise |
| `shr` | passthru | Pure bitwise |
| `shrExact` | passthru | Pure bitwise |
| `rotr` | passthru | Pure bitwise |
| `rotl` | passthru | Pure bitwise |
| `neg` | passthru | Pure arithmetic |
| `abs` | passthru | Pure arithmetic |
| `absCast` | passthru | Pure cast |
| `lossyCast` | passthru | Pure cast |
| `cast` | passthru | Pure cast |
| `sqrt` | passthru | Pure math |
| `cbrt` | passthru | Pure math |
| `pow` | passthru | Pure math |
| `powi` | passthru | Pure math |
| `exp` | passthru | Pure math |
| `exp2` | passthru | Pure math |
| `expm1` | passthru | Pure math |
| `log` | passthru | Pure math |
| `log2` | passthru | Pure math |
| `log10` | passthru | Pure math |
| `log1p` | passthru | Pure math |
| `ln` | passthru | Pure math |
| `sin` | passthru | Pure math |
| `cos` | passthru | Pure math |
| `tan` | passthru | Pure math |
| `asin` | passthru | Pure math |
| `acos` | passthru | Pure math |
| `atan` | passthru | Pure math |
| `atan2` | passthru | Pure math |
| `sinh` | passthru | Pure math |
| `cosh` | passthru | Pure math |
| `tanh` | passthru | Pure math |
| `asinh` | passthru | Pure math |
| `acosh` | passthru | Pure math |
| `atanh` | passthru | Pure math |
| `hypot` | passthru | Pure math |
| `ceil` | passthru | Pure math |
| `floor` | passthru | Pure math |
| `trunc` | passthru | Pure math |
| `round` | passthru | Pure math |
| `fabs` | passthru | Pure math |
| `fma` | passthru | Pure math |
| `fmod` | passthru | Pure math |
| `copysign` | passthru | Pure math |
| `frexp` | passthru | Pure math |
| `ldexp` | passthru | Pure math |
| `modf` | passthru | Pure math |
| `scalbn` | passthru | Pure math |
| `ilogb` | passthru | Pure math |
| `logb` | passthru | Pure math |
| `signbit` | passthru | Pure predicate |
| `isNan` | passthru | Pure predicate |
| `isInf` | passthru | Pure predicate |
| `isFinite` | passthru | Pure predicate |
| `isNormal` | passthru | Pure predicate |
| `isPositiveInf` | passthru | Pure predicate |
| `isNegativeInf` | passthru | Pure predicate |
| `isPositiveZero` | passthru | Pure predicate |
| `isNegativeZero` | passthru | Pure predicate |
| `min` | passthru | Pure comparison |
| `max` | passthru | Pure comparison |
| `clamp` | passthru | Pure clamping |
| `order` | passthru | Pure comparison |
| `gcd` | passthru | Pure math |
| `ceilPowerOfTwo` | passthru | Pure math |
| `floorPowerOfTwo` | passthru | Pure math |
| `isPowerOfTwo` | passthru | Pure predicate |
| `log2_int` | passthru | Pure math |
| `log2_int_ceil` | passthru | Pure math |
| `alignForward` | passthru | Pure alignment |
| `alignBackward` | passthru | Pure alignment |
| `alignForwardGeneric` | passthru | Pure alignment |
| `alignBackwardGeneric` | passthru | Pure alignment |
| `big.int.*` | passthru | Big integer ops (pure) |

---

## std.fmt

String formatting.

| Function | Category | Reason |
|----------|----------|--------|
| `format` | ignored | **IMPLEMENTED** - no memory safety impact |
| `formatBuf` | ignored | **IMPLEMENTED** - no memory safety impact |
| `allocPrint` | passthru | Takes Allocator, tracking via interface |
| `allocPrintZ` | passthru | Takes Allocator, tracking via interface |
| `bufPrint` | ignored | **IMPLEMENTED** - no memory safety impact |
| `bufPrintZ` | ignored | **IMPLEMENTED** - no memory safety impact |
| `count` | passthru | Pure counting |
| `comptimePrint` | passthru | Comptime only |
| `parseInt` | passthru | Pure parsing |
| `parseUnsigned` | passthru | Pure parsing |
| `parseFloat` | passthru | Pure parsing |
| `charToDigit` | passthru | Pure conversion |
| `digitToChar` | passthru | Pure conversion |
| `fmtIntSizeDec` | passthru | Returns formatter |
| `fmtIntSizeBin` | passthru | Returns formatter |
| `fmtSliceHexLower` | passthru | Returns formatter |
| `fmtSliceHexUpper` | passthru | Returns formatter |
| `fmtSliceEscapeLower` | passthru | Returns formatter |
| `fmtSliceEscapeUpper` | passthru | Returns formatter |
| `bytesToHex` | passthru | Pure conversion |
| `hexToBytes` | passthru | Pure conversion |

---

## std.debug

Debugging utilities - **all ignored** (**IMPLEMENTED** via runtime call filters).

| Function | Category | Reason |
|----------|----------|--------|
| `print` | ignored | **IMPLEMENTED** - I/O, no memory safety impact |
| `assert` | ignored | **IMPLEMENTED** - control flow |
| `panic` | ignored | **IMPLEMENTED** - terminates program |
| `dumpStackTrace` | ignored | **IMPLEMENTED** - I/O |
| `getStackTrace` | ignored | **IMPLEMENTED** - stack inspection |
| `dumpCurrentStackTrace` | ignored | **IMPLEMENTED** - I/O |
| `captureStackTrace` | ignored | **IMPLEMENTED** - stack inspection |
| `StackIterator.*` | ignored | **IMPLEMENTED** - stack inspection |
| `SelfInfo.*` | ignored | **IMPLEMENTED** - debug info |
| `SourceLocation.*` | ignored | **IMPLEMENTED** - debug info |

---

## std.log

Logging functions - **all ignored** (**IMPLEMENTED** via runtime call filters).

| Function | Category | Reason |
|----------|----------|--------|
| `debug` | ignored | **IMPLEMENTED** - I/O, no memory safety impact |
| `info` | ignored | **IMPLEMENTED** - I/O, no memory safety impact |
| `warn` | ignored | **IMPLEMENTED** - I/O, no memory safety impact |
| `err` | ignored | **IMPLEMENTED** - I/O, no memory safety impact |
| `scoped` | ignored | **IMPLEMENTED** - logger config |
| `default` | ignored | **IMPLEMENTED** - logger config |

---

## std.testing

Testing utilities - **all ignored** (needs implementation).

| Function | Category | Reason |
|----------|----------|--------|
| `expect` | ignored | Assertion, no memory safety impact |
| `expectEqual` | ignored | Assertion, no memory safety impact |
| `expectEqualSlices` | ignored | Assertion, no memory safety impact |
| `expectEqualStrings` | ignored | Assertion, no memory safety impact |
| `expectEqualDeep` | ignored | Assertion, no memory safety impact |
| `expectApproxEqAbs` | ignored | Assertion, no memory safety impact |
| `expectApproxEqRel` | ignored | Assertion, no memory safety impact |
| `expectError` | ignored | Assertion, no memory safety impact |
| `expectFmt` | ignored | Assertion, no memory safety impact |
| `refAllDecls` | ignored | Comptime only |
| `refAllDeclsRecursive` | ignored | Comptime only |
| `allocator` | ignored | Test allocator, no memory safety impact |
| `failing_allocator` | ignored | Test allocator, no memory safety impact |
| `FailingAllocator.*` | ignored | Test allocator, no memory safety impact |
| `checkAllAllocationFailures` | ignored | Test utility, no memory safety impact |

---

## std.ArrayList

Growable array list - **all passthru** (allocations tracked via Allocator interface).

| Function | Category | Reason |
|----------|----------|--------|
| `init` | passthru | Stores allocator reference |
| `initCapacity` | passthru | Uses Allocator.alloc (shimmed) |
| `deinit` | passthru | Uses Allocator.free (shimmed) |
| `toOwnedSlice` | passthru | Returns internal slice |
| `toOwnedSliceSentinel` | passthru | Returns internal slice |
| `fromOwnedSlice` | passthru | Wraps existing slice |
| `fromOwnedSliceSentinel` | passthru | Wraps existing slice |
| `clone` | passthru | Uses Allocator.alloc (shimmed) |
| `append` | passthru | Uses Allocator.realloc (shimmed) |
| `appendSlice` | passthru | Uses Allocator.realloc (shimmed) |
| `appendNTimes` | passthru | Uses Allocator.realloc (shimmed) |
| `insert` | passthru | Uses Allocator.realloc (shimmed) |
| `insertSlice` | passthru | Uses Allocator.realloc (shimmed) |
| `replaceRange` | passthru | Uses Allocator.realloc (shimmed) |
| `*AssumeCapacity` | passthru | No allocation |
| `orderedRemove` | passthru | No allocation |
| `swapRemove` | passthru | No allocation |
| `pop` | passthru | No allocation |
| `popOrNull` | passthru | No allocation |
| `getLast` | passthru | Pure access |
| `getLastOrNull` | passthru | Pure access |
| `items` | passthru | Returns slice view |
| `unusedCapacitySlice` | passthru | Returns slice view |
| `addManyAs*` | passthru | Uses Allocator.realloc (shimmed) |
| `addOne` | passthru | Uses Allocator.realloc (shimmed) |
| `resize` | passthru | Uses Allocator.realloc (shimmed) |
| `shrinkAndFree` | passthru | Uses Allocator.free (shimmed) |
| `shrinkRetainingCapacity` | passthru | No allocation |
| `clearAndFree` | passthru | Uses Allocator.free (shimmed) |
| `clearRetainingCapacity` | passthru | No allocation |
| `ensureTotalCapacity` | passthru | Uses Allocator.realloc (shimmed) |
| `ensureTotalCapacityPrecise` | passthru | Uses Allocator.realloc (shimmed) |
| `ensureUnusedCapacity` | passthru | Uses Allocator.realloc (shimmed) |
| `expandToCapacity` | passthru | No allocation |
| `writer` | passthru | Returns writer over internal buffer |

---

## std.HashMap / std.AutoHashMap

Hash map implementations - **all passthru** (allocations tracked via Allocator interface).

| Function | Category | Reason |
|----------|----------|--------|
| `init` | passthru | Stores allocator reference |
| `initContext` | passthru | Stores allocator reference |
| `deinit` | passthru | Uses Allocator.free (shimmed) |
| `clone` | passthru | Uses Allocator.alloc (shimmed) |
| `put` | passthru | Uses Allocator.realloc (shimmed) |
| `putNoClobber` | passthru | Uses Allocator.realloc (shimmed) |
| `getOrPut` | passthru | Uses Allocator.realloc (shimmed) |
| `getOrPutValue` | passthru | Uses Allocator.realloc (shimmed) |
| `*AssumeCapacity*` | passthru | No allocation |
| `get` | passthru | Pure lookup |
| `getPtr` | passthru | Pure lookup |
| `getKey` | passthru | Pure lookup |
| `getKeyPtr` | passthru | Pure lookup |
| `getEntry` | passthru | Pure lookup |
| `contains` | passthru | Pure lookup |
| `count` | passthru | Pure query |
| `remove` | passthru | No allocation |
| `fetchRemove` | passthru | No allocation |
| `swapRemove` | passthru | No allocation |
| `fetchSwapRemove` | passthru | No allocation |
| `keys` | passthru | Returns iterator |
| `values` | passthru | Returns iterator |
| `iterator` | passthru | Returns iterator |
| `keyIterator` | passthru | Returns iterator |
| `valueIterator` | passthru | Returns iterator |
| `ensureTotalCapacity` | passthru | Uses Allocator.realloc (shimmed) |
| `ensureUnusedCapacity` | passthru | Uses Allocator.realloc (shimmed) |
| `clearAndFree` | passthru | Uses Allocator.free (shimmed) |
| `clearRetainingCapacity` | passthru | No allocation |
| `capacity` | passthru | Pure query |
| `allocatedSize` | passthru | Pure query |

---

## std.fs

File system operations - **all passthru** (fd lifecycle tracked via posix.open/close).

| Function | Category | Reason |
|----------|----------|--------|
| `cwd` | passthru | Returns Dir handle, calls posix |
| `openDirAbsolute` | passthru | Calls posix.open (shimmed) |
| `openDirAbsoluteZ` | passthru | Calls posix.open (shimmed) |
| `openDirAbsoluteW` | passthru | Calls posix.open (shimmed) |
| `openFileAbsolute` | passthru | Calls posix.open (shimmed) |
| `openFileAbsoluteZ` | passthru | Calls posix.open (shimmed) |
| `openFileAbsoluteW` | passthru | Calls posix.open (shimmed) |
| `createFileAbsolute` | passthru | Calls posix.open (shimmed) |
| `createFileAbsoluteZ` | passthru | Calls posix.open (shimmed) |
| `createFileAbsoluteW` | passthru | Calls posix.open (shimmed) |
| `accessAbsolute` | passthru | Syscall, no fd lifecycle |
| `makeDirAbsolute` | passthru | Syscall, no fd lifecycle |
| `makePath` | passthru | Syscalls, no fd lifecycle |
| `deleteFileAbsolute` | passthru | Syscall, no fd lifecycle |
| `deleteDirAbsolute` | passthru | Syscall, no fd lifecycle |
| `deleteTreeAbsolute` | passthru | Syscalls, no fd lifecycle |
| `renameAbsolute` | passthru | Syscall, no fd lifecycle |
| `copyFileAbsolute` | passthru | Uses shimmed posix calls |
| `realpath` | passthru | Takes Allocator, tracking via interface |
| `realpathAlloc` | passthru | Takes Allocator, tracking via interface |
| `realpathZ` | passthru | Takes Allocator, tracking via interface |
| `selfExePath` | passthru | Takes Allocator, tracking via interface |
| `selfExeDirPath` | passthru | Takes Allocator, tracking via interface |
| `path.join` | passthru | Takes Allocator, tracking via interface |
| `path.joinZ` | passthru | Takes Allocator, tracking via interface |
| `path.resolve` | passthru | Takes Allocator, tracking via interface |
| `path.dirname` | passthru | Pure string op |
| `path.basename` | passthru | Pure string op |
| `path.extension` | passthru | Pure string op |
| `path.stem` | passthru | Pure string op |
| `path.isAbsolute` | passthru | Pure predicate |
| `path.relative` | passthru | Takes Allocator, tracking via interface |

### std.fs.File

All File methods are **passthru** - they call through to shimmed posix primitives.

| Function | Category | Reason |
|----------|----------|--------|
| `File.close` | passthru | Calls posix.close (shimmed) |
| `File.read` | passthru | Calls posix.read |
| `File.readAll` | passthru | Calls posix.read |
| `File.readToEndAlloc` | passthru | Allocation tracked via Allocator |
| `File.readToEndAllocOptions` | passthru | Allocation tracked via Allocator |
| `File.write` | passthru | Calls posix.write |
| `File.writeAll` | passthru | Calls posix.write |
| `File.pread` | passthru | Calls posix.pread |
| `File.preadv` | passthru | Calls posix.preadv |
| `File.pwrite` | passthru | Calls posix.pwrite |
| `File.pwritev` | passthru | Calls posix.pwritev |
| `File.seekTo` | passthru | Calls posix.lseek |
| `File.seekBy` | passthru | Calls posix.lseek |
| `File.getPos` | passthru | Calls posix.lseek |
| `File.getEndPos` | passthru | Calls posix.lseek |
| `File.stat` | passthru | Calls posix.fstat |
| `File.metadata` | passthru | Calls posix.fstat |
| `File.setEndPos` | passthru | Calls posix.ftruncate |
| `File.sync` | passthru | Calls posix.fsync |
| `File.datasync` | passthru | Calls posix.fdatasync |
| `File.lock` | passthru | Calls posix.flock |
| `File.unlock` | passthru | Calls posix.flock |
| `File.reader` | passthru | Returns reader wrapper |
| `File.writer` | passthru | Returns writer wrapper |
| `File.handle` | passthru | Returns raw fd (for interop) |

### std.fs.Dir

All Dir methods are **passthru** - they call through to shimmed posix primitives.

| Function | Category | Reason |
|----------|----------|--------|
| `Dir.close` | passthru | Calls posix.close (shimmed) |
| `Dir.openFile` | passthru | Calls posix.open (shimmed) |
| `Dir.createFile` | passthru | Calls posix.open (shimmed) |
| `Dir.openDir` | passthru | Calls posix.open (shimmed) |
| `Dir.makeDir` | passthru | Syscall, no fd lifecycle |
| `Dir.makePath` | passthru | Syscalls, no fd lifecycle |
| `Dir.deleteFile` | passthru | Syscall, no fd lifecycle |
| `Dir.deleteDir` | passthru | Syscall, no fd lifecycle |
| `Dir.deleteTree` | passthru | Syscalls, no fd lifecycle |
| `Dir.rename` | passthru | Syscall, no fd lifecycle |
| `Dir.symlink` | passthru | Syscall, no fd lifecycle |
| `Dir.readLink` | passthru | Syscall, no fd lifecycle |
| `Dir.readLinkAlloc` | passthru | Allocation tracked via Allocator |
| `Dir.access` | passthru | Syscall, no fd lifecycle |
| `Dir.stat` | passthru | Syscall, no fd lifecycle |
| `Dir.statFile` | passthru | Syscall, no fd lifecycle |
| `Dir.chmod` | passthru | Syscall, no fd lifecycle |
| `Dir.chown` | passthru | Syscall, no fd lifecycle |
| `Dir.iterate` | passthru | Returns iterator wrapper |
| `Dir.walk` | passthru | Allocation tracked via Allocator |
| `Dir.readDir` | passthru | Returns iterator wrapper |
| `Dir.realpath` | passthru | Takes Allocator, tracking via interface |
| `Dir.realpathAlloc` | passthru | Takes Allocator, tracking via interface |
| `Dir.fd` | passthru | Returns raw fd (for interop) |

---

## std.io

I/O abstractions - **all passthru** (fd lifecycle tracked via posix, allocations via Allocator).

| Function | Category | Reason |
|----------|----------|--------|
| `getStdIn` | passthru | Returns pre-opened fd 0 |
| `getStdOut` | passthru | Returns pre-opened fd 1 |
| `getStdErr` | passthru | Returns pre-opened fd 2 |
| `Reader.*` | passthru | Calls through to underlying fd |
| `Reader.*Alloc` | passthru | Allocation tracked via Allocator |
| `Writer.*` | passthru | Calls through to underlying fd |
| `BufferedReader.*` | passthru | Wraps reader, allocation via Allocator |
| `BufferedWriter.*` | passthru | Wraps writer, allocation via Allocator |
| `FixedBufferStream.*` | passthru | Pure memory operations |

---

## std.posix

POSIX system calls - minimal shim set for fd lifecycle, rest passthru.

### FD Lifecycle (shim - creates/destroys file descriptors)

| Function | Category | Reason |
|----------|----------|--------|
| `open` | shim | Creates fd |
| `openat` | shim | Creates fd |
| `close` | shim | Destroys fd |
| `dup` | shim | Creates fd (copy) |
| `dup2` | shim | Creates fd (copy) |
| `pipe` | shim | Creates 2 fds |
| `pipe2` | shim | Creates 2 fds |
| `socket` | shim | Creates fd |
| `accept` | shim | Creates fd |
| `epoll_create` | shim | Creates fd |

### FD Operations (passthru - operates on existing fds)

| Function | Category | Reason |
|----------|----------|--------|
| `read` | passthru | Uses existing fd |
| `write` | passthru | Uses existing fd |
| `pread` | passthru | Uses existing fd |
| `pwrite` | passthru | Uses existing fd |
| `lseek` | passthru | Uses existing fd |
| `ftruncate` | passthru | Uses existing fd |
| `fsync` | passthru | Uses existing fd |
| `fdatasync` | passthru | Uses existing fd |
| `bind` | passthru | Uses existing fd |
| `listen` | passthru | Uses existing fd |
| `connect` | passthru | Uses existing fd |
| `send` | passthru | Uses existing fd |
| `sendto` | passthru | Uses existing fd |
| `recv` | passthru | Uses existing fd |
| `recvfrom` | passthru | Uses existing fd |
| `shutdown` | passthru | Uses existing fd |
| `getsockopt` | passthru | Uses existing fd |
| `setsockopt` | passthru | Uses existing fd |
| `poll` | passthru | Uses existing fds |
| `epoll_ctl` | passthru | Uses existing fd |
| `epoll_wait` | passthru | Uses existing fd |
| `fstat` | passthru | Uses existing fd |
| `fchdir` | passthru | Uses existing fd |

### Filesystem Operations (passthru - no fd lifecycle)

| Function | Category | Reason |
|----------|----------|--------|
| `mkdir` | passthru | No fd lifecycle |
| `mkdirat` | passthru | No fd lifecycle |
| `rmdir` | passthru | No fd lifecycle |
| `unlink` | passthru | No fd lifecycle |
| `unlinkat` | passthru | No fd lifecycle |
| `rename` | passthru | No fd lifecycle |
| `renameat` | passthru | No fd lifecycle |
| `link` | passthru | No fd lifecycle |
| `linkat` | passthru | No fd lifecycle |
| `symlink` | passthru | No fd lifecycle |
| `symlinkat` | passthru | No fd lifecycle |
| `readlink` | passthru | No fd lifecycle |
| `readlinkat` | passthru | No fd lifecycle |
| `stat` | passthru | No fd lifecycle |
| `lstat` | passthru | No fd lifecycle |
| `fstatat` | passthru | No fd lifecycle |
| `access` | passthru | No fd lifecycle |
| `faccessat` | passthru | No fd lifecycle |
| `chdir` | passthru | No fd lifecycle |
| `chmod` | passthru | No fd lifecycle |
| `fchmod` | passthru | Uses existing fd |
| `fchmodat` | passthru | No fd lifecycle |
| `chown` | passthru | No fd lifecycle |
| `fchown` | passthru | Uses existing fd |
| `fchownat` | passthru | No fd lifecycle |

### Process/System (passthru or forbidden)

| Function | Category | Reason |
|----------|----------|--------|
| `getpid` | passthru | Pure query |
| `getppid` | passthru | Pure query |
| `getuid` | passthru | Pure query |
| `geteuid` | passthru | Pure query |
| `getgid` | passthru | Pure query |
| `getegid` | passthru | Pure query |
| `getcwd` | passthru | Takes Allocator |
| `kill` | passthru | Signal, no fd lifecycle |
| `sigaction` | passthru | Signal handling |
| `sigprocmask` | passthru | Signal handling |
| `nanosleep` | passthru | Time syscall |
| `clock_gettime` | passthru | Time syscall |
| `gettimeofday` | passthru | Time syscall |
| `getenv` | passthru | Environment query |
| `setenv` | passthru | Environment modification |
| `unsetenv` | passthru | Environment modification |

### Forbidden (use higher-level interfaces)

| Function | Category | Reason |
|----------|----------|--------|
| `mmap` | forbidden | Use Allocator interface |
| `munmap` | forbidden | Use Allocator interface |
| `mprotect` | forbidden | Direct memory protection |
| `mremap` | forbidden | Use Allocator interface |
| `madvise` | forbidden | Direct memory advice |
| `mlock` | forbidden | Direct memory locking |
| `munlock` | forbidden | Direct memory unlocking |
| `fork` | forbidden | Concurrency not supported |
| `execve` | forbidden | Process replacement |
| `execvpe` | forbidden | Process replacement |
| `ioctl` | forbidden | Direct device control |
| `fcntl` | passthru | File control (no fd lifecycle) |

---

## std.Thread

Threading - mostly forbidden (concurrency not supported).

| Function | Category | Reason |
|----------|----------|--------|
| `spawn` | forbidden | Thread creation (concurrency) |
| `join` | forbidden | Thread synchronization |
| `detach` | forbidden | Thread management |
| `yield` | forbidden | Thread yield |
| `getCurrentId` | passthru | Pure query |
| `Mutex.init` | shim | Mutex lifecycle |
| `Mutex.deinit` | shim | Mutex lifecycle |
| `Mutex.lock` | shim | Mutex operation (blocking) |
| `Mutex.unlock` | shim | Mutex operation |
| `Mutex.tryLock` | shim | Mutex operation |
| `RwLock.init` | shim | RwLock lifecycle |
| `RwLock.deinit` | shim | RwLock lifecycle |
| `RwLock.lockShared` | shim | RwLock operation |
| `RwLock.unlockShared` | shim | RwLock operation |
| `RwLock.lockExclusive` | shim | RwLock operation |
| `RwLock.unlockExclusive` | shim | RwLock operation |
| `Condition.init` | shim | Condition lifecycle |
| `Condition.deinit` | shim | Condition lifecycle |
| `Condition.wait` | shim | Condition operation (blocking) |
| `Condition.signal` | shim | Condition operation |
| `Condition.broadcast` | shim | Condition operation |
| `Semaphore.init` | shim | Semaphore lifecycle |
| `Semaphore.deinit` | shim | Semaphore lifecycle |
| `Semaphore.wait` | shim | Semaphore operation (blocking) |
| `Semaphore.post` | shim | Semaphore operation |
| `ResetEvent.init` | shim | Event lifecycle |
| `ResetEvent.deinit` | shim | Event lifecycle |
| `ResetEvent.set` | shim | Event operation |
| `ResetEvent.reset` | shim | Event operation |
| `ResetEvent.wait` | shim | Event operation (blocking) |
| `Pool.init` | forbidden | Thread pool (concurrency) |
| `Pool.deinit` | forbidden | Thread pool |
| `Pool.spawn` | forbidden | Thread pool |

---

## std.atomic

Atomic operations - **all passthru** (concurrency not supported, just memory ops).

| Function | Category | Reason |
|----------|----------|--------|
| `Atomic.*` | passthru | Memory operations (no concurrency analysis) |
| `spinLoopHint` | passthru | CPU hint |
| `compilerFence` | passthru | Compiler barrier |

---

## std.crypto

Cryptography - **all passthru** (pure functions, syscalls have no resource lifecycle).

| Function | Category | Reason |
|----------|----------|--------|
| `hash.*` | passthru | Pure hashing |
| `aead.*` | passthru | Pure encryption |
| `sign.*` | passthru | Pure signing |
| `kdf.*` | passthru | Pure key derivation |
| `random.*` | passthru | Syscall (getrandom), no resource lifecycle |
| `timing_safe.*` | passthru | Pure operations |
| `utils.secureZero` | passthru | Memory write |
| `Certificate.*` | passthru | Allocation tracked via Allocator |
| `tls.Client.*` | passthru | Uses shimmed posix.socket, Allocator |

---

## std.json

JSON parsing and serialization - **all passthru** (uses Allocator, I/O via shimmed posix).

| Function | Category | Reason |
|----------|----------|--------|
| `parse*` | passthru | Uses Allocator |
| `stringify*` | passthru | Uses shimmed I/O or Allocator |
| `fmt` | passthru | Returns formatter |
| `Value.*` | passthru | Uses shimmed I/O |
| `Scanner.*` | passthru | Pure iteration |
| `Reader.*` | passthru | Uses shimmed I/O |

---

## std.compress

Compression algorithms - **all passthru** (use Allocator internally).

| Function | Category | Reason |
|----------|----------|--------|
| `deflate.*` | passthru | Uses Allocator |
| `gzip.*` | passthru | Uses Allocator |
| `zlib.*` | passthru | Uses Allocator |
| `zstd.*` | passthru | Uses Allocator |
| `lz4.*` | passthru | Uses Allocator |
| `xz.*` | passthru | Uses Allocator |

---

## std.http

HTTP client - **all passthru** (fd via posix, allocations via Allocator).

| Function | Category | Reason |
|----------|----------|--------|
| `Client.*` | passthru | Uses posix.socket (shimmed), Allocator |
| `Request.*` | passthru | Uses posix I/O, Allocator |
| `Response.*` | passthru | Uses posix I/O |
| `Headers.*` | passthru | Allocation tracked via Allocator |
| `Uri.*` | passthru | Pure parsing or Allocator |

---

## std.net

Networking - **all passthru** (fd lifecycle tracked via posix.socket/close).

| Function | Category | Reason |
|----------|----------|--------|
| `Address.parse` | passthru | Pure parsing |
| `Address.parseIp` | passthru | Pure parsing |
| `Address.parseIp4` | passthru | Pure parsing |
| `Address.parseIp6` | passthru | Pure parsing |
| `Address.resolveIp` | passthru | DNS resolution (no fd lifecycle) |
| `Address.format` | passthru | Returns formatter |
| `Stream.init` | passthru | Wraps existing fd |
| `Stream.close` | passthru | Calls posix.close (shimmed) |
| `Stream.read` | passthru | Calls posix.read |
| `Stream.write` | passthru | Calls posix.write |
| `Stream.reader` | passthru | Returns reader wrapper |
| `Stream.writer` | passthru | Returns writer wrapper |
| `tcpConnectToHost` | passthru | Calls posix.socket/connect (shimmed) |
| `tcpConnectToAddress` | passthru | Calls posix.socket/connect (shimmed) |
| `getAddressList` | passthru | Allocation tracked via Allocator |

---

## std.process

Process management.

| Function | Category | Reason |
|----------|----------|--------|
| `Child.*` | forbidden | Process/concurrency not supported |
| `getEnvMap` | passthru | Uses Allocator |
| `getEnvVarOwned` | passthru | Uses Allocator |
| `exit` | passthru | Terminates process |
| `abort` | passthru | Terminates process |
| `args` | passthru | Returns iterator |
| `argsAlloc` | passthru | Uses Allocator |
| `argsFree` | passthru | Uses Allocator.free (shimmed) |
| `ArgIterator.*` | passthru | Iterator, uses Allocator |

---

## std.time

Time functions - **all passthru** (syscalls with no resource lifecycle).

| Function | Category | Reason |
|----------|----------|--------|
| `timestamp` | passthru | Syscall, no resource lifecycle |
| `milliTimestamp` | passthru | Syscall, no resource lifecycle |
| `microTimestamp` | passthru | Syscall, no resource lifecycle |
| `nanoTimestamp` | passthru | Syscall, no resource lifecycle |
| `Instant.*` | passthru | Syscall or pure calculation |
| `Timer.*` | passthru | Syscall or state change |
| `sleep` | passthru | Syscall, no resource lifecycle |
| `epoch.*` | passthru | Pure calendar calculations |

---

## std.Random

Random number generation.

| Function | Category | Reason |
|----------|----------|--------|
| `DefaultPrng.init` | passthru | Initializes PRNG state |
| `DefaultPrng.random` | passthru | Returns Random interface |
| `Xoroshiro128.init` | passthru | Initializes PRNG state |
| `Xoroshiro128.next` | passthru | Pure state transition |
| `Xoshiro256.init` | passthru | Initializes PRNG state |
| `Xoshiro256.next` | passthru | Pure state transition |
| `SplitMix64.init` | passthru | Initializes PRNG state |
| `SplitMix64.next` | passthru | Pure state transition |
| `Pcg.init` | passthru | Initializes PRNG state |
| `Pcg.next` | passthru | Pure state transition |
| `ChaCha.init` | passthru | Initializes CSPRNG state |
| `ChaCha.fill` | passthru | Pure state transition |
| `Random.int` | passthru | Pure RNG |
| `Random.intRangeLessThan` | passthru | Pure RNG |
| `Random.uintLessThan` | passthru | Pure RNG |
| `Random.float` | passthru | Pure RNG |
| `Random.boolean` | passthru | Pure RNG |
| `Random.bytes` | passthru | Pure RNG |
| `Random.shuffle` | passthru | Pure RNG (modifies caller-owned array) |
| `Random.weightedSample` | passthru | Pure RNG |

---

## std.sort

Sorting algorithms.

| Function | Category | Reason |
|----------|----------|--------|
| `sort` | passthru | In-place sort (caller owns buffer) |
| `sortUnstable` | passthru | In-place sort |
| `insertionSort` | passthru | In-place sort |
| `pdq` | passthru | In-place sort |
| `block` | passthru | In-place sort |
| `binarySearch` | passthru | Pure search |
| `lowerBound` | passthru | Pure search |
| `upperBound` | passthru | Pure search |
| `equalRange` | passthru | Pure search |
| `isSorted` | passthru | Pure predicate |
| `argSort` | passthru | Takes Allocator, tracking via interface |
| `asc` | passthru | Returns comparator |
| `desc` | passthru | Returns comparator |

---

## std.meta

Type introspection and comptime utilities.

| Function | Category | Reason |
|----------|----------|--------|
| `Child` | passthru | Type function |
| `Elem` | passthru | Type function |
| `sentinel` | passthru | Type function |
| `activeTag` | passthru | Pure introspection |
| `TagType` | passthru | Type function |
| `TagPayload` | passthru | Type function |
| `TagPayloadByName` | passthru | Type function |
| `FieldType` | passthru | Type function |
| `fieldInfo` | passthru | Type function |
| `fields` | passthru | Type function |
| `fieldNames` | passthru | Type function |
| `declarations` | passthru | Type function |
| `declarationInfo` | passthru | Type function |
| `isContainer` | passthru | Type predicate |
| `isPointer` | passthru | Type predicate |
| `isSlice` | passthru | Type predicate |
| `isIndexable` | passthru | Type predicate |
| `isIntegral` | passthru | Type predicate |
| `isFloat` | passthru | Type predicate |
| `isSignedInt` | passthru | Type predicate |
| `isUnsignedInt` | passthru | Type predicate |
| `Int` | passthru | Type function |
| `Float` | passthru | Type function |
| `Vector` | passthru | Type function |
| `alignment` | passthru | Type function |
| `sizeOf` | passthru | Type function |
| `bitSizeOf` | passthru | Type function |
| `eql` | passthru | Pure comparison |
| `hash` | passthru | Pure hash |
| `stringToEnum` | passthru | Pure conversion |
| `intToEnum` | passthru | Pure conversion |
| `enumToInt` | passthru | Pure conversion |
| `cast` | passthru | Pure cast |
| `ptrCast` | passthru | Pure cast |
| `alignCast` | passthru | Pure cast |
| `constCast` | passthru | Pure cast |
| `volatileCast` | passthru | Pure cast |

---

## std.unicode

Unicode and UTF-8 utilities.

| Function | Category | Reason |
|----------|----------|--------|
| `utf8Decode` | passthru | Pure decoding |
| `utf8Decode2` | passthru | Pure decoding |
| `utf8Decode3` | passthru | Pure decoding |
| `utf8Decode4` | passthru | Pure decoding |
| `utf8Encode` | passthru | Pure encoding |
| `utf8EncodeComptime` | passthru | Comptime only |
| `utf8CountCodepoints` | passthru | Pure counting |
| `utf8ByteSequenceLength` | passthru | Pure calculation |
| `utf8ValidateSlice` | passthru | Pure validation |
| `utf16leToUtf8` | passthru | Takes Allocator, tracking via interface |
| `utf16leToUtf8Alloc` | passthru | Takes Allocator, tracking via interface |
| `utf8ToUtf16Le` | passthru | Takes Allocator, tracking via interface |
| `utf8ToUtf16LeAlloc` | passthru | Takes Allocator, tracking via interface |
| `fmtUtf8` | passthru | Returns formatter |
| `Utf8Iterator.*` | passthru | Iterator over string |
| `Utf16LeIterator.*` | passthru | Iterator over string |

---

## std.ascii

ASCII utilities.

| Function | Category | Reason |
|----------|----------|--------|
| `isAlphanumeric` | passthru | Pure predicate |
| `isAlphabetic` | passthru | Pure predicate |
| `isDigit` | passthru | Pure predicate |
| `isHex` | passthru | Pure predicate |
| `isLower` | passthru | Pure predicate |
| `isUpper` | passthru | Pure predicate |
| `isPrint` | passthru | Pure predicate |
| `isWhitespace` | passthru | Pure predicate |
| `isControl` | passthru | Pure predicate |
| `isASCII` | passthru | Pure predicate |
| `toLower` | passthru | Pure conversion |
| `toUpper` | passthru | Pure conversion |
| `eqlIgnoreCase` | passthru | Pure comparison |
| `startsWithIgnoreCase` | passthru | Pure comparison |
| `endsWithIgnoreCase` | passthru | Pure comparison |
| `indexOfIgnoreCase` | passthru | Pure search |
| `lowerString` | passthru | In-place conversion |
| `upperString` | passthru | In-place conversion |
| `allocLowerString` | passthru | Takes Allocator, tracking via interface |
| `allocUpperString` | passthru | Takes Allocator, tracking via interface |
| `orderIgnoreCase` | passthru | Pure comparison |
| `lessThanIgnoreCase` | passthru | Pure comparison |

---

## std.builtin

Compiler built-ins (mostly passthru or comptime-only).

| Function | Category | Reason |
|----------|----------|--------|
| `default_panic` | passthru | Terminates |
| `panicImpl` | passthru | Terminates |
| `Type.*` | passthru | Type introspection |
| `CallingConvention.*` | passthru | Enum values |
| `Endian.*` | passthru | Enum values |
| `StackTrace.*` | passthru | Type definition |
| `SourceLocation.*` | passthru | Type definition |

---

## std.os.linux (and other std.os.*)

Low-level OS interfaces - mostly forbidden.

| Function | Category | Reason |
|----------|----------|--------|
| `syscall0` | forbidden | Inline assembly |
| `syscall1` | forbidden | Inline assembly |
| `syscall2` | forbidden | Inline assembly |
| `syscall3` | forbidden | Inline assembly |
| `syscall4` | forbidden | Inline assembly |
| `syscall5` | forbidden | Inline assembly |
| `syscall6` | forbidden | Inline assembly |
| `getauxval` | forbidden | Low-level |
| `tls.*` | forbidden | Thread-local storage |
| `clone` | forbidden | Process/thread creation |
| `clone3` | forbidden | Process/thread creation |
| `futex_wait` | forbidden | Low-level synchronization |
| `futex_wake` | forbidden | Low-level synchronization |
| `sigaltstack` | forbidden | Signal handling internals |
| `getcontext` | forbidden | Context manipulation |
| `setcontext` | forbidden | Context manipulation |
| `makecontext` | forbidden | Context manipulation |
| `swapcontext` | forbidden | Context manipulation |

---

## std.simd

SIMD vector operations - mostly passthru.

| Function | Category | Reason |
|----------|----------|--------|
| `iota` | passthru | Pure vector creation |
| `repeat` | passthru | Pure vector creation |
| `join` | passthru | Pure vector operation |
| `interlace` | passthru | Pure vector operation |
| `deinterlace` | passthru | Pure vector operation |
| `extract` | passthru | Pure vector operation |
| `shuffle` | passthru | Pure vector operation |
| `shiftElementsLeft` | passthru | Pure vector operation |
| `shiftElementsRight` | passthru | Pure vector operation |
| `rotateElementsLeft` | passthru | Pure vector operation |
| `rotateElementsRight` | passthru | Pure vector operation |
| `reverseOrder` | passthru | Pure vector operation |
| `reduce` | passthru | Pure vector operation |
| `prefixScan` | passthru | Pure vector operation |
| `firstTrue` | passthru | Pure predicate |
| `lastTrue` | passthru | Pure predicate |
| `countTrues` | passthru | Pure counting |
| `anyTrue` | passthru | Pure predicate |
| `allTrue` | passthru | Pure predicate |
| `negate` | passthru | Pure vector operation |
| `minMax` | passthru | Pure vector operation |
| `clamp` | passthru | Pure vector operation |

---

## std.enums

Enum utilities.

| Function | Category | Reason |
|----------|----------|--------|
| `EnumArray.init` | passthru | Pure initialization |
| `EnumArray.get` | passthru | Pure access |
| `EnumArray.set` | passthru | Pure modification |
| `EnumArray.values` | passthru | Returns array |
| `EnumMap.init` | passthru | Pure initialization |
| `EnumMap.put` | passthru | Pure modification |
| `EnumMap.get` | passthru | Pure access |
| `EnumMap.remove` | passthru | Pure modification |
| `EnumMap.count` | passthru | Pure query |
| `EnumMap.contains` | passthru | Pure predicate |
| `EnumMap.iterator` | passthru | Returns iterator |
| `EnumSet.init` | passthru | Pure initialization |
| `EnumSet.initEmpty` | passthru | Pure initialization |
| `EnumSet.initFull` | passthru | Pure initialization |
| `EnumSet.insert` | passthru | Pure modification |
| `EnumSet.remove` | passthru | Pure modification |
| `EnumSet.contains` | passthru | Pure predicate |
| `EnumSet.count` | passthru | Pure query |
| `EnumSet.iterator` | passthru | Returns iterator |
| `EnumSet.complement` | passthru | Pure set operation |
| `EnumSet.unionWith` | passthru | Pure set operation |
| `EnumSet.intersectWith` | passthru | Pure set operation |
| `EnumSet.differenceWith` | passthru | Pure set operation |
| `EnumSet.symmetricDifferenceWith` | passthru | Pure set operation |
| `EnumSet.subsetOf` | passthru | Pure predicate |
| `EnumSet.supersetOf` | passthru | Pure predicate |
| `EnumSet.eql` | passthru | Pure comparison |
| `nameCast` | passthru | Pure conversion |
| `tagName` | passthru | Pure conversion |
| `directEnumCast` | passthru | Pure conversion |
| `enumFromInt` | passthru | Pure conversion |
| `values` | passthru | Returns array |

---

## std.bit_set

Bit set implementations.

| Function | Category | Reason |
|----------|----------|--------|
| `IntegerBitSet.init` | passthru | Pure initialization |
| `IntegerBitSet.set` | passthru | Pure modification |
| `IntegerBitSet.unset` | passthru | Pure modification |
| `IntegerBitSet.toggle` | passthru | Pure modification |
| `IntegerBitSet.isSet` | passthru | Pure predicate |
| `IntegerBitSet.count` | passthru | Pure query |
| `IntegerBitSet.iterator` | passthru | Returns iterator |
| `IntegerBitSet.setUnion` | passthru | Pure set operation |
| `IntegerBitSet.setIntersection` | passthru | Pure set operation |
| `IntegerBitSet.complement` | passthru | Pure set operation |
| `ArrayBitSet.init` | passthru | Pure initialization |
| `ArrayBitSet.set` | passthru | Pure modification |
| `ArrayBitSet.unset` | passthru | Pure modification |
| `ArrayBitSet.toggle` | passthru | Pure modification |
| `ArrayBitSet.isSet` | passthru | Pure predicate |
| `ArrayBitSet.count` | passthru | Pure query |
| `ArrayBitSet.iterator` | passthru | Returns iterator |
| `DynamicBitSet.init` | passthru | Takes Allocator, tracking via interface |
| `DynamicBitSet.deinit` | passthru | Uses Allocator.free (shimmed) |
| `DynamicBitSet.resize` | passthru | Uses Allocator.realloc (shimmed) |
| `DynamicBitSet.clone` | passthru | Uses Allocator.alloc (shimmed) |
| `DynamicBitSet.set` | passthru | Pure modification |
| `DynamicBitSet.unset` | passthru | Pure modification |
| `DynamicBitSet.isSet` | passthru | Pure predicate |
| `DynamicBitSet.count` | passthru | Pure query |
| `DynamicBitSet.iterator` | passthru | Returns iterator |

---

## std.hash

Non-cryptographic hash functions.

| Function | Category | Reason |
|----------|----------|--------|
| `Wyhash.hash` | passthru | Pure hashing |
| `Wyhash.init` | passthru | Pure initialization |
| `Wyhash.update` | passthru | Pure state update |
| `Wyhash.final` | passthru | Pure finalization |
| `CityHash.hash` | passthru | Pure hashing |
| `Murmur.hash` | passthru | Pure hashing |
| `Fnv.hash` | passthru | Pure hashing |
| `XxHash.hash` | passthru | Pure hashing |
| `XxHash.init` | passthru | Pure initialization |
| `XxHash.update` | passthru | Pure state update |
| `XxHash.final` | passthru | Pure finalization |
| `Crc32.*` | passthru | Pure CRC |
| `Adler32.*` | passthru | Pure checksum |
| `autoHash` | passthru | Pure hashing |
| `autoHashStrat` | passthru | Pure hashing |

---

## Summary

### Minimal Shim Set

The stdlib requires only a small set of "lifecycle primitive" shims. Everything else is passthru.

**Memory Lifecycle (implemented):**
- `std.mem.Allocator.*` - alloc/free/realloc/etc.
- `std.heap.ArenaAllocator.init/deinit` - arena lifecycle

**FD Lifecycle (planned):**
- `posix.open/openat` - creates fd
- `posix.close` - destroys fd
- `posix.socket/accept` - creates fd
- `posix.pipe/pipe2` - creates fds
- `posix.dup/dup2` - creates fd
- `posix.epoll_create` - creates fd

**Mutex Lifecycle (planned):**
- `std.Thread.Mutex.*` - lock/unlock pairing
- `std.Thread.RwLock.*` - lock/unlock pairing

### Everything Else → Passthru

- **Collections** (ArrayList, HashMap, etc.) - use Allocator internally
- **File I/O** (std.fs, std.io) - use posix fd primitives
- **Networking** (std.net, std.http) - use posix socket primitives
- **Compression/JSON/Crypto** - use Allocator internally
- **Pure functions** (math, meta, fmt, etc.) - no side effects

### Key Insights

- **Allocator pattern**: Any function using `Allocator` is passthru - tracking happens at interface level
- **FD pattern**: Any function using posix fd operations is passthru - tracking happens at open/close
- **Ignored**: Debug, log, testing functions have no memory safety impact
- **Forbidden**: Internal vtable implementations, concurrency primitives, inline assembly
