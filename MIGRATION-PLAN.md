# Pseudo-Opcode Migration Plan

## Migration Cycle

For each opcode:
1. **Ablate** - Remove custom opcode detection/emission from `src/codegen.zig`
2. **Implement** - Add FQN pattern matching in `lib/analysis/memory_safety.zig` `call()` function
3. **Test/Bugfix** - Run targeted tests, fix any issues
4. **Full Integration** - Run `./run_integration.sh`
5. **Commit** - Commit changes
6. **Goto 1** - Next opcode

## Opcodes to Migrate

| # | Opcode | FQN Pattern | Status |
|---|--------|-------------|--------|
| 1 | `AllocCreate` | `mem.Allocator.create` | Pending |
| 2 | `AllocDestroy` | `mem.Allocator.destroy` | Pending |
| 3 | `AllocAlloc` | `mem.Allocator.alloc`, `alignedAlloc`, `dupe`, `dupeZ` | Pending |
| 4 | `AllocFree` | `mem.Allocator.free` | Pending |
| 5 | `AllocResize` | `mem.Allocator.resize` | Pending |
| 6 | `AllocRealloc` | `mem.Allocator.realloc`, `remap` | Pending |
| 7 | `MkAllocator` | Return type is `std.mem.Allocator` | Pending |
| 8 | `ArenaInit` | `ArenaAllocator.init` | Pending |
| 9 | `ArenaDeinit` | `ArenaAllocator.deinit` | Pending |

## NOT Migrating

- `noop_debug` - Keep as-is per user request

## Files to Modify

### Per-opcode changes:
- `src/codegen.zig` - Remove `isXxx()` check and payload emission
- `lib/analysis/memory_safety.zig` - Add pattern match in `call()` function

### Final cleanup (after all opcodes migrated):
- `lib/tag.zig` - Remove pseudo-opcode struct definitions from `AnyTag` union
- `src/codegen.zig` - Remove `isXxxFqn()` helper functions
- `lib/Inst.zig` - Remove `splatShim()` (replaced by `splatCall()`)
- `lib/Analyte.zig` - Remove `hasShim()` function

## Current Progress

- [x] Infrastructure: `call()` stubs in all analysis modules
- [x] Infrastructure: `splatCall()` dispatcher in `Inst.zig`
- [x] Infrastructure: `Inst.call` uses runtime FQN
- [ ] Opcode 1: AllocCreate
- [ ] Opcode 2: AllocDestroy
- [ ] Opcode 3: AllocAlloc
- [ ] Opcode 4: AllocFree
- [ ] Opcode 5: AllocResize
- [ ] Opcode 6: AllocRealloc
- [ ] Opcode 7: MkAllocator
- [ ] Opcode 8: ArenaInit
- [ ] Opcode 9: ArenaDeinit
- [ ] Final cleanup
