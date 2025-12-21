# Code Review Checklist

## Refactor and rename Payload
- [x] Decide on new names for Payloads, TypedPayload, Analyte, EIdx
  - Payloads → Refinements
  - TypedPayload → Refinement
  - Slot → Inst
  - tracked → results
  - slots.zig removed (functions moved to Inst.zig)
- [x] Rename across codebase

## Review Inst.zig
- [ ] backPropagate function
- [ ] Slot/Payload copying functions (now in Inst.zig and Refinements.zig)

## Review tag.zig types
- [ ] tag.Alloc
- [ ] tag.AllocCreate
- [ ] tag.AllocDestroy
- [ ] tag.Arg
- [ ] tag.Bitcast
- [ ] tag.Br
- [ ] tag.DbgStmt
- [ ] tag.DbgVarPtr
- [ ] tag.Load
- [ ] tag.OptionalPayload
- [ ] tag.RetSafe
- [ ] tag.StoreSafe
- [ ] tag.UnwrapErrunionPayload
- [ ] tag.AnyTag

## Review analysis/undefined.zig operators
- [ ] undefined.alloc
- [ ] undefined.alloc_create
- [ ] undefined.store_safe
- [ ] undefined.load
- [ ] undefined.dbg_var_ptr

## Review analysis/memory_safety.zig operators
- [ ] memory_safety.alloc
- [ ] memory_safety.arg
- [ ] memory_safety.store_safe
- [ ] memory_safety.dbg_var_ptr
- [ ] memory_safety.bitcast
- [ ] memory_safety.optional_payload
- [ ] memory_safety.ret_safe
- [ ] memory_safety.alloc_create
- [ ] memory_safety.alloc_destroy
- [ ] memory_safety.load

## Other
- [x] Review CLAUDE.md - documentation accuracy (updated for refactoring)
- [x] Dead code - unused imports, stale comments
  - slots.zig removed
  - Renamed: _slotLine→_instLine, buildSlotLines→buildInstLines, extractDestroyPtrSlot→extractDestroyPtrInst
- [ ] Test coverage - unit tests for new code
