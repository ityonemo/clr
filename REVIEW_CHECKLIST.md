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
- [x] tag.Alloc
- [x] tag.AllocCreate
- [x] tag.AllocDestroy
- [x] tag.Arg
- [x] tag.Bitcast
- [x] tag.Br
- [x] tag.DbgStmt
- [x] tag.DbgVarPtr
- [x] tag.DbgVarVal (split from DbgVarPtr - values don't need pointer tracking)
- [x] tag.Load
- [x] tag.OptionalPayload
- [x] tag.RetSafe
- [x] tag.Store (renamed from StoreSafe, used for both store and store_safe)
- [x] tag.UnwrapErrunionPayload
- [x] tag.AnyTag

## Review analysis/undefined.zig operators
- [x] undefined.alloc
- [x] undefined.alloc_create
- [x] undefined.store
- [x] undefined.load
- [x] undefined.dbg_var_ptr

## Review analysis/memory_safety.zig operators
- [x] memory_safety.alloc
- [x] memory_safety.store (handles parameter name/location propagation)
- [x] memory_safety.dbg_var_ptr
- [x] memory_safety.bitcast (removed - entities shared intraprocedrually)
- [x] memory_safety.optional_payload (removed - entities shared intraprocedrually)
- [x] memory_safety.ret_safe (refactored to flat control flow)
- [x] memory_safety.alloc_create (moved to pointer's analyte)
- [x] memory_safety.alloc_destroy (simplified, removed origin tracking)
- [x] memory_safety.load (moved to pointer's analyte)
- [x] memory_safety.onFinish (refactored, checks pointer's analyte)

## Other
- [x] Review CLAUDE.md - documentation accuracy (updated for refactoring)
- [x] Dead code - unused imports, stale comments
  - slots.zig removed
  - Renamed: _slotLine→_instLine, buildSlotLines→buildInstLines, extractDestroyPtrSlot→extractDestroyPtrInst
- [ ] Test coverage - unit tests for new code
