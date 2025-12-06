# AIR-OUT for zig

## Summary

  Add a new -fair-out=<path> flag that routes AIR to a dynamically loaded .so plugin for analysis/validation.
  Use of this backend will be triggered by setting ofmt=

## Gameplan

### Command-Line Flag Parsing

- File: src/main.zig
  1. Add help text for the new flag (look for -fllvm):
  -fair-out=<path>          Send AIR to a dynamically loaded analyzer plugin
  2. Add air to -ofmt documentation
  2. Add argument parsing (near -fllvm parsing).

### Compilation Config

- File: src/Compilation/Config.zig
  1. Add `air_out ?[] const u8` field to Config (root) struct (near use_llvm).
  2. Add `air_out` field to Options struct (near use_llvm).
  3. Wire through in resolve() function - assign options.air_out to the config
  // IN THEORY we should build some guards here to make sure that air_out
  // is set iff air is selected in ofmt

### Object Format Enum

- File: lib/std/Target.zig
  1. Add air variant to ObjectFormat enum (keep alphabetical)
  2. Add file extension to fileExt() function.

### Binary name Allocation

- File: lib/std/zig.zig
  1. Add case in binNameAlloc()

### Compiler Backend Enum

- File: lib/std/builtin.zig
  1. Add `stage2_air` variant to CompilerBackend enum.

### Dev Features

- File: src/dev.zig
  1. Add `air_backend` to Feature enum (near llvm_backend):
  2. Add `.air_backend` to supports(...)
  3. repeat with `air_linker`

### Target/Backend Selection

- File: src/target.zig

  1. Update zigBackend() function (check target.ofmt)
  2. Update hasLlvmSupport to reject llvm.
  3. Update backendSupportsFeature() if needed (around line 844) - decide what features the AIR backend claims to support

### Create Codegen Module

-  File: src/codegen/air.zig (new file)
  - copy src/codegen/c.zig and drop in empty
    implementations of `legalizeFeatures` and `generate`
  - empty Mir and Function structs.
  - In generate():
    const air_out_path = lf.comp.config.air_out orelse unreachable;
    // Use std.DynLib to load the .so

### Wire Codegen Module

- File: src/codegen.zig
  1. Add to devFeatureForBackend():
  .stage2_air => .air_backend,
  2. Add to importBackend():
  .stage2_air => @import("codegen/air.zig"),
  3. Add to legalizeFeatures() switch:
  .stage2_air,
  4. Add to AnyMir union:
  air_backend: @import("codegen/air.zig").Mir,
  5. Add to AnyMir.tag():
  .stage2_air => "air_backend",
  6. Add to AnyMir.deinit() switch:
  .stage2_air,
  7. Add to generateFunction() switch:
  .stage2_air,

### Linker Integration

- File src/link/Air.zig (new file):
  - copy src/link/C.zig:
  - substitute for:
    - open(...)
    - create(...)
  - no-op empty:
    - deinit()
    - updateFunc
    - updateNav
    - updateLineNumber
    - flush
    - updateExports
    - deleteExport
  - make sure the .tag is .air 

### Wire Linker Module

- File: src/link.zig

  1. Add import `pub const AirBackend = @import("link/Air.zig");`
  2. Add to File.Tag enum:
  air_backend,
  3. Add to Tag.Type():
  .air_backend => AirBackend,
  4. Add to Tag.devFeature() if it exists
  5. For functions that dispatch on tag, add air_backend cases:
    - Some will be unreachable
    - Some will need actual handling
    - Review each switch on base.tag and decide

### Compiation Config
- File: src/Compilation/Config.zig
  add to debug format resolution .strip.

### Calling Convention Support
- File: src/Zcu.zig

  1. Update callconvSupported() (around line 4469):
  .stage2_air => true,  // Accept all calling conventions

### Posix Compatibility
- File: lib/std/posix.zig

  1. Update dl_iterate_phdr()