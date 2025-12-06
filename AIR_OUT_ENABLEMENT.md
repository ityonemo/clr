# AIR-OUT for zig

## Summary

  Add a new -fair-out=<path> flag that routes AIR to a dynamically loaded .so plugin for analysis/validation.

## Gameplan

### Command-Line Flag Parsing

- File: src/main.zig
  1. Add help text for the new flag (look for -fllvm):
  -fair-out=<path>          Send AIR to a dynamically loaded analyzer plugin
  -fno-air-out              (default) Do not use AIR analyzer plugin
  2. Add storage variable for the path (find where use_llvm is stored and add nearby)
  3. Add argument parsing (near -fllvm parsing):
  4. Pass air_out_path to Compilation.Config.Options

### Compilation Config

- File: src/Compilation/Config.zig
  1. Add `air_out ?[] const u8` field to Config (root) struct (near use_llvm).
  2. Add `air_out` field to Options struct (near use_llvm).
  3. Wire through in resolve() function - assign options.air_out to the config

### Compiler Backend Enum

- File: lib/std/builtin.zig
  1. Add `stage2_air` variant to CompilerBackend enum.

### Dev Features

- File: src/dev.zig
  1. Add `air_backend` to Feature enum (near llvm_backend):
  2. Add `.air_backend` to supports(...)

### Target/Backend Selection

- File: src/target.zig

  1. Update zigBackend() function:
      if (config.air_out != null) return .stage2_air;

  1. Note: This requires changing the function signature. Find all call sites and update them.
  2. Update backendSupportsFeature() if needed (around line 844) - decide what features the AIR backend claims to support

### Create Codegen Module

-  File: src/codegen/air.zig (new file)
  - copy src/codegen/c.zig and drop in empty
    implementations of `legalizeFeatures` and `generate`
  - empty Mir and Function structs.
  - In generate():
    const air_out_path = lf.base.comp.config.air_out orelse unreachable;
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

### Calling Convention Support
- File: src/Zcu.zig

  1. Update callconvSupported() (around line 4469):
  .stage2_air => true,  // Accept all calling conventions