Clr.Air.start_link([])

Clr.set_checkers([
  Clr.Analysis.Undefined,
  Clr.Analysis.StackPointer,
  Clr.Analysis.Allocator
])

Clr.Zig.Parser.start_link([])

ExUnit.start()
