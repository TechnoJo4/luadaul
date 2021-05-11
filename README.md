# daul

<p align="center">
  <img width="256px" src="logo/daul.png"/>
</p>

daul is a language that compiles to LuaJIT bytecode.

No external dependencies.

Some examples are available in the `examples` directory.

Current State: Lots of TODOs, missing important Lua features, a bit of spaghetti, and probably bugs.

## Roadmap

Most of these goals are very long-term, but this should serve as an idea of what I strive to achieve with daul:
  - [ ] All Lua features
  - [ ] Complete LuaJIT bytecode backend
  - [ ] Inline functions
  - [ ] Self-hosting
  - [ ] Well-documented codebase
  - [ ] Type checking
  - [ ] Custom operators
  - [ ] Macros

Missing Lua(JIT) features:
  - [ ] Creation of tables (workaround: `loadstring("return {}")()`)
  - [ ] Multi-value operations
    - [ ] Varargs `...`
    - [ ] Multi-assingments `a, b = b, a`
    - [ ] Multiple returns `return a, b`
    - [ ] Other
  - [ ] LuaJIT: FFI (?)
  - [ ] LuaJIT: cdata constants
  - [ ] LuaJIT: Optimizations (e.g. template tables/TDUP)
