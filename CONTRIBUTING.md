# daul - Contributing Documentation

This document serves as a general outline of the codebase's structure, and the development process associated with it.

## Guidelines

No code in the repo should generate warnings or errors from the [selene linter](https://kampfkarren.github.io/selene/selene.html).

Files should remain relatively small, or at least avoid becoming gigantic. Split code into reasonably-sized files, as needed.

## Compiler outline

The `common` directory contains common utilities like `enum` and error reporting, common to multiple stages of the compiler.

The parser frontend, in the `parse` directory, is the first phase to compilation. This part is extremely conventional for a compiler:
 - `lexer.lua` is the tokenizer, which transforms the raw source code into a sequence of tokens for use by the parser.
 - `parser.lua` is the logic of the pratt parser that transforms the sequence of tokens into a structured AST.
 - `grammar.lua` contains the rules used by the pratt parser to parse daul code.

The `ir` directory is the second phase of the compiler:
 - `compile.lua` is the compiler, transforming the AST into IR.
 - `optimize.lua` contains IR to IR optimizations.

The bytecode emitter backends, in the `emit` directory, are the last phase of the compiler. Any of the backends may be used, each of them transforming IR into backend-dependant bytecode. The `base.lua` file contains the base code relevant to all bytecode emitters.

The first two phases are daul-specific, but any compiler could write a different parser, reimplement the IR compiler interface and use the same bytecode emitters.

## Other documentation

A full reference table of the instructions used in the IR [is available](https://github.com/TechnoJo4/luadaul/wiki/IR-Reference) on the GitHub wiki.

In order to understand and/or contribute to the bytecode-emitting backends, you may want to read:
 - A No-Frills Introduction to Lua 5.1 VM Instructions
 - The [bytecode page](https://wiki.luajit.org/Bytecode-2.0) on LuaJIT wiki
 - [Lua 5.1](https://www.lua.org/source/5.1/) and [LuaJIT](https://github.com/LuaJIT/LuaJIT) source code
