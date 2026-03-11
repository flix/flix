# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Flix is a statically typed functional, imperative, and logic programming language. This repository contains the compiler, standard library, and LSP server, all written in Scala (compiler) and Flix (standard library).

## Build Commands

Build tool is **Mill** (version pinned in `.mill-version`). JVM heap is set to 4GB in `.mill-jvm-opts`.

```bash
./mill flix.compile              # Compile the compiler
./mill flix.test                 # Run all tests (ScalaTest)
./mill flix.assembly             # Build fat JAR
./mill flix.test.testOnly <fully.qualified.SuiteName>  # Run a single test suite
```

Specialized test tasks:
```bash
./mill flix.testPackageManager   # Package manager tests
./mill flix.testFuzzerSuite      # Fuzzer tests
./mill flix.testIDECompletion    # IDE completion tests
```

Requires **Java 21**.

## Architecture

### Compiler Pipeline

The compiler has two main phases: `check()` (type checking) and `codeGen()` (code generation).

**check() pipeline** (in `Flix.scala`):
Reader → Lexer → Parser2 → Weeder2 → Desugar → Namer → Resolver → Kinder → Deriver → Typer → EntryPoints → Instances → PredDeps → Stratifier → PatMatch2 → Redundancy → Safety → Terminator → Dependencies

**codeGen() pipeline** (in `Flix.scala`):
TreeShaker1 → Specialization → LambdaDrop → Optimizer → Simplifier → ClosureConv → LambdaLift → TreeShaker2 → EffectBinder → TailPos → Eraser → Reducer → JvmBackend → JvmWriter → JvmLoader

Each phase transforms one AST representation to another. AST types are defined in `main/src/ca/uwaterloo/flix/language/ast/`. Phases live in `main/src/ca/uwaterloo/flix/language/phase/`.

### Key Directories

- `main/src/ca/uwaterloo/flix/api/` — Public API (`Flix.scala`) and LSP server
- `main/src/ca/uwaterloo/flix/language/phase/` — All compiler phases
- `main/src/ca/uwaterloo/flix/language/phase/jvm/` — JVM bytecode generation
- `main/src/ca/uwaterloo/flix/language/phase/unification/` — Type unification engines
- `main/src/ca/uwaterloo/flix/language/ast/` — AST definitions (82 files)
- `main/src/ca/uwaterloo/flix/language/errors/` — Compiler error types
- `main/src/library/` — Standard library (.flix files)
- `main/test/ca/uwaterloo/flix/` — Scala test suites
- `main/test/flix/` — Flix language tests (Test.*.flix)

### Incremental Compilation

The compiler supports incremental compilation via `ChangeSet` tracking. Many phases accept cached ASTs and a change set to avoid reprocessing unchanged inputs. Cached state is stored as fields on the `Flix` object.

## Code Style

### Scala
- 2-space indentation
- No shadowed variables, no unused local variables
- Prefer `mapN` over `for ... yield` with `Validation`
- Avoid inheritance; prefer algebraic data types and functions
- Think towards self-hosting: avoid Scala features that can't be ported to Flix
- Common method names: `visitExp`, `visitExps`, `visitPat`
- Name expressions `exp1`, `exp2`, `exp3` (not `beginExp`, `endExp`)
- Long variable names abbreviated: `eff`, `tparam`; constructors generally not: `Effect`, `TypeParam`

### Flix
- 4-space indentation
- Doc comments use `///`
- Trait instance order: Eq, Order, ToString
- Argument lists should have the subject last (to support `|>`)
- Avoid casts (effect casts OK if necessary)
- Pattern matches should align `=>`
- Variable names: `o` for Option, `l` for List; type vars: `a`, `b`, `c`; effect vars: `ef`, `ef1`, `ef2`

### General
- Every file must start with a copyright header
- Prefer functional over imperative (local mutability OK)
- Make functions/methods private when possible

## Commit Messages

Use [semantic commit messages](https://gist.github.com/joshbuchea/6f47e86d2510bce28f8e7f42ae84c716): `feat`, `fix`, `docs`, `style`, `refactor`, `test`, `chore`. Summary should be a single clear sentence, all lowercase except proper names.

## JVM Bytecode Generation Policy

- Prefer public fields over private fields with getter/setter
- Prefer direct field initialization over constructor arguments
- Ensure classes are final
