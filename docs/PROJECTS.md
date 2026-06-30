# Project Proposals

Here is a list of projects that are currently open for exploration.

If you are interested in working on any of these projects, feel free to reach
out to us on Gitter. If you are a student at Aarhus University you can reach
out to Magnus directly.

## Language & Type System

### Fluent APIs (issue 6625)

Many libraries expose *fluent* or *protocol* APIs in which methods must be
called in a particular order — for instance, a resource must be opened before it
is read and closed before it is reused. Most languages cannot enforce such
protocols statically, so misuse is only detected at runtime. By encoding the
state of an object at the type level, a sufficiently expressive type system can
make illegal call sequences a compile-time error.

### Type Qualifiers (issue 6528)

Type qualifiers annotate a type with extra information that the type system then
tracks and enforces. Familiar examples include `const` (the value may not be
mutated), `mut` (the value may be mutated), `secret` (the value must not leak),
and `nullable` (the value may be null). Adding a general, composable mechanism
for type qualifiers to Flix would let programmers express and enforce a wide
range of such properties, potentially by desugaring qualified types into an
underlying representation indexed by a type-level qualifier.

### User-Defined Kinds for Sets (issue 4536)

Flix uses kinds to classify types — for example, distinguishing ordinary types
from type-level booleans or effects. It would be useful to let programmers define
their own kinds whose inhabitants form a fixed, enumerated set, such as a kind
`Fruit` with members `Apple` and `Pear`. Such user-defined set kinds could serve
as type-level tags and enable more precise, domain-specific use of the type
system.

### Controlling Variable Capture (issue 4518)

Some language constructs are only safe if the expressions they enclose do not
capture certain kinds of variables. For example, an expression that is spawned on
another thread, or sent over a channel, should arguably not capture mutable data
that could then be accessed concurrently. It is an open question how best to
express and enforce such restrictions, and how the answer relates to the
distinction between capabilities, effects, and effect exclusion.

### Overloaded Functions for Timing Resistance (issue 4530)

Security-critical code, such as a routine that compares two secret strings, must
often be written to be *timing-resistant*: its running time must not depend on
the secret data, lest an attacker learn the secret by measuring how long the
operation takes. The same logical operation therefore comes in two variants: a
fast, ordinary one and a slower, timing-resistant one. Ideally a programmer could
write generic code and have the compiler, guided by the type system, select the
appropriate variant at compile time based on whether the data is secret —
avoiding both runtime dispatch and the risk of choosing wrongly by hand.

## Effects & Handlers

### Abortive and Tail-Resumptive Handlers (issue 12887)

Flix supports algebraic effects and handlers. When a handler invokes its
continuation, the general case requires capturing and reifying the continuation
so that it can be resumed an arbitrary number of times. However, many handlers
use their continuation in much more restricted ways: some never resume at all
(abortive handlers), while others resume exactly once in tail position
(tail-resumptive handlers). These special cases can be compiled far more
efficiently than the general case, avoiding the cost of capturing a first-class
continuation.

### Effect Refactoring (issue 4616)

Adding or removing an effect from a function is rarely a local change. If a
function gains, say, a `Log` effect, then every function that calls it — and
every higher-order function that takes it as an argument — may need to change as
well, either by gaining the effect or by becoming effect-polymorphic. Doing this
by hand is tedious and error-prone, and the right choice is not always obvious,
particularly in the presence of higher-order functions and function composition.

## Datalog & Logic Programming

### Failable Datalog (issue 5463)

Flix integrates first-class Datalog constraints into a general-purpose
functional language. Standard Datalog computes a least fixed point and always
succeeds, but many realistic problems involve operations that may fail — for
example, partial functions applied within rules. A model of *failable* Datalog
would give a principled account of how failure of various kinds propagates
through Datalog computation.

### Widening with Datalog Semantics (issue 5388)

Static analyses are often expressed as fixed-point computations over a lattice,
and Flix's first-class Datalog constraints with lattice semantics are well
suited to expressing such analyses. To guarantee termination over
infinite-height lattices, abstract interpreters use a *widening* operator that
accelerates convergence. It is an open question how widening can be expressed
naturally within Datalog semantics — for example, by combining the old and new
values of a lattice element within a rule.

### Structural Relations (issue 3127)

In Flix's Datalog, relations are referred to by a fixed schema. A more
*structural* treatment would let rules mention only the fields they care about
and ignore the rest — so that, for example, an `Edge` relation carrying an extra
`label` field could still be used by a rule that only mentions `src` and `dst`,
while a rule that derives a fact must still supply every field that fact
requires. This is reminiscent of row polymorphism applied to Datalog predicates.

## Backend & Code Generation

### Tail Recursion Modulo Cons (issue 1228)

Tail Recursion Modulo Cons (TRMC) is an essential optimization for functional
programming languages that enable very efficient compilation of common functions
such as map and filter. The key idea is the compilation of recursive functions
into imperative while-loops that operate on mutable data (even though the
functional program operates on immutable data).

### Destination-Driven Code Generation (issue 12614)

When the Flix compiler generates JVM bytecode, expression compilation
unconditionally leaves a value on the stack — even when that value is never
used. Because side-effecting, unit-returning operations such as field writes,
array stores, and spawns must satisfy this invariant, they push a `Unit` value
that is immediately popped in statement position. This pattern accounts for a
large number of redundant instruction pairs in the generated bytecode.

Destination-driven code generation addresses this by passing a *destination* to
the code generator that describes what the caller intends to do with the result:
leave it on the stack, discard it, or use it to branch. With this information
the compiler can avoid materializing unused values, and can compile boolean
conditions directly into jumps rather than producing a boolean and then testing
it.

### Compilation to WebAssembly (issue 500)

WebAssembly is a portable binary instruction format designed to run at
near-native speed inside a memory-safe, sandboxed execution environment — most
prominently in web browsers. Flix currently targets the JVM, and exploring a
WebAssembly backend would yield valuable insight into both WebAssembly and the
degree to which the compiler's JVM-specific features are properly abstracted. The
goal is not to replace the JVM backend, but to produce an experimental backend
capable of compiling and running small Flix programs in the browser.

A key technical challenge is that WebAssembly does not provide a garbage
collector, which complicates running larger programs, and that Flix's JVM-specific
features must first be cleanly encapsulated behind suitable abstractions.

### Stable JVM API (Facade) (issue 2359)

Flix compiles to the JVM and can interoperate with Java, but calling Flix code
*from* Java is awkward because Flix functions are often polymorphic and compiled
in ways that do not present a stable, predictable Java signature. Allowing
programmers to *export* selected non-polymorphic functions would let the compiler
generate a stable Java interface — a facade — that Java code can call directly,
making Flix usable as a library from the wider JVM ecosystem.

## Analysis & Metaprogramming

### Meta-Choice Expressions (issue 12418)

Parsing is usually thought of as producing a single abstract syntax tree from a
piece of source code. In some situations, however, a fragment of source code is
genuinely ambiguous and could be parsed in several different ways. A
*meta-choice* expression makes this ambiguity explicit by representing several
candidate parses of the same fragment, deferring the decision of which one is
correct to a later compiler phase that has more context, for example type
information.

### Compile-Time Interpreter (issue 1153)

A compile-time interpreter evaluates a subset of the language during compilation.
Such an interpreter opens the door to compile-time meta-programming: macro
expansion, user-defined checkers, and other forms of computation that run before
code generation. To be usable early in the pipeline, the interpreter should
operate on the typed AST rather than a lowered representation, be restricted to
pure expressions (with the exception of pure JVM interop), and use a step counter
to guarantee termination.

## Editor & Developer Tooling

### Semantic Grep (issue 11322)

Programmers frequently search their codebases, but the standard tools — such as
`grep` — operate purely on text and have no understanding of program structure
or semantics. A *semantic* search command would instead let programmers query
their code based on its meaning: for example, finding all pure functions, all
functions with a particular signature, or all uses of a given definition.

### Auto-Completion as a Service (issue 4579)

Editor features such as auto-completion and redundancy checking are usually
hard-wired into the compiler or language server. An alternative is to make them
*extensible*, so that libraries can define their own completions and checks —
for example, through macros — and have the editor surface them. This raises
interesting questions about the "algebra" of completions: how they compose, and
how a combinator library might expose the abstract syntax tree to drive them.

### VSCode Testing API Integration (issue 4502)

Flix ships with a Visual Studio Code extension that provides a rich editing
experience backed by the compiler. VSCode exposes a dedicated Testing API that
lets extensions surface a project's tests in the editor's test explorer, run
them, and report their results inline. Integrating Flix's unit tests with this
API would give Flix programmers a first-class testing experience inside their
editor.

## Testing & Benchmarking

### QuickCheck

QuickCheck frameworks are testing tools used in software development to perform
property-based testing. Instead of writing specific test cases, developers
define properties that their code should satisfy. These properties are then
tested against a large number of automatically generated random inputs.
QuickCheck frameworks generate a wide range of test inputs to explore various
edge cases and potential issues in the code. If a property fails for a
particular input, the framework automatically simplifies the input to find the
simplest case that causes the failure. This helps developers identify and fix
bugs in their code efficiently.

### Compiler Fuzzing

Compilers are large and complex pieces of software. The correctness of a
compiler is paramount: A compiler that silent mis-compiles (i.e. wrongly
translates) a program is dangerous: We cannot trust the programs we compile and
run! We can test compilers by writing unit tests, but unfortunately such tests
tend to only test the 'happy path' of the compiler. Moreover, the number of unit
tests that can be written is limited. Instead, compiler fuzzing techniques have
been proposed. A compiler fuzzer typically takes a test suite as input and subtly
changes the programs in a systematic fashion and then re-compiles the test
suite. This process is fully automatic and can be run for hours. Often such
techniques, with suitably clever 'mutation strategies', are able to find
significantly more bugs than those found by unit testing.

### Mutation Testing (issue 6095)

A test suite is only as good as its ability to detect bugs. Mutation testing
measures this directly: it systematically introduces small changes ("mutants")
into a program — for example, flipping a comparison or replacing a constant —
and then runs the test suite against each mutant. A mutant that is *not* caught
by any test reveals a gap in the suite. Because Flix has a rich, typed AST,
mutations can be generated in a principled, type-directed way.

## Libraries & Applications

### Reactive MVC Module (issue 7269)

Modern user-interface frameworks, such as React, are built around a simple but
powerful idea: the programmer describes the application state and how it
transitions, and a runtime is responsible for rendering that state and
re-rendering it whenever it changes. This separation between declarative state
and an imperative, on-demand runtime is a natural fit for a functional language
with an effect system, where the side effects performed during state transitions
can be tracked and controlled.

### Type-Indexed Streams (issue 4533)

Streams are a fundamental abstraction for working with sequences of values,
including potentially infinite ones. By indexing a stream type with type-level
information — such as whether the stream is finite or infinite, and whether it is
evaluated eagerly or lazily — the type system can rule out nonsensical operations
at compile time, for example taking the last element of an infinite stream, while
still allowing generic operations such as `map`, `zip`, and `flatMap` to be
expressed over all streams.
