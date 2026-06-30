# Project Proposals

Here is a list of projects that are currently open for exploration.

If you are interested in working on any of these projects, feel free to reach
out to us on Gitter. If you are a bachelor or master student at Aarhus
University you can reach out to Magnus directly.

## QuickCheck

QuickCheck frameworks are testing tools used in software development to perform
property-based testing. Instead of writing specific test cases, developers
define properties that their code should satisfy. These properties are then
tested against a large number of automatically generated random inputs.
QuickCheck frameworks generate a wide range of test inputs to explore various
edge cases and potential issues in the code. If a property fails for a
particular input, the framework automatically simplifies the input to find the
simplest case that causes the failure. This helps developers identify and fix
bugs in their code efficiently.

The aim of this project is to (1) explore the design space of QuickCheck
frameworks and to (2) design and implement a QuickChecker for the Flix
programming language. The work will include reading papers, language design, and
implementation in a real-world programming language.

## Package Management

Today, programs are rarely written from scratch, but rather build on a large
collection of external libraries. Different languages have different package
ecosystems: Java has Maven, JavaScript has NPM, Rust has Cargo, and so forth.
All of these languages offer some form of package manager that is used to
download, install, upgrade, and keep track of the dependencies of a software
project. The problem is non-trivial: For example, how should we handle the
situation where a project depends on package A and package B, and A depends on C
(version 1.0), but B depends on C (version 2.0)?

The aim of this project is to (1) explore the design space of package managers
for programming languages, and to (2) design and implement a fully-featured
package manager for the Flix programming language. The work will include reading
papers, language design, and implementation in a real-world programming language.

## Compiler Fuzzing

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

The aim of this project is to: (1) explore the design space of compiler fuzzing
techniques, and to (2) design and implement such a system for the Flix
programming language. The work will include reading papers, language design, and
implementation in a real-world programming language.

## Code Formatting

A significant part of compiler implementation focuses on the parser: the
compiler phase that turns source code text into abstract syntax trees. However,
the opposite direction is often forgotten: the code formatter that turns
abstract syntax trees back into neatly formatted source code text. Today,
programming languages like Go, IDEs like Intellij IDEA, and linters all come
with built-in support for code formatting.

The aim of this project is to: (1) explore the design space of code formatting
techniques, and to (2) design and implement such a system for the Flix
programming language. The work will include reading papers, language design, and
implementation in a real-world programming language.

## Tail Recursion Modulo Cons

Tail Recursion Modulo Cons (TRMC) is an essential optimization for functional
programming languages that enable very efficient compilation of common functions
such as map and filter. The key idea is the compilation of recursive functions
into imperative while-loops that operate on mutable data (even though the
functional program operates on immutable data).

The aim of this project is to: (1) understand tail recursion modulo cons, and to
(2) implement it in the Flix programming language. The work will include reading
papers, language design, and implementation in a real-world programming
language.

## Termination Analysis

A common programming mistake is to write an infinite loop. Unfortunately, most
contemporary programming languages, such as C, C++, C#, Java, Kotlin, and Scala,
do not help programmers avoid such issues. Termination analysis describes a wide
range of techniques that can verify that a program (or part of a program) always
terminates. For example, by checking that recursive calls always operate on
structurally smaller elements.

The aim of this project is to: (1) explore algorithms for automatic termination
checking, and to (2) implement one of them in the Flix programming language. The
work will include reading papers, language design, and implementation in a
real-world programming language.

## Abortive and Tail-Resumptive Handlers

Flix supports algebraic effects and handlers. When a handler invokes its
continuation, the general case requires capturing and reifying the continuation
so that it can be resumed an arbitrary number of times. However, many handlers
use their continuation in much more restricted ways: some never resume at all
(abortive handlers), while others resume exactly once in tail position
(tail-resumptive handlers). These special cases can be compiled far more
efficiently than the general case, avoiding the cost of capturing a first-class
continuation.

The aim of this project is to: (1) statically classify each handler as abortive,
tail-resumptive, or general by analyzing how the continuation is used in the
handler body, and to (2) implement specialized, more efficient compilation
strategies for the abortive and tail-resumptive cases. The work will include
reading papers, language design, and implementation in a real-world programming
language.

## Effect Handler Benchmarks

The Flix compiler ships with a suite of benchmarks used to track its performance
over time. At present these benchmarks make little or no use of algebraic
effects and handlers, even though effect handling is a central feature of the
language and a part of the compiler with interesting performance
characteristics. Without representative benchmarks it is difficult to measure
the impact of changes to the effect compilation strategy or to compare Flix
against other languages with effect handlers.

The aim of this project is to: (1) survey existing effect-handler benchmark
suites and the patterns they exercise, and to (2) design and implement a set of
effect-handler benchmarks for Flix covering common handler patterns. The work
will include reading about effect handlers, benchmark design, and
implementation and empirical evaluation in a real-world programming language.

## Destination-Driven Code Generation

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

The aim of this project is to: (1) understand destination-driven code generation
and its interaction with the effect-based calling convention, and to (2)
implement it in the Flix bytecode backend, starting with the discard destination
and extending to branch destinations. The work will include reading papers,
compiler design, and implementation in a real-world programming language.

## Meta-Choice Expressions

Parsing is usually thought of as producing a single abstract syntax tree from a
piece of source code. In some situations, however, a fragment of source code is
genuinely ambiguous and could be parsed in several different ways. A
*meta-choice* expression makes this ambiguity explicit by representing several
candidate parses of the same fragment, deferring the decision of which one is
correct to a later compiler phase that has more context, for example type
information.

The aim of this project is to: (1) explore the design space of ambiguity-
tolerant parsing and meta-choice representations, and to (2) design and
implement meta-choice expressions in the Flix compiler. The work will include
reading papers, language design, and implementation in a real-world programming
language.

## Semantic Grep

Programmers frequently search their codebases, but the standard tools — such as
`grep` — operate purely on text and have no understanding of program structure
or semantics. A *semantic* search command would instead let programmers query
their code based on its meaning: for example, finding all pure functions, all
functions with a particular signature, or all uses of a given definition.

The aim of this project is to: (1) explore the design space of semantic code
search and the query algebra it requires, such as filters for purity, argument
structure, and uses-of a definition, and to (2) design and implement a semantic
`grep` command for the Flix compiler. The work will include language design and
implementation in a real-world programming language.

## Reactive MVC Module

Modern user-interface frameworks, such as React, are built around a simple but
powerful idea: the programmer describes the application state and how it
transitions, and a runtime is responsible for rendering that state and
re-rendering it whenever it changes. This separation between declarative state
and an imperative, on-demand runtime is a natural fit for a functional language
with an effect system, where the side effects performed during state transitions
can be tracked and controlled.

The aim of this project is to: (1) design a reactive model-view-controller
module in which the user defines the application state and its transitions while
the library provides a runtime that evaluates those transitions on demand and
renders the result, and to (2) implement a proof of concept — for example, a
minimal DOM of text fields and buttons rendered with Swing, driven by a small
set of effects. The work will involve functional programming, language design,
and implementation in a real-world programming language.

## Fluent APIs

Many libraries expose *fluent* or *protocol* APIs in which methods must be
called in a particular order — for instance, a resource must be opened before it
is read and closed before it is reused. Most languages cannot enforce such
protocols statically, so misuse is only detected at runtime. By encoding the
state of an object at the type level, a sufficiently expressive type system can
make illegal call sequences a compile-time error.

The aim of this project is to: (1) explore the design space of typestate and
fluent APIs, drawing on existing work in the literature, and to (2) design and
implement language support that allows such protocol-enforcing APIs to be
expressed in Flix, for example by indexing types with type-level booleans that
track object state. The work will include reading papers, language design, and
implementation in a real-world programming language.

## Type Qualifiers

Type qualifiers annotate a type with extra information that the type system then
tracks and enforces. Familiar examples include `const` (the value may not be
mutated), `mut` (the value may be mutated), `secret` (the value must not leak),
and `nullable` (the value may be null). Adding a general, composable mechanism
for type qualifiers to Flix would let programmers express and enforce a wide
range of such properties, potentially by desugaring qualified types into an
underlying representation indexed by a type-level qualifier.

The aim of this project is to: (1) explore the design space of type qualifiers,
how they compose, and how they interact with type classes, and to (2) design and
implement syntax and checking for type qualifiers in the Flix programming
language. The work will include reading papers, language design, and
implementation in a real-world programming language.

## Mutation Testing

A test suite is only as good as its ability to detect bugs. Mutation testing
measures this directly: it systematically introduces small changes ("mutants")
into a program — for example, flipping a comparison or replacing a constant —
and then runs the test suite against each mutant. A mutant that is *not* caught
by any test reveals a gap in the suite. Because Flix has a rich, typed AST,
mutations can be generated in a principled, type-directed way.

The aim of this project is to: (1) explore the design space of mutation testing,
including which AST to mutate and which mutation operators to start with, and to
(2) design and implement a mutation-testing command for the Flix compiler that
repeatedly mutates a program, runs the test suite, and reports the surviving
mutants. The work will include reading papers, language design, and
implementation in a real-world programming language.

## Failable Datalog

Flix integrates first-class Datalog constraints into a general-purpose
functional language. Standard Datalog computes a least fixed point and always
succeeds, but many realistic problems involve operations that may fail — for
example, partial functions applied within rules. A model of *failable* Datalog
would give a principled account of how failure of various kinds propagates
through Datalog computation.

The aim of this project is to: (1) develop a model of Datalog that supports
failure and to characterize the different kinds of failure that can arise, and
to (2) design and implement support for failable Datalog in the Flix programming
language. The work will include reading papers, language design, and
implementation in a real-world programming language.

## Widening with Datalog Semantics

Static analyses are often expressed as fixed-point computations over a lattice,
and Flix's first-class Datalog constraints with lattice semantics are well
suited to expressing such analyses. To guarantee termination over
infinite-height lattices, abstract interpreters use a *widening* operator that
accelerates convergence. It is an open question how widening can be expressed
naturally within Datalog semantics — for example, by combining the old and new
values of a lattice element within a rule.

The aim of this project is to: (1) investigate how widening can be expressed in
Datalog with lattice semantics and whether existing mechanisms already suffice,
and to (2) design and implement support for widening in the Flix programming
language. The work will include reading papers, language design, and
implementation in a real-world programming language.

## Effect Refactoring

Adding or removing an effect from a function is rarely a local change. If a
function gains, say, a `Log` effect, then every function that calls it — and
every higher-order function that takes it as an argument — may need to change as
well, either by gaining the effect or by becoming effect-polymorphic. Doing this
by hand is tedious and error-prone, and the right choice is not always obvious,
particularly in the presence of higher-order functions and function composition.

The aim of this project is to: (1) study how effects propagate through programs
under common refactorings and characterize the design choices involved — for
example, when to add a concrete effect versus when to generalize to an
effect-polymorphic signature — and to (2) design and implement automated
effect-refactoring support for the Flix programming language. The work will
include reading papers, language design, and implementation in a real-world
programming language.

## Auto-Completion as a Service

Editor features such as auto-completion and redundancy checking are usually
hard-wired into the compiler or language server. An alternative is to make them
*extensible*, so that libraries can define their own completions and checks —
for example, through macros — and have the editor surface them. This raises
interesting questions about the "algebra" of completions: how they compose, and
how a combinator library might expose the abstract syntax tree to drive them.

The aim of this project is to: (1) explore the design space of extensible,
library-defined auto-completion and redundancy checking, including how
completions compose, and to (2) design and implement such a mechanism for the
Flix programming language. The work will include reading papers, language
design, and implementation in a real-world programming language.

## User-Defined Kinds for Sets

Flix uses kinds to classify types — for example, distinguishing ordinary types
from type-level booleans or effects. It would be useful to let programmers define
their own kinds whose inhabitants form a fixed, enumerated set, such as a kind
`Fruit` with members `Apple` and `Pear`. Such user-defined set kinds could serve
as type-level tags and enable more precise, domain-specific use of the type
system.

The aim of this project is to: (1) explore the design space of user-defined kinds
and the requirements they place on kind inference and checking, and to (2) design
and implement support for user-defined set kinds in the Flix programming
language. The work will include reading papers, language design, and
implementation in a real-world programming language.

## Type-Indexed Streams

Streams are a fundamental abstraction for working with sequences of values,
including potentially infinite ones. By indexing a stream type with type-level
information — such as whether the stream is finite or infinite, and whether it is
evaluated eagerly or lazily — the type system can rule out nonsensical operations
at compile time, for example taking the last element of an infinite stream, while
still allowing generic operations such as `map`, `zip`, and `flatMap` to be
expressed over all streams.

The aim of this project is to: (1) explore the design of type-indexed streams,
including the finiteness and evaluation flags and the generalized algebraic data
types they appear to require, and to (2) implement a stream library in the Flix
programming language that exploits these type indices. The work will include
reading papers, language design, and implementation in a real-world programming
language.

## Overloaded Functions for Timing Resistance

Security-critical code, such as a routine that compares two secret strings, must
often be written to be *timing-resistant*: its running time must not depend on
the secret data, lest an attacker learn the secret by measuring how long the
operation takes. The same logical operation therefore comes in two variants: a
fast, ordinary one and a slower, timing-resistant one. Ideally a programmer could
write generic code and have the compiler, guided by the type system, select the
appropriate variant at compile time based on whether the data is secret —
avoiding both runtime dispatch and the risk of choosing wrongly by hand.

The aim of this project is to: (1) explore how secrecy can be embedded in the
type system and used to drive compile-time specialization between efficient and
timing-resistant implementations, replacing ad-hoc reification with a more
structured form of overloading, and to (2) design and implement such a mechanism
in the Flix programming language. The work will include reading papers, language
design, and implementation in a real-world programming language.

## Controlling Variable Capture

Some language constructs are only safe if the expressions they enclose do not
capture certain kinds of variables. For example, an expression that is spawned on
another thread, or sent over a channel, should arguably not capture mutable data
that could then be accessed concurrently. It is an open question how best to
express and enforce such restrictions, and how the answer relates to the
distinction between capabilities, effects, and effect exclusion.

The aim of this project is to: (1) explore mechanisms for controlling what kinds
of variables an expression may capture — for instance forbidding the capture of
mutable data under `spawn` or channel sends — and to relate these mechanisms to
capabilities and effects, and to (2) design and implement such a mechanism in the
Flix programming language. The work will include reading papers, language design,
and implementation in a real-world programming language.

## VSCode Testing API Integration

Flix ships with a Visual Studio Code extension that provides a rich editing
experience backed by the compiler. VSCode exposes a dedicated Testing API that
lets extensions surface a project's tests in the editor's test explorer, run
them, and report their results inline. Integrating Flix's unit tests with this
API would give Flix programmers a first-class testing experience inside their
editor.

The aim of this project is to: (1) study the VSCode Testing API and how the Flix
compiler and language server expose test information, and to (2) implement
integration between Flix's tests and the VSCode test explorer, including
discovering, running, and reporting tests. The work will involve API design and
implementation in a real-world programming language and editor extension.

## Structural Relations

In Flix's Datalog, relations are referred to by a fixed schema. A more
*structural* treatment would let rules mention only the fields they care about
and ignore the rest — so that, for example, an `Edge` relation carrying an extra
`label` field could still be used by a rule that only mentions `src` and `dst`,
while a rule that derives a fact must still supply every field that fact
requires. This is reminiscent of row polymorphism applied to Datalog predicates.

The aim of this project is to: (1) explore the design space of structural,
row-polymorphic relations for Datalog and the well-formedness rules they require,
and to (2) design and implement structural relations in the Flix programming
language. The work will include reading papers, language design, and
implementation in a real-world programming language.

## Stable JVM API (Facade)

Flix compiles to the JVM and can interoperate with Java, but calling Flix code
*from* Java is awkward because Flix functions are often polymorphic and compiled
in ways that do not present a stable, predictable Java signature. Allowing
programmers to *export* selected non-polymorphic functions would let the compiler
generate a stable Java interface — a facade — that Java code can call directly,
making Flix usable as a library from the wider JVM ecosystem.

The aim of this project is to: (1) specify which functions may be exported and
what stable Java signatures they should generate, and to (2) design and implement
an export mechanism that produces a stable JVM facade in the Flix compiler. The
work will include language design and implementation in a real-world programming
language.

## Compile-Time Interpreter

A compile-time interpreter evaluates a subset of the language during compilation.
Such an interpreter opens the door to compile-time meta-programming: macro
expansion, user-defined checkers, and other forms of computation that run before
code generation. To be usable early in the pipeline, the interpreter should
operate on the typed AST rather than a lowered representation, be restricted to
pure expressions (with the exception of pure JVM interop), and use a step counter
to guarantee termination.

The aim of this project is to: (1) explore the design space of compile-time
interpretation and meta-programming, and to (2) design and implement an
experimental compile-time interpreter for a pure subset of Flix, operating on the
typed AST and usable as the basis for later phases such as macro expansion. The
work will include reading papers, language design, and implementation in a
real-world programming language.

## Compilation to WebAssembly

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

The aim of this project is to: (1) gain insight into WebAssembly and the
abstractions needed to retarget the compiler, and to (2) implement an
experimental WebAssembly backend, on a separate branch, capable of compiling and
running small Flix programs in WebAssembly-enabled browsers. The work will include
reading papers, language design, and implementation in a real-world programming
language.
