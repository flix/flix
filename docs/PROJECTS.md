# Project Proposals

Here is a list of projects that are currently open for exploration.

If you are interested in working on any of these projects, feel free to reach
out to us on Gitter. If you are a bachelor or master student at Aarhus
University you can reach out to Magnus directly. 

## QuickCheck

# Package Management

Today, programs are rarely written from scratch, but rather build on a large
collection of external libraries. Different languages have different package
ecosystems: Java has Maven, JavaScript has NPM, Rust has Cargo, and so forth.
All of these languages offer some form of package manager that is used to
download, install, upgrade, and keep track of the dependencies of a software
project. The problem is non-trivial: For example, how should we handle the
situation where a project depends on package A and package B, and A depends on C
(version 1.0), but B depends on C (version 2.0)?

The aim of this project is to (1) explore the design space of package managers
for programming languages, and to (2) design and implement a package manager for
the Flix programming language. The work will include reading papers, language
design, and implementation in a real-world programming language


## Compiler Fuzzing
Compilers are large and complex pieces of software. The correctness of a
compiler is paramount: A compiler that silent mis-compiles (i.e. wrongly
translates) a program is dangerous: We cannot trust the programs we compile and
run! We can test compilers by writing unit tests, but unfortunately such tests
tend to only test the 'happy path' of the compiler. Moreover, the amount of unit
tests that can be written is limited. Instead, compiler fuzzing techniques have
been proposed. A compiler fuzzy typically takes a test suite as input and subtly
changes the programs in a systematic fashion and then re-compiles the test
suite. This process is fully automatic and can be run for hours. Often such
techniques, with suitably clever 'mutation strategies', are able to find
significantly more bugs than those found by unit testing.

The aim of this project is to: (1) explore the design space of compiler fuzzing
techniques, and to (2) design and implement such a system for the Flix
programming language. The work will include reading papers, language design, and
implementation in a real-world programming language being developed at Aarhus
University (see flix.dev).

## Code Formatting

A significant part of compiler implementation focuses on the parser: the
compiler phase that turns source code text into abstract syntax trees. However,
the opposite direction is often forgotten: the code formatter that turns
abstract syntax trees back into neatly formatted source code text. Today,
programming languages like Go, IDEs like Intellij IDEA, and linters all come
with built-in support for code formatting.

This project aims to explore different strategies for code formatters and
experiment with the implementation of such formatters. This project is genuinely
part art and part science.

The project offers the opportunity to read research papers, work on language
design, and work on a real-world programming language developed at Aarhus
University and by a community of open-source contributors (see www.flix.dev).

## Sub-Typing and Sub-Effecting

A type system characterizes the values of an expression, whereas an effect
system characterizes the computational side effects of an expression.
Programming languages with subtyping allow a more specific type to be used
where a less precise type is required (e.g., passing a Student object where a
Person object is expected). Similarly, sub-effecting allows a function with a
more specific effect to be passed where a less precise effect is expected.

This project aims to explore different strategies for designing and implementing
sub-effecting in a programming language focusing on type and effect inference.

The project offers the opportunity to read research papers, work on language
design, and work on a real-world programming language developed at Aarhus
University and by a community of open-source contributors (see www.flix.dev).

## Tail Recursion Modulo Cons

Tail Recursion Modulo Cons (TRMC) is an essential optimization for functional
programming languages that enable very efficient compilation of common functions
such as map and filter. The key idea is the compilation of recursive functions
into imperative while-loops that operate on mutable data (even though the
functional program operates on immutable data).

This project aims to explore different strategies for the implementation of tail
calls and specifically for Tail Recursion Modulo Cons (TRMC).

The project offers the opportunity to read research papers, work on language
design, and work on a real-world programming language developed at Aarhus
University and by a community of open-source contributors (see www.flix.dev).

## Termination Analysis

A common programming mistake is to write an infinite loop. Unfortunately, most
contemporary programming languages, such as C, C++, C#, Java, Kotlin, and Scala,
do not help programmers avoid such issues. Termination analysis describes a wide
range of techniques that can verify that a program (or part of a program) always
terminates. For example, by checking that recursive calls always operate on
structurally smaller elements.

This project aims to explore strategies for termination analysis in a 
functional programming language.

The project offers the opportunity to read research papers, work on language
design, and work on a real-world programming language developed at Aarhus
University and by a community of open-source contributors (see www.flix.dev). 
