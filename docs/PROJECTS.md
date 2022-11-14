# Project Proposals

Here is a list of projects that are currently open for exploration.

If you are interested in working on any of these projects, feel free to reach
out to us on Gitter. If you are a bachelor or master student at Aarhus
University you can reach out to Magnus directly. 

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

## Sub-Effecting

A type system characterizes the values of an expression, whereas an effect
system characterizes the computational side effects of an expression.
Programming languages with subtyping allows a more specific type to be used
where a less precise type is required (e.g., passing a Student object where a
Person object is expected). Similarly, sub-effecting allows a function with a
more specific effect to be passed where a less precise effect is expected.

This project aims to explore different strategies for designing and implementing
sub-effecting in a programming language focusing on type and effect inference.

The project offers the opportunity to read research papers, work on language
design, and work on a real-world programming language developed at Aarhus
University and by a community of open-source contributors (see www.flix.dev).

## Tail Recursion Modulo Cons

Tail Recursion Modulo Cons (TRMC) is an important optimization for pure
functional programming languages that allow common functions such as `map` and
filter to as-efficient as if written in impure, imperative programming languages.

The project aims to explore topics such as tail calls, tail call elimination,
and in particular the special form of Tail Recursion Modulo Cons (TRMC).

The project offers the opportunity to read research papers, work on language
design, and work on a real-world programming language developed at Aarhus
University and by a community of open-source contributors (see www.flix.dev).


## Termination Analysis
A common programming mistake is to unintentional write an infinite loop. Most
contemporary programming languages such as C, C++, C#, Java, Kotlin, Scala, etc.
do not help programmers avoid such issues. Termination analysis or termination
checkers describe a wide-range of techniques that can be used to verify that a
program (or part of a program) always terminates, for any input. These
techniques range from sophisticated type systems to the use of SMT solvers. In
the case of functional programming, such termination checkers may try to ensure
termination by verifying that recursion is always on structurally smaller
elements. 

The aim of this project is to explore the design space of termination analysis
for a functional language.

The project offers the opportunity to read research papers, work on language
design, and work on a real-world programming language developed at Aarhus
University and by a community of open-source contributors (see www.flix.dev). 
