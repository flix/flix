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
