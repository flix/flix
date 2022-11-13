# Project Proposals

This is a list of projects that are currently open for exploration.

If you are interested in working on any of these projects, feel free to reach
out to us on Gitter. If you are a bachelor or master student at Aarhus
University you can reach out to Magnus directly. 

## Code Formatting
A significant part of compiler design focuses on the parser; the phase that turns
source code text into abstract syntax trees, but the opposite direction is often
forgotten: the code formatter that turns abstract syntax trees back into neatly
formatted source code. Today, programming languages like Go, IDEs like Intellij IDEA,
and linters all offer support for automatic code re-formatting.

The aim of this project is to explore different strategies for code formatters,
i.e. pretty printers. This turns out to part art and part science.
The work will include reading papers, language design, and implementation 
in a real-world programming language being developed
at Aarhus University (see flix.dev).

## Sub-Effecting

TBD

Change to declarative system?
Change to W?
Simplify types with free variables?


## Tail Recursion Modulo Cons

Tail Recursion Modulo Cons (TRMC) is an important optimization for pure
functional programming languages that allow common functions such as `map` and
filter to as-efficient as if written in impure, imperative programming languages.

The project aims to explore topics such as tail calls, tail call elimination,
and in particular the special form of Tail Recursion Modulo Cons (TRMC).
The work will include reading papers, language
design, and implementation in a real-world programming language being developed
at Aarhus University (see flix.dev).

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
for a functional language. The work will include reading papers, language
design, and implementation in a real-world programming language being developed
at Aarhus University (see flix.dev).

## Record-Replay

## Capture Control

