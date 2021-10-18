# Project Proposals

This is a list of projects that are currently open for exploration.

If you are interested in working on any of these projects, feel free to reach
out to us on Gitter. If you are a bachelor or master student at Aarhus
University you can reach out to Magnus directly. 

## Auto-Completion and Program Synthesis

Integrated development environments (IDEs), such as Intellij IDEA, aid
programmers by providing auto-completion for field and method names.
Dependently-typed programming languages such as Agda and Idris take this a step
further by allowing the programmer to have the compiler fill in entire
expressions based on the types of the program. Essentially, the compiler
searches for expressions that satisfy the requirements (e.g. types) of a hole in
the program. 

The aim of this project is to: (1) explore the design space of such program
completions, and to (2) design and implement such a system for the Flix
programming language. The work will include reading papers, language design, and
implementation in a real-world programming language being developed at Aarhus
University (see flix.dev).



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



## Online API Documentation - A Web Programming Project
Most programming languages come with a large standard library. To be useable,
programmers must be able to quickly get an overview of the API and to search for
specific functions, data types, classes, traits, etc. Today, most programming
languages have online API documentation (e.g. Javadoc, Scaladoc, etc.). The
modern web offers a lot of opportunity to improve upon such API documentation.
For example, few online API documentation tools provide the ability to search
"by type" or to run code online. 

The aim of this project is to explore modern web technologies and to implement
an online API documentation website for Flix, a "Flixdoc". 



## Inlining - The Holy Grail of Compiler Optimizations

A compiler checks an input program for well-formedness and translates it to a
lower-level format, i.e. JVM bytecode or machine code. An *optimizing* compiler
aims to not just translate a program, but to make it run faster. In particular,
high-level programs often contain a lot of abstracts that when compiled naive
are a source of overhead. Inlining is one of the most powerful program
optimization techniques. During inlining, function definitions are expanded to
their body expressions which often enables further optimizations. However,
inlining also increases the size of the generated code, and can lead to
non-termination if applied carelessly.

The aim of this project is to: (1) explore the design space of compiler
optimization strategies for functional and imperative programming languages, and
to (2) design and implement such optimizations for the Flix programming
language. This includes looking into techniques such as monomorphization,
closure elimination, case-of-case optimizations, partial evaluation, and more. 



## Delimited Continuations
Most programming languages support a notion of a exceptions. An exception is a
special control-flow construct that allows a function to abort execution and to
transfer control to an exception handler. Exceptions are typically implemented
as an intrinsic part of the programming language. However, it is possible and
useful to offer more powerful features that enable users or library authors to
implement their own control-structures. Delimited continuations is one such
feature. In a programming language with delimited continuations, exceptions can
be implemented entirely as a library. Moreover, delimited continuations can
enable resumable exceptions and other interesting control-flow constructs.

The aim of this project is to: (1) explore the design space of delimited
continuations and to (2) design and implement a notion of delimited
continuations for the Flix programming language. The work will include reading
papers, language design, and implementation in a real-world programming language
being developed at Aarhus University (see flix.dev).



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
