# Project Proposals

This is a list of projects that are currently open for exploration.

If you are interested in working on any of these projects, you can reach out to us on Gitter.

## Auto-Completion and Program Synthesis

Integrated development environments (IDEs), such as Intellij IDEA, aid programmers by providing auto-completion for 
field and method names. Dependently-typed programming languages such as Agda and Idris take
this a step further by allowing the programmer to have the compiler fill in entire expressions based on the
types of the program. Effectively, the compiler searches for expressions that satisfy the requirements (e.g. types)
of a "hole" in the program. If there are multiple candidates, the compiler uses heuristics (e.g. Machine Learning) to 
rank the different choices.

The aim of this project is to: (1) explore the design space of such program completions, and to 
(2) design and implement such a system for the Flix programming language.

The work will include reading papers, language design, and implementation in a real-world programming language
being developed at Aarhus University (see flix.dev).

### Literature
- Type Driven Development with Idris - Edwin Brady


## Compiler Fuzzing
Compilers are large and complex pieces of software. The correctness of a
compiler is paramount: A compiler that silent mis-compiles (i.e. wrongly
translates) a program is dangerous: We cannot trust the programs we compile and
run! We can test compilers by writing unit tests, but unfortunate such tests
tend to only test the 'happy path' of the compiler. Moreover, the amount of unit
tests that can be written is limited by human factors. Instead, compiler fuzzing
techniques have been proposed. A compiler fuzzy typically takes a test suite as
input and subtly changes the programs in a systematic fashion and recompiles the
test suite. This process is fully automatic and be run for hours. Often such
techniques, with suitably clever 'mutation strategies' are able to find more
bugs than those found by ordinary unit testing. 

The aim of this project is to: (1) explore the design space of compiler fuzzing
techniques and to (2) design and implement such a system for the Flix
programming language. The work will include reading papers, language design, and
implementation in a real-world programming language being developed at Aarhus
University (see flix.dev).

### Literature
- Skeletal program enumeration for rigorous compiler testing - Zhang et al.


## Inlining - The Holy Grail of Compiler Optimizations

A compiler checks an input program for well-formedness and outputs a typically
lower-level program, e.g. JVM bytecode, machine code, etc. An *optimizing*
compiler aims to not just translate a program, but to eliminate the overhead of
abstractions used in the source program. Inlining is one of the most powerful
techniques; simply put during inlining a function definition is replaced by its
body which often enables further optimizations. However, inlining also increases
the size of the generated code (and can lead to non-termination if not applied
carefully). 

The aim of this project is to: (1) explore the design space of compiler
optimization strategies for functional and imperative programming languages and
to (2) design and implement such optimizations for the Flix programming
language. This includes looking into techniques such as monomorphization,
closure elimination, case-of-case optimizations, partial evaluation, and more. 

## Literature 




## Delimited Continuations




## GADTS - Extensible Variants -- MLSub


## "Flixdoc" - A Web Programming Project
Most programming languages come with large standard libraries. To be useable,
programmer most be able to quickly get an overview of the API surface and to
quickly search for functions, data types, etc. Today, most serious programming
language have an online web page (e.g. Javadoc) that presents this information.
The modern web offers a lot of opportunity to improve upon existing API website.
For example, everything from incremental loading to integrated search to online
examples that can be run in the browser. 

The aim of this project is to learn about modern web technologies and to
implement a online API documentation website for Flix, a "Flixdoc". 

### Literature
- https://reactjs.org/



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

### Literature
- Andreas Abel: Termination Checker for Simple Functional Programs
