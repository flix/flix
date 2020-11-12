# Bachelor Project Proposals

# Package Management
Today, programs are rarely written from scratch, but rather build on a large collection of external libraries.
Different languages have different package ecosystems: Java has Maven, JavaScript has NPM, Rust has Cargo, and so forth.
All of these languages offer some form of package manager that is used to download, install, upgrade, and keep track
of the dependencies of a software project. The problem is non-trivial: For example, how should we handle the 
situation where a project depends on package A and package B, and A depends on C (version 1.0), but B depends on C
(version 2.0)?

The aim of this project is to (1) explore the design space of package managers for programming languages, and to
(2) design and implement a package manager for the Flix programming language. 
The work will include reading papers, language design, and implementation in a real-world programming language
being developed at Aarhus University (see flix.dev).

### Literature
- Michael Hanus: Semantic Versioning Checking in a Declarative Package Manager
- https://medium.com/@sdboyer/so-you-want-to-write-a-package-manager-4ae9c17d9527 

Contact: Magnus Madsen <magnusm@cs.au.dk>



# Auto-Completion and Program Synthesis
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
- Edwin Brady: Type Driven Development with Idris



# Fixpoint Engine
Datalog is a simple, yet powerful, declarative logic programming language. A Datalog program consists of a 
collection of constraints; each constrain is either a fact or rule. Together, the facts and rules imply a minimal
model, a unique solution to any Datalog program. The "fixpoint engine" is the software responsible for computing
the solution to a Datalog program. It is essentially a specialized form of database with support for parallel
evaluation.

The aim of this project is to: (1) understand how the semi-naive evaluation algorithm works, and to
(2) re-design and re-implemented the fixpoint engine in the Flix language to make it faster and easier to maintain.

- Stefano Ceri: What you always wanted to know about Datalog (and never dared to ask)
- https://flix.dev/programming-flix/#/fixpoints/ 



# Type Classes
Type classes, not to be confused with classes from object-oriented programming, provide a powerful and flexible 
mechanism for polymorphism that allows overloading of functions. For example, we can define a type class "Eq" with a 
function "equals" that determine if two values are equal, and then we can implement the "Eq" for booleans, integers, 
strings, list of integers, pairs, tuples, and so forth, and have the compiler use the "right" Eq instance
based on the type of an expression. Type classes have recently become quite popular and are being implemented in
languages such as Scala and Rust.  

The aim of this project is (1) explore the haskell implementation of type classes, (2) read and understand some
of the research literature on type classes to understand the fundamental issues, and (3) lay the foundations for a
simple implementation of type classes for the Flix programming language.


### Literature
- Philip Wadler: How to make ad-hoc polymorphism less ad hoc



# Uniqueness / Ownership Typing 
Resources, such a raw memory, file handles, sockets, database connections etc. must be carefully managed to ensure 
correct use and to ensure they are released when no longer needed. In languages such as C and Java, it is to
a high-degree the responsibility of the programmer to ensure such correct use. For example, in C it is easy to forget
to free memory or to free memory twice, whereas in Java it is easy to forget to close a file stream.

Programming languages such as Rust and Clean attempt to address these issues with uniqueness / ownership type systems.
In these languages, the type system tracks the use of a resource and ensures that it accessed correctly and released
at an appropriate time. 

The aim of this project is to explore the design space of uniqueness/ownership typing for a functional language.
The work will include reading papers, language design, and implementation in a real-world programming language.

### Literature
- Walker, David: Substructural type systems. Advanced Topics in Types and Programming Languages
- Clarke, David G., John M. Potter, and James Noble: Ownership types for flexible alias protection

Contact: Magnus Madsen <magnusm@cs.au.dk>



# Algebraic Effects
Modern languages, such as Java, JavaScript, Python, Kotlin, Go, etc., offer an increasing number of control-flow constructs,
such as async/await, exception, co-routines, generators, etc. Algebraic effects is a promising approach to a unified mechanism 
that is sufficiently powerful to express each of the former features and gives the programmer the power to define new such
control-flow constructs.

The aim of this project is to explore the design space of algebraic effects for a functional language.
The work will include reading papers, language design, and implementation in a real-world programming language
being developed at Aarhus University (see flix.dev).

### Literature
- Pretnar, Matija. An introduction to algebraic effects and handlers. (invited tutorial paper.)
- Bauer, Andrej, and Matija Pretnar: Programming with algebraic effects and handlers.

Contact: Magnus Madsen <magnusm@cs.au.dk>


# Termination Analysis
A common programming mistake is to unintentional write an infinite loop. 
Most contemporary programming languages such as C, C++, C#, Java, Kotlin, Scala, etc. do not help programmers avoid such issues.
Termination analysis or termination checkers describe a wide-range of techniques that can be used to verify that a program 
(or part of a program) always terminates, for any input. These techniques range from sophisticated type systems to the use of
SMT solvers. In the case of functional programming, such termination checkers may try to ensure termination by 
verifying that recursion is always on structurally smaller elements. 

The aim of this project is to explore the design space of termination analysis for a functional language.
The work will include reading papers, language design, and implementation in a real-world programming language
being developed at Aarhus University (see flix.dev).

### Literature
- Andreas Abel: Termination Checker for Simple Functional Programs

Contact: Magnus Madsen <magnusm@cs.au.dk>