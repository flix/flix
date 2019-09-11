# Bachelor Project Proposals

# Termination Analysis
A common programming mistake is to write an unintentional infinite loop. 
Most contemporary programming languages such as C, C++, C#, Java, Kotlin, Scala, etc. do not help programmers avoid such issues.
Termination analysis or termination checkers describe a wide-range of techniques that can be used to verify that a program 
(or part of a program) always terminates, for any input. These techniques range from sophisticated type systems to the use of
SMT solvers. In the case of functional programming, such termination checkers may try to ensure termination by verifying that recursion is always on structurally smaller elements. 

The aim of this project is to explore the design space of termination analysis for a functional language.
The work will include reading papers, language design, and implementation in a real-world programming language.

### Literature
- Andreas Abel: Termination Checker for Simple Functional Programs

Contact: Magnus Madsen <magnusm@cs.au.dk>



# Package Manager
Programs today are not written from scratch, but rather build on a large collection of external libraries.
Different languages have different package ecosystems: Java has Maven, JavaScript has NPM, Rust has Cargo, and so forth.
All of these languages offer some form of package manager that is used to download, install, upgrade, and keep track
of the dependencies of a software project. The problem is highly non-trivial, for example, how should we handle the 
situation where a project depends on package A and package B, and A depends on C (version 1.0), but B depends on C
(version 2.0)?

The aim of this project is to (1) explore the design space of package managers for programming languages, and
(2) design and implementation a package manager for the Flix programming language. 
The work will include reading papers, language design, and implementation in a real-world programming language.

### Literature
- Michael Hanus: Semantic Versioning Checking in a Declarative Package Manager
- https://medium.com/@sdboyer/so-you-want-to-write-a-package-manager-4ae9c17d9527 

Contact: Magnus Madsen <magnusm@cs.au.dk>

# Program Completion




# Fixpoint Engine






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
The work will include reading papers, language design, and implementation in a real-world programming language.

### Literature
- Pretnar, Matija. An introduction to algebraic effects and handlers. (invited tutorial paper.)
- Bauer, Andrej, and Matija Pretnar: Programming with algebraic effects and handlers.

Contact: Magnus Madsen <magnusm@cs.au.dk>


