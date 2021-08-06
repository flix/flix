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
- https://doc.flix.dev/fixpoints/



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
