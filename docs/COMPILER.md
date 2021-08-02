# Compiler

## Feature Maturity

The following table ranks the maturity of features in the Flix programming language.

- A score of 10 means that the feature is stable, well-documented, and well-tested. 
- A score of 5 means that the feature is unstable, likely not well-documented, and not yet battle tested.
- A score of 0 means that the feature is experimental and should be used with care.

| Feature                 | Score | Comments                                     |
|-------------------------|-------|----------------------------------------------|
| Standard Library        | 10    |                                              |
| Algebraic Data Types    | 9     |                                              |
| Arithmetic              | 9     |                                              |
| Arrays                  | 9     |                                              |
| Pattern Matching        | 9     |                                              |
| Records                 | 9     |                                              |
| References              | 9     |                                              |
| UFCS                    | 9     |                                              |
| Stratified Negation     | 9     |                                              |
| JVM interoperability    | 8     | No support for null, exceptions, or anonymous classes. |
| Opaque Types            | 8     | Not yet optimized by backend.                |
| Parametric Polymorphism | 8     | A few issues left to be worked out.          |
| Polymorphic Effects     | 8     |                                              |
| Namespaces              | 8     |                                              |
| First-Class Constraints | 7     |                                              |
| Type Aliases            | 7     | Not yet well-tested.                         |
| Datalog Solver          | 6     | Being rewritten.                             |
| Concurrency             | 5     | Run-time based on threads. Closures must not be moved between threads. |
| Equality                | 5     | Works, but ad-hoc.                           |
| Interactive Mode        | 4     | Experimental.                                |
| LSP Server              | 1     | Experimental.                                |

## Phase Maturity

The following table ranks the maturity of each compiler phase.

| Phase       | Score | Comment |
|-------------|-------|---------|
| Parser      |     8 |         |
| Weeder      |     8 |         |
| Namer       |     8 |         |
| Resolver    |     8 |         |
| Typer       |     7 |         |
| Stratifier  |     9 |         |
| PatternExh  |     2 | Works well, but rewrite planned. |
| Redundancy  |     9 |         |
| Linter      |     0 | Not yet implemented. |
| Safety      |    10 |         |
| Monomorph   |     6 |         |
| Synthesize  |     5 |         |
| Simplifier  |     7 |         |
| ClosureConv |     7 |         |
| LambdaLift  |     7 |         |
| Tailrec     |     8 |         |
| Inliner     |     0 | Not yet implemented. |
| Optimizer   |     8 |         |
| TreeShaker  |     7 |         |
| VarNumber   |     5 |         |
| Finalize    |     8 |         |
| JVMBackend  |     3 | Rewrite planned |

