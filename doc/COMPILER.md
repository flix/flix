# Compiler

## Maturity of Features

The following ranks the maturity/quaity of Flix features.

A score of 10 means excellent. A score 0 means terrible.

| Feature                 | Score | Comment                                      |
|-------------------------|-------|----------------------------------------------|
| Algebraic Data Types    | 8     |                                              |
| Arrays                  | 8     |                                              |
| Concurrency             | 5     | Issues w.r.t. lambdas and channels           |
| JVM Interop             | 7     | Issues w.r.t. arrays, exceptions, and null   |
| References              | 9     |                                              |
| Namespaces              | 6     | No import/use mechanism                      |
| Equality                | 5     | Works, but ad-hoc and inefficient            |
| Arithemetic             | 9     |                                              |
| Strings                 | 10    |                                              |
| String Interpolation    | 5     | Lacks coercion of primitive types to strings |
| Lambdas                 | 8     |                                              |
| Pattern Matching        | 9     |                                              |
| UFCS                    | 9     |                                              |
| Records                 | 9     | Disallow duplicate labels?                   |
| First-Class Constraints | 9     |                                              |
| Datalog Engine          | 6     | Being replaced                               |
|                         |       |                                              |

Just because a feature does not have a score of 8, does not make that it doesn't work.
It just means that its implementation may be suboptimal. If something has a score of 10
that means it essentially cannot be improved.

## Maturity of Phases

The following ranks the maturity/quality of the compiler phases.

A score of 10 means excellent. A score 0 means terrible.

| Phase       | Score | Comment |
|-------------|-------|---------|
| Parser      |     8 |         |
| Weeder      |     8 |         |
| Namer       |     8 |         |
| Resolver    |     8 |         |
| Typer       |     7 |         |
| Stratifier  |     9 |         |
| PatternExh  |     2 | Needs a rewrite |
| Redundancy  |     9 |         |
| Linter      |     0 | Planned |
| Safety      |    10 |         |
| Monomorph   |     6 |         |
| Synthesize  |     5 |         |
| Simplifier  |     7 |         |
| ClosureConv |     7 |         |
| LambdaLift  |     7 |         |
| Tailrec     |     8 |         |
| Inliner     |     0 | Planned |
| Optimizer   |     8 |         |
| TreeShaker  |     7 |         |
| VarNumber   |     5 |         |
| Finalize    |     8 |         |
| JVMBackend  |     3 | Rewrite planned |

