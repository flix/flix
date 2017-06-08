<p align="center" >
    <img src="https://raw.githubusercontent.com/flix/flix/master/doc/logo.png" height="91px" 
    alt="The Flix Programming Language" 
    title="The Flix Programming Language">
</p>

**Flix** is a statically typed functional- and logic programming language inspired by Scala, OCaml, F#, Haskell, and Datalog.
The syntax of Flix resembles Scala and Datalog. 
The type system supports local type inference and is based on Hindley-Milner. 
Flix runs on the Java Virtual Machine and compiles directly to JVM bytecode.

**Flix** supports hybrid functional and logic programming featuring a built-in fixed point engine based on semi-naive evaluation.
The functional and logic languages can be used independently, if so desired. 
For example, a Flix program can be purely functional, or Flix can be used as a standalone Datalog engine. 

A unique feature of Flix is its extension of Datalog semantics with user-defined lattices and monotone functions.

**Flix** comes with a standard library with common functional data types such as `Option`, `Result`, `List`, `Set`, and `Map`. 

See the [official Flix website](https://flix.github.io/) for more information about Flix. 
You can [try Flix](https://flix.github.io/try/) in the online editor.

[![Build Status](https://travis-ci.org/flix/flix.svg?branch=master)](https://travis-ci.org/flix/flix)
[![CircleCI](https://circleci.com/gh/flix/flix.svg?style=svg)](https://circleci.com/gh/flix/flix)
[![Gitter](https://badges.gitter.im/gitterHQ/gitter.svg)](https://gitter.im/flix/Lobby)

## Sample Flix Code (Functional)

```
///
/// The expressions of the lambda calculus are: variables, lambda abstractions, and applications.
///
enum Expression {
    // A variable expression. A variable is represented by an integer.
    case Var(Int),

    // A lambda abstraction expression. A variable is represented by an integer.
    case Abs(Int, Expression),

    // A function application expression.
    case App(Expression, Expression)
}

///
/// Performs alpha conversion by introducing fresh variables for all variables in the given expression `e0`.
///
def alpha(e0: Expression, m: Map[Int, Int]): Expression = match e0 with {
    case Var(x) =>
        // Check if we need to rename the variable.
        match Map.get(x, m) with {
            case None    => Var(x)
            case Some(y) => Var(y)
        }
    case Abs(x, e) =>
        // Generate a fresh variable name for `x`.
        let y = freshVar();
        Abs(y, alpha(e, Map.insert(x, y, m)))

    case App(e1, e2) =>
        // Recursively perform alpha conversion on each expression.
        App(alpha(e1, m), alpha(e2, m))
}
```

## Sample Flix Code (Logic)

```
rel LitStm(r: Str, c: Int)         // r = c
rel AddStm(r: Str, x: Str, y: Str) // r = x + y
rel DivStm(r: Str, x: Str, y: Str) // r = x / y

LocalVar(r, sum(v1, v2)) :- AddStm(r, x, y),
                            LocalVar(x, v1),
                            LocalVar(y, v2).

ArithmeticError(r) :- isMaybeZero(y),
                      DivStm(r, n, d),
                      LocalVar(d, y).
```

## Features

- algebraic data types and pattern matching.
- higher order functions.
- local type inference.
- parametric polymorphism.
- scala and prolog-style syntax.
- parallel rule evaluation.
- built-in delta debugger.
- built-in quick checker framework.
- built-in test framework.
- built-in static program verifier.
- integrated browser-based debugger and profiler.

## License

Flix is available under the Apache 2.0 license.

## Sponsors

We kindly thank EJ Technologies for providing us with 
[JProfiler](http://www.ej-technologies.com/products/jprofiler/overview.html)
and JetBrains for providing us with 
[IntelliJ IDEA](https://www.jetbrains.com/idea/).
