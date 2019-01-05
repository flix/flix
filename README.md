<p align="center" >
    <img src="https://raw.githubusercontent.com/flix/flix/master/doc/logo.png" height="91px" 
    alt="The Flix Programming Language" 
    title="The Flix Programming Language">
</p>

**Flix** is a statically typed functional- and logic programming language inspired by Scala, OCaml, F#, Haskell, and Datalog.
The syntax of Flix resembles Scala and Datalog. 
The type system supports local type inference and is based on Hindley-Milner. 
Flix runs on the Java Virtual Machine and compiles directly to JVM bytecode.

See the [official Flix website](https://flix.github.io/) for more information about Flix. 

[![Build Status](https://travis-ci.org/flix/flix.svg?branch=master)](https://travis-ci.org/flix/flix)
[![Gitter](https://badges.gitter.im/gitterHQ/gitter.svg)](https://gitter.im/flix/Lobby)

## Example

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

## License

Flix is available under the Apache 2.0 license.

## Sponsors

We kindly thank EJ Technologies for providing us with 
[JProfiler](http://www.ej-technologies.com/products/jprofiler/overview.html)
and JetBrains for providing us with 
[IntelliJ IDEA](https://www.jetbrains.com/idea/).
