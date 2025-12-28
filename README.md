<p align="center" >
    <img src="https://raw.githubusercontent.com/flix/flix/master/docs/logo.png" height="91px" 
    alt="The Flix Programming Language" 
    title="The Flix Programming Language">
</p>

**Flix** is a statically typed functional, imperative, and logic programming language.

We refer you to the [official Flix website (flix.dev)](https://flix.dev/) for more information about Flix. 

[![Gitter](https://badges.gitter.im/gitterHQ/gitter.svg)](https://gitter.im/flix/Lobby)

## Example

```flix
///
/// The expressions of the lambda calculus are: variables, lambda abstractions, and applications.
///
enum Expression {
    // A variable expression. A variable is represented by an integer. 
    case Var(Int32),
    
    // A lambda abstracation expression. A variable is represented by an integer.
    case Abs(Int32, Expression),
    
    // A function application expression.
    case App(Expression, Expression),
}

///
/// Performs alpha conversion by introducing fresh variables for all variables in the given expression `e0`.
///
def alpha(e0: Expression, m: Map[Int32, Int32]): Expression = match e0 {
    case Var(x) =>
        // Check if we need to rename the variable.
        match Map.get(x, m) {
            case None => Var(x)
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

## Building

See [docs/BUILD.md](docs/BUILD.md).

## License

Flix is available under the Apache 2.0 license.

## Sponsors

We kindly thank [EJ Technologies](https://www.ej-technologies.com/) for providing us with 
[JProfiler](http://www.ej-technologies.com/products/jprofiler/overview.html)
and [JetBrains](https://www.jetbrains.com/) for providing us with 
[IntelliJ IDEA](https://www.jetbrains.com/idea/).
