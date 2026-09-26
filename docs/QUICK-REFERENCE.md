# Flix Quick Reference

A one-page cheat sheet for the Flix language. For full details see the
[Flix Programming Guide](https://flix.dev/).

## Program structure

```flix
/// Doc comment. Ordinary comments start with //.
def main(): Unit \ IO =
    println("Hello, world!")
```

Expressions are separated with `;`. `def` defines a function, `let` a local:

```flix
def area(w: Int32, h: Int32): Int32 = w * h

def greeting(name: String): String \ IO =
    let msg = "Hello, ${name}!";
    println(msg);
    msg
```

Anonymous functions use `->`: `x -> x + 1`. Pipelines use `|>`: `l |> List.length |> println`.

## Primitive types

`Unit`, `Bool`, `Char`, `String`, `Int8`, `Int16`, `Int32`, `Int64`,
`BigInt`, `Float32`, `Float64`, `BigDecimal`.

Common operators: `+ - * / %`, `== != < <= > >=`, `and or not`,
`::` (list cons), `:::` (list append).

## Branching

```flix
if (x < 0) {
    "negative"
} else if (x == 0) {
    "zero"
} else {
    "positive"
}
```

## Enums and pattern matching

```flix
enum Shape {
    case Circle(Int32)
    case Rectangle(Int32, Int32)
}

def area(s: Shape): Int32 = match s {
    case Shape.Circle(r)       => 3 * (r * r)
    case Shape.Rectangle(h, w) => h * w
}
```

Enums can be parametric: `enum Tree[a] { case Leaf(a), case Node(Tree[a], Tree[a]) }`.
Use `case _ => ...` as a wildcard. Tuples destructure the same way:

```flix
match pair {
    case (1, _) => "starts with one"
    case _      => "something else"
}
```

## Lists

```flix
let l1 = 1 :: 2 :: 3 :: Nil;
let l2 = l1 ::: 4 :: Nil;
match l1 {
    case Nil     => 0
    case _ :: xs => 1 + List.length(xs)
}
```

## Strings

```flix
let name = "Ada";
let msg = "Hello, ${name}!";  // string interpolation
```

## Records

```flix
let r = {name = "Ada", age = 36};
r#name                            // field access
{name = "Grace" | r}              // update a field
{+lang = "Flix" | r}              // add a field
{-age | r}                        // drop a field
```

## Modules

```flix
mod Calc {
    pub def sum(x: Int32, y: Int32): Int32 = x + y
}

use Calc.sum;
use Calc.{sum};
```

## Traits and instances

```flix
trait Show[t] {
    pub def show(x: t): String
}

instance Show[Int32] {
    pub def show(x: Int32): String = "${x}"
}
```

## Effects

Pure functions carry the empty effect `\ {}`; impure ones declare what they
use, e.g. `\ IO`. Effect-polymorphic functions abstract over it with a
variable:

```flix
def inc(x: Int32): Int32 \ {} = x + 1
def twice(f: Int32 -> Int32 \ ef, x: Int32): Int32 \ ef = f(f(x))
```

## Option and Result

```flix
match opt {
    case Some(x) => x
    case None    => 0
}

match res {
    case Ok(v)  => v
    case Err(e) => 0
}
```

## Loops

```flix
def main(): Unit \ IO =
    let l = 1 :: 2 :: 3 :: Nil;
    foreach (x <- l)
        println(x)
```
