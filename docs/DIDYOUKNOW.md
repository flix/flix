# Did You Know?

## Language

Did you know that:

- Flix offers a unique combination of features, including: algebraic data types and pattern matching, 
  extensible records, type classes, higher-kinded types, channel and process-based concurrency, 
  a polymorphic effect system, purity polymorphic functions, and first-class Datalog constraints.

- Flix has no implicit global state. Any global state must be passed around explicitly.

- Flix is one language. There are no pragmas or compiler flags to enable or disable features. 

- Flix supports type parameter elision. That is, in most cases, a polymorphic function does 
  can be written without explicitly declaring its type parameters.

- the Flix type and effect system can enforce purity.

- Flix supports effect polymorphism? For example, the effect of `List.map` 
  depends on the effect of its argument.

- every declaration in Flix is private by default.

- In Flix nothing in is executed before main. There is no global state nor any static initializers.

- Flix supports full tail call elimination, i.e. tail calls do not grow the stack.
  (Flix-- being on the JVM -- has to emulate tail calls until Project Loom arrives.)

- Flix supports extensible records with row polymorphism.

- Flix supports string interpolation by default, e.g. "Hello ${name}". String interpolation
  uses the `ToString` type class -- as it should be.

- Flix supports the "pipeline" operator `|>` and the Flix standard library is designed 
  to make good use of it.

- Flix supports set and map literals `Set#{1, 2, 3}` and `Map#{1 => 2, 3 => 4}`.

- Flix supports monadic do-notation called `let*`.

- Flix supports "program holes" written as either `???` or as `?name`.

- Flix supports infix function applications via backticks.

- Flix compiles to JVM bytecode and runs on the Java Virtual Machine.

- Flix supports channels and processes, including the powerful `select` expression.

- Flix supports first-class Datalog constraints, i.e. Datalog program fragments are values.

- Flix supports compile-time checked stratified negation.

- Flix supports partial application, i.e. a function can be called with fewer
  arguments that its declared number of formal parameters.

- the Flix type and effect system is based on Hindley-Miler.
  The same core type system that powers OCaml, Standard ML, and Haskell.

- the Flix type and effect system is sound, i.e. if a program type checks
  then a type error cannot occur at run-time.

- the Flix type and effect system supports complete type inference, i.e.
  if a program is typeable then the type inference in the Flix compiler
  will find the typing without a single annotation.

- The Flix "Tips and Tricks"-section https://doc.flix.dev/tipstricks/ contains many useful niceties.

- Flix -- by design -- has no implicit coercions (but does provide several explicit coercion functions).

- Flix -- by design -- disallows unused variables and shadowed variables. Research has shown that such
  redundancies are correlated with bugs.

- Flix -- by design -- disallows allow unused declarations. This prevents bit rot.

- Flix -- by design -- does not support function overloading. Instead, functions are given meaningful names, e.g.
  `Map.insert` and `Map.insertWithKey`.

- Flix -- by design -- does not support variadic functions. We believe such overloading
  is unprincipled and unnecessary.

- Flix -- by design -- does not support labelled arguments. Records can be used as an alternative and works
  both for top-level, local, and first-class functions.

- Flix has a unique meta-programming facility that allows a higher-order function to inspect
  the purity of its function argument(s).

- Flix names its floats and integers types after their sizes (`Float32`, `Float64` and `Int32` and `Int64`).

- Controversial: Flix defines division by zero to equal zero.

- Controversial: Flix defines String division as concatenation with the path separator. 
  For example, `"Foo" / "Bar.txt" => "Foo\Bar.txt"` on Windows.

## Standard Library

Did you know that:

- Flix has an extensive standard library with more than 2,000 functions 
  spanning more than 30,000 lines of code.

- the Flix Prelude (the collection of functions which are imported by default) is 
  deliberately kept minimal and contains less than 20 functions.

- most higher-order functions in the Flix standard library are effect polymorphic
  (they can be called with pure or impure functions). The exceptions are functions
  that define (or use) equality, ordering, etc.

- Mutating data structure operations end with an exclamation mark. For example `Array.transform!`.

- the Flix standard library uses records to avoid confusion when functions take multiple arguments of
  the same type. For example, `String.contains` must be called as `String.contains(substr = "foo", "bar")`.

- the Flix `List` module has more than 95 functions.

- the Flix `String` module has more than 95 functions.

- the Flix `Foldable` module offers more than 30 functions.

- the Flix standard library follows the convention of "subject-last" to allow for pipelining (`|>`).

## Ecosystem

Did you know that:

- Flix has an official Visual Studio Code extension.

- Flix has an official dark theme inspired by Monokai called "Flixify Dark".

- the Flix website (https://flix.dev/) lists the design principles behind Flix.

- Flix has an online playground available at https://play.flix.dev/

- Flix has online API documentation available at https://doc.flix.dev/

- the Flix VSCode extension uses the real Flix compiler?

- the Flix VSCode extension supports auto-complete, jump to definition, 
  hover to see type and effect, find all usages, renaming, and more.

- the Flix VSCode extension has built-in snippets for type class instances.
  Try `instance Eq [auto complete]`.

- the Flix VSCode extension supports semantic tokens (highlighting based not just on the syntax, 
  but on the semantics of a program.)

- the Flix VSCode extension has built-in "code hints" that suggests when lazy and/or parallel
  evaluation is enabled (or inhibited by impurity).

- Flix has community build where Flix libraries can be included in the CI process used to build Flix?

- Flix has a nascent build system and package manager (the latter based on GitHub releases). 
  Today it is possible to build, package, and install Flix packages, but there is no dependency 
  management yet.

## Compiler

Did you know that:

- Flix -- by design -- has no compiler warnings, only compiler errors.
  Warnings can be ignored, but errors cannot be.

- Flix uses monomorphization hence primitive values are (almost) never boxed.

- the Flix compiler supports incremental and parallel compilation.

- the Flix compiler has more than 28 compiler phases?

- the Flix compiler contains more than 150,000 lines of code.

- the Flix compiler has more than 12,500 manually written unit tests.

- the performance of the Flix compiler is tracked at https://arewefast.flix.dev/

## Other

Did you know that:

- Flix is developed by programming language researchers at Aarhus University (Denmark) 
  in collaboration with researchers at the University of Waterloo (Canada), and at
  Eberhard Karls University of Tübingen (Germany), and by a growing open source community.

- Several novel aspects of the Flix programming language has been described in the research literature?

- Flix has received grants from the Independent Research Fund Denmark, 
  from Amazon Research, and from the Concordium Foundation.

- Flix has been discussed on Reddit, HackerNews, and LambdaTheUltimate.

- more than 40 people have contributed to the Flix compiler.

- more than 1,500 pull requests have been merged into the Flix compiler.
