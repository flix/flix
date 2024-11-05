# Did You Know?

## Language

Did you know that:

- Flix offers a unique combination of features, including: algebraic data types
  and pattern matching, extensible records, type classes, higher-kinded types,
  polymorphic effects, and first-class Datalog constraints.

- Flix has no global state. Any state must be passed around explicitly.

- Flix is one language. There are no pragmas or compiler flags to enable or
  disable features.

- Flix supports type parameter elision. That is, polymorphic functions can be
  written without explicitly introducing their type parameters. For example,
  `def map(f: a -> b, l: List[a]): List[b]`.

- the Flix type and effect system can enforce that a function argument is pure.

- Flix supports effect polymorphism. For example, the `List.map` function is
  effect polymorphic: its purity depends on the purity of its function argument.

- in Flix every declaration is private by default.

- In Flix no execution happens before `main`. There is no global state nor any
  static field initializers.

- Flix supports full tail call elimination, i.e. tail calls do not grow the
  stack. Flix -- being on the JVM -- emulates tail calls until Project Loom
  arrives.

- Flix supports extensible records with row polymorphism.

- Flix supports string interpolation by default, e.g. "Hello ${name}". String
  interpolation uses the `ToString` type class.

- Flix supports the "pipeline" operator `|>` and the Flix standard library is
  designed around it.

- In Flix type variables are lowercase and types are uppercase.

- In Flix local variables and functions are lowercase whereas enum constructors
  are uppercase.

- Flix supports set and map literals `Set#{1, 2, 3}` and `Map#{1 => 2, 3 => 4}`.

- Flix supports monadic do-notation with the `let*` construct.

- Flix supports "program holes" written as either `???` or as `?name`.

- Flix supports infix function applications via backticks.

- Flix compiles to JVM bytecode and runs on the Java Virtual Machine.

- Flix supports channel and process-based concurrency, including the powerful
  `select` expression.

- Flix supports first-class Datalog constraints, i.e. Datalog program fragments
  are values that can be passed to and returned from functions, etc.

- Flix supports compile-time checked stratified negation.

- Flix supports partial application, i.e. a function can be called with fewer
  arguments that its declared formal parameters.

- the Flix type and effect system is powered by Hindley-Milner. The same core
  type system that is used by OCaml, Standard ML, and Haskell.

- the Flix type and effect system is sound, i.e. if a program type checks then a
  type error cannot occur at run-time. If an expression is pure then it cannot
  have a side-effect.

- the Flix type and effect system supports complete type inference, i.e. if a
  program is typeable then the type inference with find the typing.

- The Flix "Tips and Tricks"-section https://doc.flix.dev/tipstricks/ describes
  many useful smaller features of the language.

- Flix has a unique meta-programming feature that allows a higher-order
  functions to inspect the purity of its function argument(s).

- Flix names its floats and integers types after their sizes, e.g. `Float32`,
  `Float64`, `Int32` and `Int64`.

- Flix -- by design -- uses records for labelled arguments. Records are a
  natural part of the type system and works for top-level, local, and
  first-class functions.

- Flix -- by design -- has no implicit coercions, but provide several functions
  for explicit coercions.

- Flix -- by design -- disallows unused variables and shadowed variables since
  these are a frequent source of bugs.

- Flix -- by design -- disallows allow unused declarations. This prevents bit
  rot.

- Flix -- by design -- does not support unprincipled overloading. Instead,
  functions are given meaningful names, e.g. `Map.insert` and
  `Map.insertWithKey`.

- Flix -- by design -- does not support variadic functions. We believe it is
  better to pass an explicit array or list.

- Controversial: Flix defines division by zero to equal zero.

- Controversial: Flix defines String division as concatenation with the path
  separator. For example, `"Foo" / "Bar.txt" => "Foo\Bar.txt"` on Windows.

## Standard Library

Did you know that:

- Flix has an extensive standard library with more than 2,600 functions spanning
  more than 30,000 lines of code.

- the Flix Prelude, i.e. the functions which are imported by default, is kept
  minimal and contains less than 20 functions.

- most higher-order functions in the Flix standard library are effect
  polymorphic, i.e. they can be called with pure or impure functions.

- the Flix type and effect system enforces that equality and ordering functions
  must be pure.

- the Flix standard library uses records to avoid confusion when a function
  takes multiple arguments of the same type. For example, `String.contains` must
  be called as `String.contains(substr = "foo", "bar")`.

- the Flix `List` module offers more than 95 functions.

- the Flix `String` module offers more than 95 functions.

- the Flix `Foldable` module offers more than 30 functions.

- the Flix standard library follows the convention of "subject-last" to enable
  pipelining (`|>`).

## Ecosystem

Did you know that:

- Flix has an official Visual Studio Code extension.

- Flix has an official dark theme inspired by Monokai called "Flixify Dark".

- the Flix website (https://flix.dev/) lists the design principles behind Flix.

- Flix has an online playground available at https://play.flix.dev/

- Flix has online API documentation available at https://doc.flix.dev/

- the Flix VSCode extension uses the real Flix compiler.

- the Flix VSCode extension supports auto-complete, jump to definition, hover to
  show the type and effect of an expression, find all usages, and more.

- the Flix VSCode extension has built-in snippets for type class instances. Try
  `instance Eq [auto complete]`.

- the Flix VSCode extension supports semantic highlighting.

- the Flix VSCode extension has built-in "code hints" that suggests when lazy
  and/or parallel evaluation is enabled or inhibited by impurity.

- Flix has community build where Flix libraries can be included in the CI
  pipeline used to build the Flix compiler.

- Flix has a nascent build system and package manager based on GitHub releases.
  Today it is possible to build, package, and install Flix packages. Dependency
  management is in the works.

## Compiler

Did you know that:

- Flix -- by design -- has no compiler warnings, only compiler errors. Warnings
  can be ignored, but errors cannot be.

- the Flix compiler uses monomorphization hence primitive values are (almost)
  never boxed.

- the Flix compiler supports incremental and parallel compilation.

- the Flix compiler has more than 28 compiler phases.

- the Flix compiler contains more than 80,000 lines of code.

- the Flix compiler has more than 13,500 manually written unit tests.

- the performance of the Flix compiler is tracked at https://arewefast.flix.dev/

## Other

Did you know that:

- Flix is developed by programming language researchers at Aarhus University
  (Denmark) in collaboration with researchers at the University of Waterloo
  (Canada), and at Eberhard Karls University of Tübingen (Germany), and by a
  growing open source community.

- Several novel aspects of the Flix programming language has been described in
  the research literature, including its type and effect system and support for
  first-class Datalog constraints.

- Flix is funded by the Independent Research Fund Denmark, Amazon Research,  
  DIREC, the Stibo Foundation, and the Concordium Foundation.

- more than 50 people have contributed to the Flix compiler.

- more than 2,000 pull requests have been merged into the Flix compiler.
