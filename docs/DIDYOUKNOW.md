# Did You Know?

## Language

Did you know that:

- Flix has no global state.

- Flix is one language. There are no pragmas or compiler flags to enable or disable features. 

- Flix supports type parameter elision.

- in Flix division by zero yields zero.

- Did you know that the Flix type and effect system can enforce purity?
  For example, equality and comparison functions must be pure.

- Did you know that Flix disallows unused variables and shadowed variables?
  Research has shown that such redundancies are correlated with bugs.

- Did you know that Flix supports effect polymorphism?
  For example, the effect of `List.map` depends on the effect of its argument.

- Everything in Flix is private by default.

- In Flix, nothing in is executed before main.

- Flix has no implicit coercions (but does provide several explicit coercion functions).

- Flix does not allow unused declarations.

- Flix has no overloading. Functions are given useful names instead. (But Flix has type classes).

- Flix does not have variadic functions.

- Flix does not have labelled arguments.

- Default Implementations

- Explicit Override

- Type classes can be declared sealed.

- Flix compiles to JVM bytecode and runs on the Java Virtual Machine.

- Flix supports first-class Datalog constraints.

- Flix supports stratified negation.

- Flix supports partial application, i.e. a function can be called with fewer
  arguments that its declared number of formal parameters.

- the Flix type and effect system is based on Hindley-Miler.
  The same core type system that powers OCaml, Standard ML, and Haskell.

- the Flix type and effect system is sound, i.e. if a program type checks
  then a type error cannot occur at run-time.

- the Flix type and effect system supports complete type inference, i.e.
  if a program is typeable then the type inference in the Flix compiler
  will find the typing without a single annotation.

## Standard Library

Did you know that:

- Flix's standard library is extensive.


- Flix has minimal prelude with only 18 functions.

- All functions in the Flix prelude are total. Most functions in the Flix library are total.

- No blessed library.

- Mutable data is functional data

- Consistent names of functional and destructive operations

- Destructive operations are marked with '!'

- The Flix standard library is X lines of code.

- Flix library uses records to for certain function arguments.

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

- Several novel aspects of the Flix programming language has been described in the research literature?

## Compiler

Did you know that:

- by design Flix has no compiler warnings, only compiler errors.
  Warnings can be ignored, but errors cannot be.

- the Flix compiler supports incremental and parallel compilation.

- the Flix compiler has more than 28 compiler phases?

- the Flix compiler contains more than 150,000 lines of code.

- the Flix compiler has more than 12,500 manually written unit tests.

- the performance of the Flix compiler is tracked at https://arewefast.flix.dev/

## Other

Did you know that:

- Flix is developed by programming language researchers at Aarhus University (Denmark) 
  in collaboration with researchers at the University of Waterloo (Canada), at
  Eberhard Karls University of Tübingen (Germany), and by a growing open source community.

- Flix has received grants from the Independent Research Fund Denmark, 
  from Amazon Research, and from the Concordium Foundation.

- Flix has been discussed on Reddit, HackerNews, and LambdaTheUltimate.

- more than 40 people have contributed to the Flix compiler.

- more than 1,500 pull requests have been merged into the Flix compiler.
