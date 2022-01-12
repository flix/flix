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

- Flix's standard library is extensive.

- Flix does not have variadic functions.

- Flix does not have labelled arguments.

- Default Implementations

- Explicit Override

- Type classes can be declared sealed.

## Standard Library

Did you know that:

- Flix has minimal prelude with only 18 functions.

- All functions in the Flix prelude are total. Most functions in the Flix library are total.

- No blessed library.

- Mutable data is functional data

- Consistent names of functional and destructive operations

- Destructive operations are marked with '!'

- The Flix standard library is X lines of code.

- Flix library uses records to for certain function arguments.

## Ecosystem / Visual Studio Code

Did you know that:

## Compiler

Did you know that:

- the Flix compiler supports incremental and parallel compilation?

- The Flix compiler is X lines of the code.

- Flix has no compiler warnings, only compiler errors.
