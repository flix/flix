# Did You Know?

- Did you know that the most expensive compilation phases in Flix are parallel? 
  For example, Flix will type check each definition of your program in parallel.

- Did you know that Flix disallows unused variables and shadowed variables? 
  Research has shown that such redundancies are correlated with bugs.

- Did you know that the Flix type and effect system can enforce purity?
  For example, equality and comparison functions must be pure.

- Did you know that Flix supports effect polymorphism? 
  For example, the effect of `List.map` depends on the effect of its argument.

TODO: Divide into categories, e.g. compiler, languages, std lib, eco system.

TODO

- Flix has no null value.

- Everything in Flix is private by default.

- Flix has no implicit coercions (but does provide several explicit coercion functions).

- All functions in the Flix prelude are total. Most functions in the Flix library are total.

- In Flix division by zero is zero.

- Flix has no warnings, only errors.

- In Flix, nothing in is executed before main.

- Flix has no global state.

- Flix does not allow unused declarations.

- Flix is one language. There is no compiler flag that enables or disables features.

- Flix has no overloading. Functions are given useful names instead. (But Flix has type classes).

- Flix's standard library is extensive.

- The Flix compiler is X lines of the code.

- The Flix standard library is X lines of code.

- Flix does not have variadic functions.

- Flix does not have labelled arguments.

- Flix supports type parameter elision.

- Flix has no binary or octal literals.

- FLix aims for a timeless design.


