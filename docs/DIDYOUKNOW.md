# Did You Know?

- Did you know that the most expensive compilation phases in Flix are parallel? 
  For example, Flix will type check each definition of your program in parallel.

- Did you know that Flix disallows unused variables and shadowed variables? 
  Research has shown that such redundancies are correlated with bugs.

- Did you know that the Flix type and effect system can enforce purity?
  For example, equality and comparison functions must be pure.

- Did you know that Flix supports effect polymorphism? 
  For example, the effect of `List.map` depends on the effect of its argument.

