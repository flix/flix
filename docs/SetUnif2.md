# SetUnification2: A Walkthrough

`SetUnification2` solves systems of set equations over effect formulas. It sits in front of the
Zhegalkin solver, and its relation to it is this:

> They are the same algorithm on two representations. The free-step solver is successive variable
> elimination restricted to the steps whose residual is syntactically empty, and the Zhegalkin
> solver is the same elimination with the residual kept and solved. Seeing the step in the ring
> makes the correspondence exact.

This document explains the four steps of the solver on a worked example, and then shows the step
in the ring. The code lives in
`main/src/ca/uwaterloo/flix/language/phase/unification/set/SetUnification2.scala`.

## The Example

The system below is one the Typer could produce for a function declared `\ ef + IO`, where `ef`
is a rigid effect variable from its signature. Named effects such as `IO` and `Net` are element
sets, the rigid `ef` is a constant, and `x1` to `x4` are flexible variables.

```
E1  x1 ~ x2                        a lambda's arrow effect x1 meets the callee's instantiated effect x2
E2  x3 ~ x1 ∪ IO                   the block's effect: that call plus a println
E3  (x3 ∪ x4) ∩ ¬(ef ∪ IO) ~ ∅     the body must fit the declared effect ef + IO
E4  ef ∩ ¬x4 ~ ∅                   another part of the body calls the ef-effect parameter, so ef ⊆ x4
E5  IO ∩ ¬x1 ~ ∅                   the lambda prints, so IO ⊆ x1
```

An equation with `∅` on the right is an inclusion: `s ∩ ¬t ~ ∅` says `s ⊆ t`.

## The One Rule

Read any equation as `q ~ ∅`. Solving it for a variable `x` is one step of successive variable
elimination (SVE):

```
a = q[x := ∅]      t = q[x := 𝓤]
binding   x ↦ a ∪ (x ∩ ¬t)
residual  a ∩ t ~ ∅
```

The step is *free* when the residual is `∅`. Three shapes are always free, and they are the whole
solver:

| Shape                             | Binding          | Why the residual is `∅`              |
|-----------------------------------|------------------|--------------------------------------|
| `x ~ f` with `x` not in `f`       | `x ↦ f`          | `a = f`, `t = ¬f`, so `a ∩ t = ∅`    |
| `x ∩ g ~ ∅` with `x` not in `g`   | `x ↦ x ∩ ¬g`     | `a = ∅`                              |
| `¬x ∩ g ~ ∅` with `x` not in `g`  | `x ↦ x ∪ g`      | `t = ∅`                              |

Everything else is rearranged until one of these applies, or handed to the Zhegalkin SVE where the
residuals live. Set formulas (trees of unions and intersections) are linear on unions of
variables but blow up on residuals; Zhegalkin polynomials are the reverse. The solver takes the
free steps on trees and hands what remains to polynomials.

## Step 1: Alias and Define

The solver passes over the original equalities and binds whatever is a plain alias or definition,
applying the substitution so far to each equation before looking at it. One pass here:

```
E1  x1 ~ x2            alias:   x1 ↦ x2           (the lower variable goes to the higher)
E2  x3 ~ x2 ∪ IO       define:  x3 ↦ x2 ∪ IO      (x1 already replaced; x3 is not on the right)
E3, E4, E5             neither side is a bare variable: kept
```

A second pass binds nothing, so the phase ends. Each binding removes its variable from the system,
so there is at most one pass per variable.

Doing this on the equalities, before anything is turned into inclusions, is what keeps ranges
small: `x3` leaves in one step as `x2 ∪ IO`, rather than as two inclusion constraints that would
each bind it halfway.

## Step 2: Inclusions

Each remaining equation `f1 ~ f2` becomes two constraints, `f1 ∩ ¬f2 ~ ∅` and `¬f1 ∩ f2 ~ ∅`. If
a side is already `∅` or `𝓤`, one constraint suffices, as here. Complements are pushed to the atoms
by De Morgan, so every constraint is an intersection of members:

```
C3  (x2 ∪ x4) ∩ ¬ef ∩ ¬IO       from E3 with x3 replaced; the IO inside the union vanished
C4  ef ∩ ¬x4
C5  IO ∩ ¬x2                    from E5 with x1 replaced
```

The vanishing `IO` is the complement absorption `¬a ∩ (a ∪ b) = ¬a ∩ b`, built into the formula
constructors (`SetFormula.mkInterAll` and `mkUnionAll`).

Had E2 not been a definition, say `x3 ∪ Net ~ x2 ∪ IO`, it would have become the pair
`(x3 ∪ Net) ∩ ¬x2 ∩ ¬IO ~ ∅` and `(x2 ∪ IO) ∩ ¬x3 ∩ ¬Net ~ ∅`.

## Step 3: The Worklist

Constraints sit on a stack, so the solver pops C5 first, then C4, then C3. Each popped constraint
first receives the bindings made since it was created, then one case applies. In the order of the
checks:

1. It **holds** if it is `∅`, or if its element sets are disjoint (`e1 ∩ e2 ∩ ¬e3` folds to one
   element set, which may be empty).
2. It is a **conflict** if it is `𝓤`.
3. It has a **free step** if a member `x` or `¬x` has its variable in no other member.
4. It is **ground**, and goes to the emptiness test: it holds or is a conflict.
5. It has a **union member**, and is distributed: `(g1 ∪ g2) ∩ h ~ ∅` becomes `g1 ∩ h ~ ∅` and
   `g2 ∩ h ~ ∅`. This is bounded by a budget on the constraints it creates.
6. Otherwise it is **stuck** and is parked. It is revisited when one of its variables is bound.

On the example:

```
pop C5  IO ∩ ¬x2                 free step on ¬x2, g = IO:         x2 ↦ x2 ∪ IO
pop C4  ef ∩ ¬x4                 free step on ¬x4, g = ef:         x4 ↦ x4 ∪ ef
pop C3  (x2 ∪ x4) ∩ ¬ef ∩ ¬IO    apply the two new bindings:
        (x2 ∪ IO ∪ x4 ∪ ef) ∩ ¬ef ∩ ¬IO  =  (x2 ∪ x4) ∩ ¬ef ∩ ¬IO   by absorption
                                 no member is a bare variable; a union member exists:
                                 distribute, 2 constraints created, budget 104
pop     x2 ∩ ¬ef ∩ ¬IO           free step on x2, g = ¬ef ∩ ¬IO:   x2 ↦ x2 ∩ (ef ∪ IO)
pop     x4 ∩ ¬ef ∩ ¬IO           free step on x4:                  x4 ↦ x4 ∩ (ef ∪ IO)
```

The worklist is empty, nothing is stuck, nothing was left over. Each binding is composed onto the
substitution, and the constructors keep the composed ranges flat:

```
x1, x2, x3  ↦  (x2 ∩ (ef ∪ IO)) ∪ IO       IO, plus any part of ef
x4          ↦  (x4 ∩ (ef ∪ IO)) ∪ ef       ef, plus any part of IO
```

This is the most general unifier. The `x2` and `x4` on the right are free parameters: every choice
of them satisfies all five equations, and every solution is such a choice.

### Lazy substitution

The bindings are applied lazily. Each constraint records how many bindings existed when it was
created (its `version`). C3 was created when two bindings existed and popped when four did, so it
received exactly the two newer ones. The parts split off from C3 were created at four bindings, so
the part for `x4` received only the one binding made in between.

Applying the whole substitution again would be wrong, not just slow. A range such as
`x4 ↦ x4 ∪ ef` keeps `x4` as its own parameter, so a second application yields `(x4 ∪ ef) ∪ ef`,
and a part split off along such a union would be split again on every visit. This was an infinite
loop in the first version of the solver.

### The split budget

Distribution is the only step that creates constraints. Its budget is eight per input equation
plus sixty-four (`SplitBudgetPerEquation` and `SplitBudgetBase`). A constraint with many union
members multiplies, so past the budget the remaining union-bearing constraints are set aside whole
for step 4, where unions cost nothing and variables cost everything.

## Step 4: SVE on What Remains

Here there is nothing left, which is the normal case: across a full standard library compile no
constraint reaches this step. Two things can:

- **Stuck constraints.** A constraint whose variables sit only inside symmetric differences has
  no free step and no union. If the system also had

  ```
  E6  x5 ⊕ x6 ~ IO
  ```

  step 2 would give `(x5 ⊕ x6) ∩ ¬IO ~ ∅` and `¬(x5 ⊕ x6) ∩ IO ~ ∅`, both stuck.

- **Leftovers** of an exhausted split budget.

Step 4 converts them to Zhegalkin polynomials and calls `SveAlgorithm.sveAll` at the given
`SveMode`. For E6 this returns a solution equivalent to `x5 ↦ x6 ⊕ IO`, which is composed onto the
substitution from step 3. The variable count is checked against `SetUnification.MaxVars` first.

Without symmetric differences the first three steps are complete on their own: a constraint with
no union member has only atoms as members, so a variable member either has a free step or the
constraint is ground.

## How a Type Error Surfaces

Suppose E4 had been `Net ∩ ¬x4 ~ ∅`, so that part of the body talks to the network. Then
`x4 ↦ x4 ∪ Net`, and `Net` survives in C3 because nothing excludes it:

```
C3  (x2 ∪ x4 ∪ Net) ∩ ¬ef ∩ ¬IO       distribute into three parts
    Net ∩ ¬ef ∩ ¬IO                    fold element sets: Net minus IO is Net, so  Net ∩ ¬ef
                                       ground, not empty since ef is rigid:  conflict
```

Processing stops, the substitution is discarded, and E3 is reported unsolvable with the bindings
found so far applied, which is the message that the body's effect does not fit `ef + IO`.

The emptiness test is the one place the main path still uses Zhegalkin: it decides whether a
ground formula over constants and element sets denotes `∅`.

## The Same Step in the Ring

A Zhegalkin polynomial is a sum of monomials, each a coefficient from the cofinite sets
intersected with a product of variables. Rigid constants such as `ef` are variables that are never
eliminated, element sets such as `IO` are coefficients, and the only connectives are `⊕` and `∩`.
Every polynomial is linear in each variable, so for any `x`:

```
q = a ⊕ (x ∩ b)          a, b free of x
q[x := ∅] = a            q[x := 𝓤] = a ⊕ b
binding    x ↦ a ∪ (x ∩ ¬(a ⊕ b))  =  a ⊕ (x ∩ ¬a ∩ ¬b)
residual   a ∩ (a ⊕ b)  =  a ∩ ¬b
```

The residual vanishes in exactly two cases. If `a = ∅` then `q = x ∩ b`, so `x` is a factor of the
whole polynomial, and the binding collapses to `x ↦ x ∩ ¬b`. If `b = a` then
`q = a ⊕ (x ∩ a) = a ∩ ¬x`, so `¬x` is a factor, and the binding collapses to `x ↦ x ∪ a`. These
are the two free shapes of step 3, with `g` in the role of `b` and of `a`. A top-level member `x`
of an intersection is the tree's way of saying "`x` divides `q`", and a member `¬x` says "`¬x`
divides `q`". The polynomial can express the same facts, as "every monomial contains `x`" or "`q`
has the form `a ⊕ x·a`", but nothing in its normal form points at them, and after one substitution
of a range such as `x ∩ ¬g` the factor structure is spread over the monomials.

### Why two representations

The step is identical, so the whole difference is which operations the representation makes cheap.

The polynomial is canonical: two formulas are equal iff their polynomials are, and `q = ∅` is a
syntactic check that all coefficients are zero. That is what makes the residual harmless there,
since `a ∩ ¬b` is just another polynomial and the question "is it already solved" is free. The
price is union. `a ∪ b` is `a ⊕ b ⊕ (a ∩ b)`, so a union of `n` variables has `2^n − 1` monomials,
and `¬` is `𝓤 ⊕ ·`, which flips everything.

The tree is the reverse. A union of `n` variables has `n` leaves and a complement is one De Morgan
step, but there is no canonical form, so emptiness is not visible and a residual can only grow.
Normal forms such as NNF or DNF give the tree a canonical shape at the cost of the same
exponential the polynomial pays on products of unions. The present design stops trying: the tree
solver never produces a residual. Free steps have none, distribution is an equivalence, the ground
test converts to a polynomial where zero is syntactic, and whatever would have a residual is
converted whole.

### Where the xor sits

The tree solver is "SVE without the xor" in two ways. First, `⊕` is the ring's addition, and
turning everything into a sum is precisely what buys canonicity and loses the factors. The tree
keeps `∪` as a connective, which keeps factors visible, which is what the free step needs.

Second, the fragment the tree solver handles is the monotone one: in an inclusion `s ∩ ¬t ~ ∅`,
every variable occurs under `∩` and `∪` as `x` or as `¬x`, and the constraint is monotone or
antimonotone in each. A free step is an inclusion, `x ⊆ ¬g` or `g ⊆ x`. Symmetric difference is
the one connective that is neither monotone nor antimonotone in its arguments. `x ⊕ c ~ ∅` is not
an inclusion but two, `x ⊆ c` and `c ⊆ x`, so a variable inside an xor is invisible to the free
step and the solver parks the constraint.

It does not have to. The identity `a ⊕ b = (a ∩ ¬b) ∪ (¬a ∩ b)` says an xor member is a union of
two intersections, so it could be distributed like any other union, at the cost of one split for a
binary xor and `2^(n−1)` parts for an n-ary one. The stuck example of step 4 would become

```
(x5 ⊕ x6) ∩ ¬IO ~ ∅   →   x5 ∩ ¬x6 ∩ ¬IO ~ ∅      free:  x5 ↦ x5 ∩ (x6 ∪ IO)
                           ¬x5 ∩ x6 ∩ ¬IO ~ ∅      free after substitution
```

and the only remaining reason for a hand-off would be the split budget. The solver does not do
this because xor reaches it rarely, from `SymmetricDiff` types and from ranges converted back out
of Zhegalkin results, and the hand-off handles it correctly.

### Same answer, different syntax

Both solvers return reproductive solutions: the eliminated variable survives as the free parameter
of its own range, `a ∪ (x ∩ ¬t)`. The Zhegalkin solver eliminates variables in a fixed sequence,
each residual feeding the next; the tree solver has no sequence and takes whichever free step a
constraint exposes, and because distribution preserves equivalence, the order changes only the
syntax of the result. The two results are equal as set functions, one written with unions, the
other with xors. This is why the printed types do not depend on which solver ran: the type
simplifier round-trips every effect through the polynomial anyway.

### Where they meet in the code

- The ground emptiness test converts a variable-free formula to a polynomial for its zero test.
- Stuck and leftover constraints are converted for the full elimination, and their solution comes
  back through `Zhegalkin.toSetFormula`, which is the only way an `Xor` node enters a range.
- The type simplifier uses the same round trip for display.

## Complexity

- Step 1 makes one pass over the equations per pass that binds a variable, so at most one pass
  per variable.
- Step 3 creates at most the budget many constraints by distribution, and every other step closes
  or parks the constraint it takes up. The number of steps is bounded by the constraints created
  plus one revisit of a stuck constraint per binding.
- Step 4 is exponential in the variables of the equations it receives, guarded by `MaxVars`.
