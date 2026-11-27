# SetUnification2: A Walkthrough

`SetUnification2` solves systems of set equations over effect formulas. It sits in front of the
Zhegalkin solver, and its relation to it is this:

> They are the same algorithm on two representations. The free-step solver is successive variable
> elimination restricted to the steps whose residual is syntactically empty, and the Zhegalkin
> solver is the same elimination with the residual kept and solved. Seeing the step in the ring
> makes the correspondence exact.

This document explains the four steps of the solver on a worked example, and then shows the step
in the ring.

## The Example

The system below is one the Typer could produce for a function declared `\ ef + IO`, where `ef`
is a rigid effect variable from its signature. Named effects such as `IO` and `Net` are element
sets, the rigid `ef` is a constant, and `x1` to `x4` are flexible variables.

```
E1  x1 ~ x2                       a lambda's arrow effect x1 meets the callee's instantiated effect x2
E2  x3 ~ x1 ∪ IO                  the block's effect: that call plus a println
E3  (x3 ∪ x4) ∩ ¬(ef ∪ IO) ~ ∅    the body must fit the declared effect ef + IO
E4  ef ∩ ¬x4 ~ ∅                  another part of the body calls the ef-effect parameter, so ef ⊆ x4
E5  IO ∩ ¬x1 ~ ∅                  the lambda prints, so IO ⊆ x1
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

The equations are first sorted by size, smallest first, which puts them in the order E1, E2, E4,
E5, E3. When a variable has several definitions, the first one binds it and the others become
constraints, so this makes the smallest definition the one that is copied into every occurrence
of the variable. Equations of equal size keep their order.

The solver then passes over the equalities and binds whatever is a plain alias or definition,
applying the substitution so far to each equation before looking at it. One pass here:

```
E1  x1 ~ x2            alias:   x1 ↦ x2           (the lower variable goes to the higher)
E2  x3 ~ x2 ∪ IO       define:  x3 ↦ x2 ∪ IO      (x1 already replaced; x3 is not on the right)
E4, E5, E3             neither side is a bare variable: kept
```

A second pass binds nothing, so the phase ends. Each binding removes its variable from the system,
so there is at most one pass per variable.

Doing this on the equalities, before anything is turned into inclusions, is what keeps ranges
small: `x3` leaves in one step as `x2 ∪ IO`, rather than as two inclusion constraints that would
each bind it halfway.

## Step 2: Inclusions

Each remaining equation `f1 ~ f2` becomes two constraints, `f1 ∩ ¬f2 ~ ∅` and `¬f1 ∩ f2 ~ ∅`. If
a side is `∅` or `𝓤`, one of the two simplifies to `∅` and is dropped, as here. Complements are
pushed to the atoms by De Morgan, so every constraint is an intersection of members:

```
C4  ef ∩ ¬x4
C5  IO ∩ ¬x2                    from E5 with x1 replaced
C3  (x2 ∪ x4) ∩ ¬ef ∩ ¬IO       from E3 with x3 replaced; the IO inside the union vanished
```

The vanishing `IO` is the complement absorption `¬a ∩ (a ∪ b) = ¬a ∩ b`, built into the formula
constructors (`SetFormula.mkInterAll` and `mkUnionAll`).

Had E2 not been a definition, say `x3 ∪ Net ~ x2 ∪ IO`, it would have become the pair
`(x3 ∪ Net) ∩ ¬x2 ∩ ¬IO ~ ∅` and `(x2 ∪ IO) ∩ ¬x3 ∩ ¬Net ~ ∅`.

## Step 3: The Worklist

Constraints sit on a stack, and those of the largest equation are pushed last, so the solver pops
C3 first, then C5, then C4. A binding is applied to every waiting constraint when it is made, so a
popped constraint is up to date. One case applies to it. In the order of the checks:

1. It has a **free step** if a member `x` or `¬x` has its variable in no other member.
2. It is **ground**, and goes to the emptiness test: it holds, or it is a conflict and the solve
   ends.
3. It has a **union member**, and is distributed: `(g1 ∪ g2) ∩ h ~ ∅` becomes `g1 ∩ h ~ ∅` and
   `g2 ∩ h ~ ∅`. This is bounded by a budget on the constraints it creates.
4. Otherwise it is **left whole** for step 4.

On the example:

```
pop C3  (x2 ∪ x4) ∩ ¬ef ∩ ¬IO     no member is a bare variable; a union member exists:
                                  distribute, 2 constraints created, budget 104
pop     x2 ∩ ¬ef ∩ ¬IO            free step on x2, g = ¬ef ∩ ¬IO:   x2 ↦ x2 ∩ (ef ∪ IO)
pop     x4 ∩ ¬ef ∩ ¬IO            free step on x4:                  x4 ↦ x4 ∩ (ef ∪ IO)
pop C5  IO ∩ (¬x2 ∪ (¬ef ∩ ¬IO))  C5 with the binding of x2 applied: distribute
pop     ¬x2 ∩ IO                  free step on ¬x2, g = IO:         x2 ↦ x2 ∪ IO
pop     ∅                         the other part, IO ∩ ¬ef ∩ ¬IO: holds
pop C4  ef ∩ (¬x4 ∪ (¬ef ∩ ¬IO))  C4 with the binding of x4 applied: distribute
pop     ¬x4 ∩ ef                  free step on ¬x4, g = ef:         x4 ↦ x4 ∪ ef
pop     ∅                         the other part, ef ∩ ¬ef ∩ ¬IO: holds
```

The worklist is empty and nothing was left whole. Each binding is composed onto the substitution,
and the constructors keep the composed ranges flat:

```
x1, x2  ↦  (x2 ∪ IO) ∩ (ef ∪ IO)            IO, plus any part of ef
x3      ↦  ((x2 ∪ IO) ∩ (ef ∪ IO)) ∪ IO     the same set
x4      ↦  (x4 ∪ ef) ∩ (ef ∪ IO)            ef, plus any part of IO
```

This is the most general unifier. The `x2` and `x4` on the right are free parameters: every choice
of them satisfies all five equations, and every solution is such a choice.

### Applying a binding once

A binding is applied to the waiting constraints when it is made, and never again. C5 received the
binding `x2 ↦ x2 ∩ (ef ∪ IO)` while it waited on the stack, and the parts split off from it later
start from that formula.

Bringing a constraint up to date with the whole substitution instead would be wrong, not just
slow. A range such as `x4 ↦ x4 ∪ ef` keeps `x4` as its own parameter, so a second application
yields `(x4 ∪ ef) ∪ ef`, and a part split off along such a union would be split again on every
visit.

### The split budget

Distribution is the only step that creates constraints, so it is bounded by a budget. Past the
budget, a constraint that would be split is set aside whole for step 4. See
[What the split budget does](#what-the-split-budget-does).

## Step 4: SVE on What Remains

Here there is nothing left, which is the normal case: across a full standard library compile no
constraint reaches this step. Two things can:

- **Symmetric differences.** A constraint whose variables sit only inside symmetric differences
  has no free step and no union. If the system also had

  ```
  E6  x5 ⊕ x6 ~ IO
  ```

  step 2 would give `(x5 ⊕ x6) ∩ ¬IO ~ ∅` and `¬(x5 ⊕ x6) ∩ IO ~ ∅`, and both are left whole.

- **Leftovers** of an exhausted split budget.

Step 4 converts them to Zhegalkin polynomials and calls `SveAlgorithm.sveAll` at the given
`SveMode`. For E6 this returns a solution equivalent to `x5 ↦ x6 ⊕ IO`, which is composed onto the
substitution from step 3. The variable count is checked against `SetUnification.MaxVars` first.

Without symmetric differences the first three steps are complete on their own: a constraint with
no union member has only atoms as members, so a variable member either has a free step or the
constraint is ground.

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
step and the solver leaves the constraint whole.

It does not have to. The identity `a ⊕ b = (a ∩ ¬b) ∪ (¬a ∩ b)` says an xor member is a union of
two intersections, so it could be distributed like any other union, at the cost of one split for a
binary xor and `2^(n−1)` parts for an n-ary one. The example of step 4 would become

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

## Complexity

Let `n` be the number of input equations and `v` the number of variables they mention.

- **Step 1** makes passes over the equations until a pass binds nothing. Every binding removes its
  variable from the system, so there are at most `v + 1` passes of `n` substitutions each.
- **Step 2** turns every remaining equation into at most two constraints, so step 3 starts with at
  most `2n` constraints.
- **Step 3** takes one constraint off the worklist at a time and does exactly one thing with it:
  closes it (ground and empty, or solved by a free step), splits it, sets it aside whole, or
  reports a conflict and stops. Only a split adds constraints, and the split budget caps how
  many, so step 3 sees at most `2n + B` constraints in total, where `B` is the budget below. A
  free step closes the constraint it comes from, so step 3 binds at most that many times, and
  every binding is applied to the constraints still waiting: quadratic in `2n + B`, which is
  itself linear in `n`. A ground constraint also pays for the emptiness test, which goes through
  a Zhegalkin polynomial and can be exponential in the rigid constants it mentions.
- **Step 4** is exponential in the variables of the constraints it receives. Before it runs,
  each constraint is checked against `SetUnification.MaxVars` (currently 11), and a constraint
  with more variables is reported as too complex instead.

Steps 1 to 3 are therefore polynomial, and all the exponential cost sits in step 4 and in the
ground emptiness test. The split budget is what keeps step 3 polynomial: without it, a
constraint with `k` unions of `m` members each distributes into `m^k` parts.

### What the split budget does

```scala
private val SplitBudgetPerEquation: Int = 8
private val SplitBudgetBase: Int = 64
```

The budget of one solve is `8n + 64`, so it grows with the size of the system and leaves room
for a few splits even in a system of one equation. It counts constraints created by
distribution: splitting a union of `k` members adds `k` to the count. The check comes before
each split, so the last split may overshoot the budget by the width of one union.

Once the budget is spent, distribution stops, but nothing else does. Free steps and ground tests
work as before. A constraint that would otherwise be split goes to step 4
whole, where unions cost nothing and variables cost everything. The budget does not change the
answer when step 4 can solve what is set aside. It only moves work from step 3 to step 4, and the
cost is that the `MaxVars` limit may now reject a system that distribution would have solved.

### How often the budget is hit

The numbers below come from counting every call to `SetUnification2.solve` on top of master
`3f42e7e89`, first while compiling the standard library (an empty program), then over one run of
the test suite.

| | Standard library | Test suite |
|---|---:|---:|
| Solves | 4,564 | 504,085 |
| Equations per solve (median / p99 / max) | 3 / 45 / 230 | 2 / 16 / 230 |
| Solves with no split at all | 4,431 (97.1%) | 499,293 (99.0%) |
| Solves with more than 64 splits | 0 | 4 |
| Most splits in one solve | 15 | 120 |
| Most of its budget used by one solve | 17% (15 splits) | 33% (24 splits) |
| Solves that exhaust the budget | 0 | 0 |
| Solves that reach step 4 | 0 | 0 |

Distribution is rare: about one solve in thirty splits at all in the standard library, one in a
hundred in the test suite, and no solve uses more than a third of its budget. No solve in either
run reached step 4. On this code base the budget is a safety net against blow-up, not something
ordinary programs run into.
