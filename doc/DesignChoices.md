Design Choices
==============
The purpose of this document is to outline and justify the design choices made during the development of Flix.

Logical Foundation
==================
A Flix program is an unordered collection horn clauses over a set of relations.

A ''horn clause'' is a logical formula of the form:

  H(...) :- A(...), B(...), ...

In order to all increase the expressivity of horn clauses are generalized to constraint horn clauses (CHC) of the form:

  H(...) :- c, A(...), B(...), ...

where c is a conjunction of boolean valued functions: f1(...) /\ f2(...) /\ ....

Each function f1, f2, ...

A special case is the function f(x, y) = x != y (when is this monotone?)

Aggregation:

  H(...) :- A(...) |> xs = f(x, y), B(...) |> g, ...

where f and g are (strict, monotone) folds: B -> ((A, B) -> B) -> B.

Unfolding:

  H(...) :- x = A(xs) |> x = unfold(xs)

Negation
--------
Flix inherits all the challenges associated with negation present in Datalog. Concretely, consider a clause:

  ```A(x) :- !A(x).```

such unrestricted use of negation immediately leads into inconsistency.

Relation to antitone, if any? "not top" is a big problem for e.g. cst.
Unless not top is interpreted as some single elm "e"????

Look more closely at the Fixpoint Semantics paper and its relation to stratified negation.
Implementation needs to maintain two booleans: truthy, falsy. How does inference work?
Does every fact need to be tagged with how it was infered? (or at least the negated premises?)

Evaluation Strategies
=====================

Naive Evaluation Strategy
=========================


Efficient Evaluation Strategy
=============================

Parallel Evaluation Strategy
============================


References
==========
