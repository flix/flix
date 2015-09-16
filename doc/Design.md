# Design #
The purpose of this document is to outline and justify the design choices made during the development of Flix.

# Constraints #
A Flix program is an collecton of a horn clauses:

A _horn clause_ is a logical formula of the form:

```
  H(...) :- A(...), B(...), ...
```

In order to all increase the expressivity of horn clauses are generalized to constraint horn clauses (CHC) of the form:

```
  H(...) :- c, A(...), B(...), ...
```

where c is a conjunction of boolean valued functions: `f1(...) /\ f2(...) /\ ....`

A special case is the function f(x, y) = x != y (when is this monotone?)

Aggregation:

```
  H(...) :- A(...) |> xs = f(x, y), B(...) |> g, ...
```

where f and g are (strict, monotone) folds: `B -> ((A, B) -> B) -> B.`

Unfolding:

```
  H(...) :- x = A(xs) |> x = unfold(xs)
```
