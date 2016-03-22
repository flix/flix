// TODO
//def joinAll[M: Foldable[E], JoinSemiLattice[E]](m: M[E]): E = reduce(m, lub)
//def meetAll[M: Foldable[E], JoinSemiLattice[E]](m: M[E]): E = reduce(m, lub)


/**
 * The commutative law asserts that for a binary operator `f` the result of f(x, y) is equal to f(y, x).
 */
law Commutativity[T](f: (T, T) -> T): Prop =
    ∀(x: T, y: T). f(x, y) ≡ f(y, x)

/**
 * The associative law asserts that for a binary operator `f` the result of f(x, f(y, z)) is equal to f(f(x, y), z).
 */
law Associativity[T](f: (T, T) -> T): Prop =
    ∀(x: T, y: T, z: T). f(x, f(y, z)) ≡ f(f(x, y), z)

/**
 * The strictness law asserts that a function `f` applied to bottom must yield bottom.
 */
law Strict1[A: PartialOrder, R: PartialOrder](f: (A) -> R): Prop =
   f(⊥) ≡ ⊥

law Strict2[A: PartialOrder, B: PartialOrder, R: PartialOrder](f: (A, B) -> R): Prop =
   ∀(x: A, y: B). (f(⊥, y) ≡ ⊥) ∧ (f(x, ⊥) ≡ ⊥)

/**
 * The monotonicity law asserts that a function `f` is order-preserving.
 */
law Monotone1[A: PartialOrder, R: PartialOrder](f: (A) -> R): Prop =
    ∀(x: A, y: A). (x ⊑ y) → (f(x) ⊑ f(y))

law Monotone2[A: PartialOrder, B: PartialOrder, R: PartialOrder](f: (A, B) -> R): Prop =
    ∀(x1: A, y1: A, x2: B, y2: B). ((x1 ⊑ y1) ∧ (x2 ⊑ y2)) → (f(x) ⊑ f(y))
