/*
 *  Copyright 2022 Jakob Schneider Villumsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod UnorderedFoldable {

    ///
    /// A trait for unordered data structures that can be folded.
    ///
    pub trait UnorderedFoldable[t: Type -> Type] {
        ///
        /// The associated effect of the UnorderedFoldable which represents the
        /// effect of accessing its elements.
        ///
        type Aef[m]: Eff = {}

        ///
        /// Unordered fold of `t`.
        ///
        /// Applies `f` to all elements in `t` and combines the results
        /// i.e. `CommutativeMonoid.combine(f(a))` for all `a` in `t`.
        ///
        pub def foldMap(f: a -> b \ ef, t: t[a]): b \ (ef + UnorderedFoldable.Aef[t]) with CommutativeMonoid[b]

        ///
        /// Returns the result of applying `CommutativeMonoid.combine` to all the elements in `t`.
        ///
        pub def fold(t: t[a]): a \ UnorderedFoldable.Aef[t] with CommutativeMonoid[a] =
            UnorderedFoldable.foldMap(identity, t)

        ///
        /// Returns true if and only if `t` is empty.
        ///
        pub def isEmpty(t: t[a]): Bool \ UnorderedFoldable.Aef[t] =
            UnorderedFoldable.forAll(_ -> false, t)

        ///
        /// Returns true if and only if `t` is non-empty.
        ///
        pub def nonEmpty(t: t[a]): Bool \ UnorderedFoldable.Aef[t] = not UnorderedFoldable.isEmpty(t)

        ///
        /// Returns `true` if and only if at least one element in `t` satisfies the predicate `f`.
        ///
        /// Returns `false` if `t` is empty.
        ///
        pub def exists(f: a -> Bool \ ef, t: t[a]): Bool \ (ef + UnorderedFoldable.Aef[t]) =
            use SemiGroup.Any.Any;
            let Any(x) = UnorderedFoldable.foldMap(a -> Any(f(a)), t);
            x

        ///
        /// Returns `true` if and only if all elements in `t` satisfy the predicate `f`.
        ///
        /// Returns `true` if `t` is empty.
        ///
        pub def forAll(f: a -> Bool \ ef, t: t[a]): Bool \ (ef + UnorderedFoldable.Aef[t]) =
            use SemiGroup.All.All;
            let All(x) = UnorderedFoldable.foldMap(a -> All(f(a)), t);
            x

        ///
        /// Returns `true` if and only if the element `x` is in `t`.
        ///
        pub def memberOf(x: a, t: t[a]): Bool \ UnorderedFoldable.Aef[t] with Eq[a] =
            UnorderedFoldable.exists(y -> x == y, t)

        ///
        /// Returns the number of elements in `t`.
        ///
        pub def size(t: t[a]): Int32 \ UnorderedFoldable.Aef[t] =
            UnorderedFoldable.foldMap(_ -> 1, t)

        ///
        /// Returns the number of elements in `t` that satisfy the predicate `f`.
        ///
        pub def count(f: a -> Bool \ ef, t: t[a]): Int32 \ (ef + UnorderedFoldable.Aef[t]) =
            UnorderedFoldable.foldMap(a -> if (f(a)) 1 else 0, t)

    }

}
