/*
 * Copyright 2021 Stephen Tetley
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Traversable {

    ///
    /// A trait for data structures that can be traversed in left-to-right
    /// order with an applicative functor.
    ///
    pub trait Traversable[t: Type -> Type] with Functor[t], Foldable[t] {

        ///
        /// Returns the result of applying the applicative mapping function `f` to all the elements of the
        /// data structure `t`.
        ///
        pub def traverse(f: a -> m[b] \ ef, t: t[a]): m[t[b]] \ (ef + Foldable.Aef[t]) with Applicative[m]

        ///
        /// Returns the result of running all the actions in the data structure `t`.
        ///
        pub def sequence(t: t[m[a]]): m[t[a]] \ Foldable.Aef[t] with Applicative[m] =
            Traversable.traverse(identity, t)
    }

    ///
    /// Returns the result of applying the applicative mapping function `f` to all the elements of the
    /// data structure `t`.
    ///
    /// `for` is `traverse` with its arguments flipped.
    ///
    pub def for(t: t[a], f: a -> m[b] \ ef): m[t[b]] \ (ef + Foldable.Aef[t]) with Applicative[m], Traversable[t] =
        Traversable.traverse(f, t)

    ///
    /// Returns the result of applying `f` to the traversable structure `t` and the initial state `acc`.
    /// The result is a pair of the final state and the updated copy of the structure.
    ///
    /// `mapAccumLeft` is essentially the combination of `map` and `foldLeft` - like map it returns an updated copy
    /// of the initial structure, like `foldLeft` it passes an updating accumulator through each step of the traversal.
    ///
    pub def mapAccumLeft(f: (acc, a) -> (acc, b) \ ef, start: acc, t: t[a]): (acc, t[b]) \ (ef + Foldable.Aef[t]) with Traversable[t] =
        region rc {
            let acc = Ref.fresh(rc, start);
            let Identity.Identity(tb) = t
                |> Traversable.traverse(
                    x -> {
                        let st = Ref.get(acc);
                        let (acc1, b) = f(st, x);
                        Ref.put(acc1, acc);
                        Identity.Identity(b)
                    }
                );
            (Ref.get(acc), tb)
        }

}
