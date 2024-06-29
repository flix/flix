/*
 * Copyright 2021 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

///
/// A trait for data structures that can be traversed in left-to-right
/// order with an applicative functor.
///
pub trait Traversable[t : Type -> Type] with Functor[t], Foldable[t] {

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

    ///
    /// Traversing with the identity function wrapped into an applicative preserves the container `t`
    /// where accessing elements within `t` is pure.
    ///
    law identity: forall(t: t[a], f: a -> m[a]) with Eq[m[t[a]]], Applicative[m] where Foldable.Aef[t] ~ {} Traversable.traverse(f, t) == Applicative.point(t)

    ///
    /// sequence identity.
    ///
    law identity: forall(t: t[a], f: a -> m[a]) with Eq[m[t[a]]], Applicative[m] where Foldable.Aef[t] ~ {} Traversable.sequence(Functor.map(f, t)) == Applicative.point(t)

    // Missing laws: naturality and composition.

}


mod Traversable {

    ///
    /// Returns the result of applying the applicative mapping function `f` to all the elements of the
    /// data structure `t`.
    ///
    /// `for` is `traverse` with it's arguments flipped.
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
            let acc = ref start @ rc;
            let Identity.Identity(tb) = t |> Traversable.traverse(x -> {
                let st = deref acc;
                let (acc1, b) = f(st, x);
                Ref.put(acc1, acc);
                Identity.Identity(b)
            });
            (deref acc, tb)
        }

}

