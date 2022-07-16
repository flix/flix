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
/// A type class for data structures that can be traversed with an applicative functor.
///
pub class Traversable[t : Type -> Type] with Functor[t], Foldable[t] {

    ///
    /// Returns the result of applying the applicative mapping function `f` to all the elements of the
    /// data structure `t`.
    ///
    pub def traverse(f: a -> m[b] & ef, t: t[a]): m[t[b]] & ef with Applicative[m]

    ///
    /// Returns the result of running all the actions in the data structure `t`.
    ///
    pub def sequence(t: t[m[a]]): m[t[a]] with Applicative[m] =
        Traversable.traverse(identity, t)

    ///
    /// Traversing with the identity function wrapped into an applicative preserves the container `t`.
    ///
    law identity: forall(t: t[a], f: a -> m[a]) with Eq[m[t[a]]], Applicative[m] . Traversable.traverse(f, t) == Applicative.point(t)

    ///
    /// sequence identity.
    ///
    law identity: forall(t: t[a], f: a -> m[a]) with Eq[m[t[a]]], Applicative[m] . Traversable.sequence(Functor.map(f, t)) == Applicative.point(t)

    // Missing laws: naturality and composition.

}


namespace Traversable {

    ///
    /// Returns the result of applying the applicative mapping function `f` to all the elements of the
    /// data structure `t`.
    ///
    /// `for` is `traverse` with it's arguments flipped.
    ///
    pub def for(t: t[a], f: a -> m[b] & ef): m[t[b]] & ef with Applicative[m], Traversable[t] =
        Traversable.traverse(f, t)

    ///
    /// Returns the result of applying `f` to the traversable structure `t` and the initial state `acc`.
    /// The result is a pair of the final state and the updated copy of the structure.
    ///
    /// `mapAccumLeft` is essentially the combination of `map` and `foldLeft` - like map it returns an updated copy
    /// of the initial structure, like `foldLeft` it passes an updating accumulator through each step of the traversal.
    ///
    pub def mapAccumLeft(f: (acc, a) -> (acc, b) & ef, start: acc, t: t[a]): (acc, t[b]) & ef with Traversable[t] =
        runContStateLeft(traverse(x1 -> ContStateLeft((k, s) -> let (s1, b) = f(s, x1); k(b, s1)), t), start) as & ef

    ///
    /// Helper type for `mapAccumLeft`.
    ///
    /// ContStateLeft the type of the State monad implemented in CPS. In this case we just need
    /// `Applicative.ap` so no Monad instance is defined.
    ///
    /// A simple implementation of the State monad bursts the stack when `mapAccumLeft` is run on large
    /// collections, so use ContState the CPS-State monad instead.
    ///
    /// We use an Impure continuation so we can internally cast the `ef` of `mapAccumLeft` and not lose
    /// impurity information.
    ///
    enum ContStateLeft[ef: Eff, ka: Type, s: Type, a: Type]((a -> s -> ka & Impure \ ef) -> s -> ka & Impure \ ef)

    ///
    /// Helper function for ContStateLeft's implementations of `map` and `ap`.
    ///
    def applyLeft1(ma: ContStateLeft[ef, ka, s, a], st: s, cont: a -> s -> ka & Impure \ ef): ka & Impure \ ef =
        let ContStateLeft(f) = ma;
        f(cont, st)

    ///
    /// Returns the result (new state and answer) of applying `ma` to the initial state `st`.
    ///
    def runContStateLeft(ma: ContStateLeft[ef, (s, a), s, a], st: s): (s, a) & Impure \ ef =
        let ContStateLeft(f) = ma;
        let cont = (a, s) -> (s, a) as & Impure \ ef;
        f(cont, st)


    instance Functor[ContStateLeft[ef1, ka, s]] {
        pub def map(f: a -> b & ef, ma: ContStateLeft[ef1, ka, s, a]): ContStateLeft[ef1, ka, s, b] & ef =
            upcast(() -> ContStateLeft((k, s) ->
                applyLeft1(ma, s, (a, s1) -> k(f(a) as & Impure, s1))
            ))()
    }

    instance Applicative[ContStateLeft[ef1, ka, s]] {
        pub def point(x: a): ContStateLeft[ef1, ka, s, a] =
            ContStateLeft((k, s) -> k(x, s))

        pub def ap(mf: ContStateLeft[ef1, ka, s, a -> b & ef], ma: ContStateLeft[ef1, ka, s, a]): ContStateLeft[ef1, ka, s, b] & ef =
            upcast(() -> ContStateLeft((k, s) ->
                applyLeft1(mf, s, (f, s1) ->
                    applyLeft1(ma, s1, (a, s2) ->
                        k(f(a) as & Impure, s2)))
            ))()
    }

    ///
    /// Returns the result of applying `f` to the traversable structure `t` and the initial state `acc`.
    /// The result is a pair of the final state and the updated copy of the structure.
    ///
    /// `mapAccumRight` is essentially the combination of `map` and `foldLeft` - like map it returns an updated copy
    /// of the intial structure, like `foldRight` it passes an updating accumulator through each step of the traversal.
    ///
    pub def mapAccumRight(f: (acc, a) -> (acc, b) & ef, start: acc, t: t[a]): (acc, t[b]) & ef with Traversable[t] =
        runContStateRight(traverse(x1 -> ContStateRight((k, s) -> let (s1, b) = f(s, x1); k(b, s1)), t), start) as & ef


    ///
    /// Helper type for `mapAccumRight`.
    ///
    /// ContStateRight is exactly the same as ContStateLeft except the evaluation order of `ap` is flipped.
    ///
    enum ContStateRight[ef: Eff, ka: Type, s: Type, a: Type]((a -> s -> ka & Impure \ ef) -> s -> ka & Impure \ ef)

    ///
    /// Helper function for ContStateRight's implementations of `map` and `ap`.
    ///
    def applyRight1(ma: ContStateRight[ef, ka, s, a], st: s, cont: a -> s -> ka & Impure \ ef): ka & Impure \ ef =
        let ContStateRight(f) = ma;
        f(cont, st)

    ///
    /// Returns the result (new state and answer) of applying `ma` to the initial state `st`.
    ///
    def runContStateRight(ma: ContStateRight[ef, (s, a), s, a], st: s): (s, a) & Impure \ ef =
        let ContStateRight(f) = ma;
        let cont = (a, s) -> (s, a) as & Impure \ ef;
        f(cont, st)


    instance Functor[ContStateRight[ef1, ka, s]] {
        pub def map(f: a -> b & ef, ma: ContStateRight[ef1, ka, s, a]): ContStateRight[ef1, ka, s, b] & ef =
            upcast(() -> ContStateRight((k, s) ->
                applyRight1(ma, s, (a, s1) -> k(f(a) as & Impure, s1))
            ))()
    }

    instance Applicative[ContStateRight[ef1, ka, s]] {
        pub def point(x: a): ContStateRight[ef1, ka, s, a] = ContStateRight((k, s) -> k(x, s))

        pub def ap(mf: ContStateRight[ef1, ka, s, a -> b & ef], ma: ContStateRight[ef1, ka, s, a]): ContStateRight[ef1, ka, s, b] & ef =
            upcast(() -> ContStateRight((k, s) ->
                applyRight1(ma, s, (a, s1) ->
                    applyRight1(mf, s1, (f, s2) ->
                        k(f(a) as & Impure, s2)))
            ))()
    }

}

