/*
 *  Copyright 2021 Felix Wiemuth
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

///
/// A type class for applicatives that support monadic bind (`flatMap`), i.e., allow to apply a function
/// that takes a normal value and produces a monadic value to a monadic value. That is, the bind mechanism
/// supports extraction of monadic values, or, viewed differently, allows to combine (flatten) nested
/// monadic values (`flapMap` can be understood as a `Functor.map` followed by a `flatten`).
///
pub lawful class Monad[m : Type -> Type] with Applicative[m] {

    ///
    /// Apply function `f` to the monadic value `x`, resulting in a combined monadic value.
    ///
    pub def flatMap(f: a -> m[b] \ ef, x: m[a]) : m[b] \ ef

    ///
    /// Applying `flatMap` to the `point` function is the identity function.
    ///
    law leftIdentity: forall(f: a -> m[b], x: m[a]) with Eq[m[a]] . Monad.flatMap(Applicative.point, x) == x

    ///
    /// Applying `flatMap` to a function and to a non-monadic argument after applying `point` to it is the same
    /// as applying the function directly to that argument.
    ///
    law rightIdentity: forall(f: a -> m[b], x: a) with Eq[m[b]] . Monad.flatMap(f, Applicative.point(x)) == f(x)

    ///
    /// Subsequent `flatMap` calls (nested in the monadic argument) can be moved to the outside by
    /// providing a lambda as first argument which then calls `flatMap`.
    ///
    law associativity: forall(x: m[a], f: a -> m[b], g: b -> m[c]) with Eq[m[c]] . Monad.flatMap(g, Monad.flatMap(f, x)) == Monad.flatMap(y -> Monad.flatMap(g, f(y)), x)

    ///
    /// Applicatively applying a monadic function to a monadic argument is the same as using `flatMap`
    /// to extract function and argument, applying them normally and then applying `point` to the result.
    ///
    law apCorrespondence: forall(f: m[a -> b], x: m[a]) with Eq[m[b]] . Applicative.ap(f, x) == Monad.flatMap(g -> Monad.flatMap(y -> Applicative.point(g(y)), x), f)
}


mod Monad {

    ///
    /// The monadic `join` operator.
    /// Flatten `x` - a monadic action nested in an outer monadic layer - to a single layer.
    ///
    /// E.g. for the Option monad: `flatten(Some(Some(1)))` becomes `Some(1)`.
    ///
    pub def flatten(x: m[m[a]]): m[a] with Monad[m] = flatMap(identity, x)

    ///
    /// The left-to-right Kleisli composition operator for monads.
    ///
    /// Map `x` with the monadic function `f1` and then map its result with the function `f2`.
    ///
    pub def kleisliLeft(f1: a -> m[b] \ ef1, f2: b -> m[c] \ ef2, x: a): m[c] \ { ef1, ef2 } with Monad[m] =
        flatMap(x1 -> f2(x1), f1(x))

    ///
    /// The right-to-left Kleisli composition operator for monads.
    ///
    /// Map `x` with the monadic function `f2` and then map its result with the function `f1`.
    ///
    pub def kleisliRight(f1: b -> m[c] \ ef1, f2: a -> m[b] \ ef2, x: a): m[c] \ { ef1, ef2 } with Monad[m] =
        flatMap(x1 -> f1(x1), f2(x))


    ///
    /// `=<<` is an operator alias for `flatMap`.
    ///
    pub def =<<(k: a -> m[b] \ ef, x: m[a]): m[b] \ ef with Monad[m] = flatMap(k, x)

    ///
    /// `>>=` is the operator `=<<` with its arguments flipped.
    ///
    /// `>>=` is the monadic bind operator.
    ///
    pub def >>=(x: m[a], k: a -> m[b] \ ef): m[b] \ ef with Monad[m] = flatMap(k, x)

    ///
    /// `>=>` is an operator alias for `kleisliLeft`.
    ///
    pub def >=>(f1: a -> m[b] \ ef1, f2: b -> m[c] \ ef2): a -> m[c] \ { ef1, ef2 } with Monad[m] = x ->
        kleisliLeft(f1, f2, x)


    ///
    /// `<=<` is an operator alias for `kleisliRight`.
    ///
    pub def <=<(f1: b -> m[c] \ ef1, f2: a -> m[b] \ ef2): a -> m[c] \ { ef1, ef2 } with Monad[m] = x ->
        kleisliRight(f1, f2, x)

}
