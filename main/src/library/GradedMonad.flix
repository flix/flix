/*
 * Copyright 2022 Stephen Tetley
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

pub class GradedMonad[m: Bool -> Type -> Type] with GradedApplicative[m] {

    ///
    /// Apply function `f` to the graded monadic value `x`, resulting in a combined graded monadic value.
    ///
    pub def flatMap(f: a -> m[ef1, b] \ ef, x: m[ef2, a]): m[ef1 and ef2 and ef, b]


}


mod GradedMonad {



    ///
    /// The graded version of the monadic `join` operator.
    /// Flatten `x` - a graded monadic action nested in an outer graded monadic layer - to a single layer.
    ///
    pub def flatten[a: Type, ef1: Bool, ef2: Bool, m: Bool -> Type -> Type]
                   (x: m[ef1, m[ef2, a]]): m[ef1 and ef2, a] with GradedMonad[m] = GradedMonad.flatMap(identity, x)

    ///
    /// The left-to-right Kleisli composition operator for graded monads.
    ///
    /// Map `x` with the graded monadic function `f1` and then map its result with the function `f2`.
    ///
    pub def kleisliLeft[a: Type, b: Type, c: Type, ef1: Bool, ef2: Bool, ef3: Bool, ef4: Bool, m: Bool -> Type -> Type]
                      (f1: a -> m[ef1, b] \ ef3, f2: b -> m[ef2, c] \ ef4, x: a): m[ef1 and ef2 and ef3 and ef4, c] with GradedMonad[m] =
        GradedMonad.flatMap(x1 -> f2(x1), GradedMonad.flatMap(f1, GradedApplicative.point(x)))

    ///
    /// The right-to-left Kleisli composition operator for graded monads.
    ///
    /// Map `x` with the graded monadic function `f2` and then map its result with the function `f1`.
    ///
    pub def kleisliRight[a: Type, b: Type, c: Type, ef1: Bool, ef2: Bool, ef3: Bool, ef4: Bool, m: Bool -> Type -> Type]
                        (f1: b -> m[ef2, c] \ ef4, f2: a -> m[ef1, b] \ ef3, x: a): m[ef1 and ef2 and ef3 and ef4, c] with GradedMonad[m] =
        GradedMonad.flatMap(x1 -> f1(x1), GradedMonad.flatMap(f2, GradedApplicative.point(x)))

    ///
    /// `=<<` is an operator alias for `flatMap`.
    ///
    pub def =<<[a: Type, b: Type, ef1: Bool, ef2: Bool, ef: Bool, m: Bool -> Type -> Type]
               (k: a -> m[ef1, b] \ ef, x: m[ef2, a]): m[ef1 and ef2 and ef, b] with GradedMonad[m] = 
        GradedMonad.flatMap(k, x)

    ///
    /// `>>=` is the operator `=<<` with its arguments flipped.
    ///
    /// `>>=` is the monadic bind operator.
    ///
    pub def >>=[a: Type, b: Type, ef1: Bool, ef2: Bool, ef: Bool, m: Bool -> Type -> Type]
               (x: m[ef1, a], k: a -> m[ef2, b] \ ef): m[ef1 and ef2 and ef, b] with GradedMonad[m] = 
        GradedMonad.flatMap(k, x)


}


