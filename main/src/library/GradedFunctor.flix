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

pub class GradedFunctor[m: Bool -> Type -> Type] {
    pub def map(f: a -> b \ ef1, x: m[ef, a]): m[ef and ef1, b]
}

mod GradedFunctor {

    ///
    /// Replaces the value `a` in `ma` by the given value `x` preserving the structure of `ma`.
    ///
    pub def replace[a: Type, b: Type, ef: Bool, m: Bool -> Type -> Type]
                    (ma: m[ef, a], x: b): m[ef, b] with GradedFunctor[m] = GradedFunctor.map(_ -> x, ma)

    ///
    /// Replaces the value `a` in `ma` with `Unit` preserving the structure of `ma`.
    ///
    /// This function is typically used to discard the return value of computing `ma`.
    ///
    pub def ignore[a: Type, ef: Bool, m: Bool -> Type -> Type]
                  (ma: m[ef, a]): m[ef, Unit] with GradedFunctor[m] = replace(ma, ())

    ///
    /// `<$>` is an operator alias for `gmap`.
    ///
    pub def <$>[a: Type, b: Type, ef: Bool, ef1: Bool, m: Bool -> Type -> Type]
               (f: a -> b \ ef1, x: m[ef, a]): m[ef and ef1, b] with GradedFunctor[m] = GradedFunctor.map(f, x)

    ///
    /// `<$$>` is the operator `<$>` with its arguments flipped.
    ///
    pub def <$$>[a: Type, b: Type, ef: Bool, ef1: Bool, m: Bool -> Type -> Type]
                (x: m[ef, a], f: a -> b \ ef1): m[ef and ef1, b] with GradedFunctor[m] = GradedFunctor.map(f, x)

    ///
    /// `<$` is the operator `$>` with its arguments flipped.
    ///
    pub def <$[a: Type, b: Type, ef: Bool, m: Bool -> Type -> Type]
              (x: a, ma: m[ef, b]): m[ef, a] with GradedFunctor[m] = replace(ma, x)

    ///
    /// `$>` is an operator alias for `replace`.
    ///
    pub def $>[a: Type, b: Type, ef: Bool, m: Bool -> Type -> Type]
              (ma: m[ef, a], x: b): m[ef, b] with GradedFunctor[m] = replace(ma, x)

}
