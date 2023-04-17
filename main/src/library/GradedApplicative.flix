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

pub class GradedApplicative[m: Bool -> Type -> Type] with GradedFunctor[m] {

    ///
    /// Lifts `x` into the graded applicative context.
    ///
    pub def point(x: a): m[ef, a]

    ///
    /// Apply the function-type graded applicative `f` to the argument-type graded applicative `x`.
    ///
    pub def ap(f: m[ef1, a -> b \ ef2], x: m[ef, a]): m[ef and ef1 and ef2, b]

    ///
    /// Map a binary function to work on graded applicatives.
    /// Instances can define more efficient implementations than the default implementation.
    ///
    pub def map2(f: t1 -> t2 -> a \ ef, x1: m[ef1, t1], x2: m[ef2, t2]): m[ef1 and ef2 and ef, a] =
        GradedApplicative.ap(GradedFunctor.map(f, x1), x2)

    ///
    /// Map a ternary function to work on graded applicatives.
    /// Instances can define more efficient implementations than the default implementation.
    ///
    pub def map3(f: t1 -> t2 -> t3 -> a \ ef, x1: m[ef1, t1], x2: m[ef2, t2], x3: m[ef3, t3]): m[ef1 and ef2 and ef3 and ef, a] =
        GradedApplicative.ap(GradedApplicative.map2(f, x1, x2), x3)

    ///
    /// Map a 4-ary function to work on graded applicatives.
    /// Instances can define more efficient implementations than the default implementation
    ///
    pub def map4(f: t1 -> t2 -> t3 -> t4 -> a \ ef, x1: m[ef1, t1], x2: m[ef2, t2], x3: m[ef3, t3], x4: m[ef4, t4]): m[ef1 and ef2 and ef3 and ef4 and ef, a] =
        GradedApplicative.ap(GradedApplicative.map3(f, x1, x2, x3), x4)

    ///
    /// Map a 5-ary function to work on graded applicatives.
    /// Instances can define more efficient implementations than the default implementation
    ///
    pub def map5(f: t1 -> t2 -> t3 -> t4 -> t5 -> a \ ef, x1: m[ef1, t1], x2: m[ef2, t2], x3: m[ef3, t3], x4: m[ef4, t4], x5: m[ef5, t5]): m[ef1 and ef2 and ef3 and ef4 and ef5 and ef, a] =
        GradedApplicative.ap(GradedApplicative.map4(f, x1, x2, x3, x4), x5)

}

mod GradedApplicative {

    ///
    /// Chain two graded applicative actions, returns the product of their results.
    ///
    pub def product[a: Type, b: Type, ef1: Bool, ef2: Bool, m: Bool -> Type -> Type]
                   (fa: m[ef1, a], fb: m[ef2, b]): m[ef1 and ef2, (a, b)] with GradedApplicative[m] =
        GradedApplicative.map2((a, b) -> (a, b), fa, fb)

    ///
    /// Chain two graded applicative actions, return only the result of the first.
    ///
    pub def productLeft[a: Type, b: Type, ef1: Bool, ef2: Bool, m: Bool -> Type -> Type]
                       (fa: m[ef1, a], fb: m[ef2, b]): m[ef1 and ef2, a] with GradedApplicative[m] =
        GradedApplicative.map2((a, _) -> a, fa, fb)

    ///
    /// Chain two graded applicative actions, return only the result of the second.
    ///
    pub def productRight[a: Type, b: Type, ef1: Bool, ef2: Bool, m: Bool -> Type -> Type]
                        (fa: m[ef1, a], fb: m[ef2, b]): m[ef1 and ef2, b] with GradedApplicative[m] =
        GradedApplicative.map2((_, b) -> b, fa, fb)

    ///
    /// Chain three graded applicative actions, return the 3-tuple of their results.
    ///
    pub def product3[t1: Type, t2: Type, t3: Type, ef1: Bool, ef2: Bool, ef3: Bool, m: Bool -> Type -> Type]
                    (x1: m[ef1, t1], x2: m[ef2, t2], x3: m[ef3, t3]): m[ef1 and ef2 and ef3, (t1, t2, t3)] with GradedApplicative[m] =
        GradedApplicative.map3((a, b, c) -> (a, b, c), x1, x2, x3)

    ///
    /// Chain four graded applicative actions, return the 4-tuple of their results.
    ///
    pub def product4[t1: Type, t2: Type, t3: Type, t4: Type, ef1: Bool, ef2: Bool, ef3: Bool, ef4: Bool, m: Bool -> Type -> Type]
                    (x1: m[ef1, t1], x2: m[ef2, t2], x3: m[ef3, t3], x4: m[ef4, t4]): m[ef1 and ef2 and ef3 and ef4, (t1, t2, t3, t4)] with GradedApplicative[m] =
        GradedApplicative.map4((a, b, c, d) -> (a, b, c, d), x1, x2, x3, x4)

    ///
    /// Chain five graded applicative actions, return the 5-tuple of their results.
    ///
    pub def product5[t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, ef1: Bool, ef2: Bool, ef3: Bool, ef4: Bool, ef5: Bool, m: Bool -> Type -> Type]
                    (x1: m[ef1, t1], x2: m[ef2, t2], x3: m[ef3, t3], x4: m[ef4, t4], x5: m[ef5, t5]): m[ef1 and ef2 and ef3 and ef4 and ef5, (t1, t2, t3, t4, t5)] with GradedApplicative[m] =
        GradedApplicative.map5((a, b, c, d, e) -> (a, b, c, d, e), x1, x2, x3, x4, x5)

    ///
    /// `<*>` is an operator alias for `ap`.
    ///
    pub def <*>[m: Bool -> Type -> Type, ef: Bool, ef1: Bool, ef2: Bool, a: Type, b: Type]
               (mf: m[ef1, a -> b \ ef2], ma: m[ef, a]): m[ef and ef1 and ef2, b] with GradedApplicative[m] =
        GradedApplicative.ap(mf, ma)

    ///
    /// `<**>` is a variant of the operator `<*>` with its arguments flipped.
    ///
    /// The order of evaluation is `ma` then `mf`.
    ///
    pub def <**>[a: Type, b: Type, ef1: Bool, ef2: Bool, ef: Bool, m: Bool -> Type -> Type]
                (ma: m[ef1, a], mf: m[ef2, a -> b \ ef]): m[ef1 and ef2 and ef, b] with GradedApplicative[m] =
        GradedApplicative.map2((a, f) -> f(a), ma, mf)

    ///
    /// `<*` is an operator alias for `productLeft`.
    ///
    pub def <*[a: Type, b: Type, ef1: Bool, ef2: Bool, m: Bool -> Type -> Type]
              (ma: m[ef1, a], mb: m[ef2, b]): m[ef1 and ef2, a] with GradedApplicative[m] =
        productLeft(ma, mb)

    ///
    /// `*>` is an operator alias for `productRight`.
    ///
    pub def *>[a: Type, b: Type, ef1: Bool, ef2: Bool, m: Bool -> Type -> Type]
              (ma: m[ef1, a], mb: m[ef2, b]): m[ef1 and ef2, b] with GradedApplicative[m] =
        productRight(ma, mb)

}
