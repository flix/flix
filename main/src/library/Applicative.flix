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
/// A trait for functors that support application, i.e. allow to:
///
/// - Make an applicative value out of a normal value (embed it into a default context), e.g. embed `5` into `Some(5)`.
/// - Apply a function-type applicative to a matching argument-type applicative, resulting in an applicative of
/// the function's result type.
///
/// The meaning of the application realized by the `ap` function is defined by the respective instance.
/// Conceptually this can be understood as applying functions "contained" in the first applicative to arguments
/// in the second applicative, where the possible quantity of functions/arguments depends on the type `m`.
/// For example, an `Option[a -> b]` can be `None`, or contain a function of type `a -> b`, and only in the
/// latter case a function is applied. A `List[a -> b]` is an applicative that contains a list of functions,
/// which are all to be applied to all arguments contained in the arguments list.
///
/// A minimal implementation must define `point` and at least one of `ap` and `map2` (if `map2` is implemented,
/// `ap` can be defined based on `map2` as shown below). If both `ap` and `map2` are defined,
/// they must be equivalent to their default definitions:
///     `ap(f: m[a -> b \ e], x: m[a]): m[b] \ ef = map2(identity, f, x)`
///     `map2(f: a -> b -> c \ e, x: m[a], y: m[b]): m[c] \ ef = ap(Functor.map(f, x), y)`
///
pub lawful trait Applicative[m : Type -> Type] with Functor[m] {
    ///
    /// Puts `x` into a default context.
    ///
    pub def point(x: a): m[a]

    ///
    /// Apply the function-type applicative `f` to the argument-type applicative `x`.
    ///
    pub def ap(f: m[a -> b \ ef], x: m[a]): m[b] \ ef

    ///
    /// Lift a binary function to work on `Applicative`s.
    /// Instances can define more efficient implementations than the default implementation
    /// (which is `Applicative.ap(Functor.map(f, x1), x2)`).
    ///
    pub def map2(f: t1 -> t2 -> r \ ef, x1: m[t1], x2: m[t2]): m[r] \ ef = Applicative.ap(Functor.map(f, x1), x2)

    ///
    /// Lift a ternary function to work on `Applicative`s.
    /// Instances can define more efficient implementations than the default implementation
    /// (which is `Applicative.ap(Applicative.map2(f, x1, x2), x3)`).
    ///
    pub def map3(f: t1 -> t2 -> t3 -> r \ ef, x1: m[t1], x2: m[t2], x3: m[t3]): m[r] \ ef = Applicative.ap(Applicative.map2(f, x1, x2), x3)

    ///
    /// Lift a 4-ary function to work on `Applicative`s.
    /// Instances can define more efficient implementations than the default implementation
    /// (which is `Applicative.ap(Applicative.map3(f, x1, x2, x3), x4)`).
    ///
    pub def map4(f: t1 -> t2 -> t3 -> t4 -> r \ ef, x1: m[t1], x2: m[t2], x3: m[t3], x4: m[t4]): m[r] \ ef = Applicative.ap(Applicative.map3(f, x1, x2, x3), x4)

    ///
    /// Lift a 5-ary function to work on `Applicative`s.
    /// Instances can define more efficient implementations than the default implementation
    /// (which is `Applicative.ap(Applicative.map3(f, x1, x2, x3), x4)`).
    ///
    pub def map5(f: t1 -> t2 -> t3 -> t4 -> t5 -> r \ ef, x1: m[t1], x2: m[t2], x3: m[t3], x4: m[t4], x5: m[t5]): m[r] \ ef = Applicative.ap(Applicative.map4(f, x1, x2, x3, x4), x5)

    ///
    /// Applying the identity function wrapped into an applicative preserves every applicative value.
    ///
    law identity: forall(x: m[a]) with Eq[m[a]] Applicative.ap(Applicative.point(identity), x) == x

    ///
    /// Applicatively composing two functions and then applicatively applying the resulting function is the same
    /// as applicatively applying the functions one by one.
    ///
    law composition: forall(f: m[b -> c], g: m[a -> b], v: m[a]) with Eq[m[c]] Applicative.ap(Applicative.ap(Applicative.ap(Applicative.point((f, g) -> g >> f), f), g), v) == Applicative.ap(f, Applicative.ap(g, v))

    ///
    /// `point` is a homomorphism from normal to applicative values regarding application (normal vs. applicative).
    ///
    // TODO error in type checker does not allow to compile this, see #2099
    // law homomorphism: forall(f: a -> b, x: a) with Eq[m[b]] Applicative.ap(Applicative.point(f), Applicative.point(x)) == Applicative.point(f(x))

    ///
    /// Directly applicatively applying a function `f` to an argument `x` put into a default context is the same as putting the function that takes a function and applies it to `x` into a default context and applicatively applying it to `f`.
    ///
    law interchange: forall(f: m[a -> b], x: a) with Eq[m[b]] Applicative.ap(f, Applicative.point(x)) == Applicative.ap(Applicative.point(f -> f(x)), f)

    ///
    /// `map2` is equivalent to its default implementation.
    ///
    law map2Correspondence: forall(f: t1 -> t2 -> r, x1: m[t1], x2: m[t2]) with Eq[m[r]] Applicative.map2(f, x1, x2) == Applicative.ap(Functor.map(f, x1), x2)

    ///
    /// `map3` is equivalent to its default implementation.
    ///
    law map3Correspondence: forall(f: t1 -> t2 -> t3 -> r, x1: m[t1], x2: m[t2], x3: m[t3]) with Eq[m[r]] Applicative.map3(f, x1, x2, x3) == Applicative.ap(Applicative.map2(f, x1, x2), x3)

    ///
    /// `map4` is equivalent to its default implementation.
    ///
    law map4Correspondence: forall(f: t1 -> t2 -> t3 -> t4 -> r, x1: m[t1], x2: m[t2], x3: m[t3], x4: m[t4]) with Eq[m[r]] Applicative.map4(f, x1, x2, x3, x4) == Applicative.ap(Applicative.map3(f, x1, x2, x3), x4)

    ///
    /// `map5` is equivalent to its default implementation.
    ///
    law map5Correspondence: forall(f: t1 -> t2 -> t3 -> t4 -> t5 -> r, x1: m[t1], x2: m[t2], x3: m[t3], x4: m[t4], x5: m[t5]) with Eq[m[r]] Applicative.map5(f, x1, x2, x3, x4, x5) == Applicative.ap(Applicative.map4(f, x1, x2, x3, x4), x5)

    ///
    /// Mapping a function over an applicative (with `Functor.map`) is the same as putting the function
    /// into a default context and applying it to the applicative.
    ///
    law mapCorrespondence: forall(f: a -> b, x: m[a]) with Eq[m[b]] Functor.map(f, x) == Applicative.ap(Applicative.point(f), x)
}

mod Applicative {

    ///
    /// Chain two applicative actions, returns the product of their results.
    ///
    pub def product(fa: m[a], fb: m[b]): m[(a, b)] with Applicative[m] =
        Applicative.map2((a, b) -> (a, b), fa, fb)

    ///
    /// Chain two applicative actions, return only the result of the first.
    ///
    pub def productLeft(fa: m[a], fb: m[b]): m[a] with Applicative[m] =
        Applicative.map2((a, _) -> a, fa, fb)

    ///
    /// Chain two applicative actions, return only the result of the second.
    ///
    pub def productRight(fa: m[a], fb: m[b]): m[b] with Applicative[m] =
        Applicative.map2((_, b) -> b, fa, fb)

    ///
    /// Chain three applicative actions, return the 3-tuple of their results.
    ///
    pub def product3(x1: m[t1], x2: m[t2], x3: m[t3]): m[(t1, t2, t3)] with Applicative[m] =
        Applicative.map3((a, b, c) -> (a, b, c), x1, x2, x3)

    ///
    /// Chain four applicative actions, return the 4-tuple of their results.
    ///
    pub def product4(x1: m[t1], x2: m[t2], x3: m[t3], x4: m[t4]): m[(t1, t2, t3, t4)] with Applicative[m] =
        Applicative.map4((a, b, c, d) -> (a, b, c, d), x1, x2, x3, x4)

    ///
    /// Chain five applicative actions, return the 5-tuple of their results.
    ///
    pub def product5(x1: m[t1], x2: m[t2], x3: m[t3], x4: m[t4], x5: m[t5]): m[(t1, t2, t3, t4, t5)] with Applicative[m] =
        Applicative.map5((a, b, c, d, e) -> (a, b, c, d, e), x1, x2, x3, x4, x5)

    ///
    /// `<*>` is an operator alias for `ap`.
    ///
    pub def <*>(mf: m[a -> b \ ef], ma: m[a]): m[b] \ ef with Applicative[m] = Applicative.ap(mf, ma)

    ///
    /// `<**>` is a variant of the operator `<*>` with its arguments flipped.
    ///
    /// The order of evaluation is `ma` then `mf`.
    ///
    pub def <**>(ma: m[a], mf: m[a -> b \ ef]): m[b] \ ef with Applicative[m] = Applicative.map2((a, f) -> f(a), ma, mf)

    ///
    /// `<*` is an operator alias for `productLeft`.
    ///
    pub def <*(ma: m[a], mb: m[b]): m[a] with Applicative[m] = productLeft(ma, mb)

    ///
    /// `*>` is an operator alias for `productRight`.
    ///
    pub def *>(ma: m[a], mb: m[b]): m[b] with Applicative[m] = productRight(ma, mb)

}
