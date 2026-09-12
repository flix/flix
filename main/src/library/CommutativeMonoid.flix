/*
 *  Copyright 2022 Jakob Schneider Villumsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod CommutativeMonoid {

    ///
    /// A trait for types that form a commutative monoid.
    ///
    pub trait CommutativeMonoid[a] with Monoid[a] {

        ///
        /// Returns a neutral element.
        ///
        pub def empty(): a = Monoid.empty()

        ///
        /// An associative & commutative binary operation on `a`.
        ///
        pub def combine(x: a, y: a): a =
            Monoid.combine(x, y)
    }

    instance CommutativeMonoid[Unit]

    instance CommutativeMonoid[Int8]

    instance CommutativeMonoid[Int16]

    instance CommutativeMonoid[Int32]

    instance CommutativeMonoid[Int64]

    instance CommutativeMonoid[BigInt]

    instance CommutativeMonoid[Float32]

    instance CommutativeMonoid[Float64]

    instance CommutativeMonoid[BigDecimal]

    instance CommutativeMonoid[(a1, a2)] with CommutativeMonoid[a1], CommutativeMonoid[a2]

    instance CommutativeMonoid[(a1, a2, a3)] with CommutativeMonoid[a1], CommutativeMonoid[a2], CommutativeMonoid[a3]

    instance CommutativeMonoid[(a1, a2, a3, a4)] with CommutativeMonoid[a1], CommutativeMonoid[a2], CommutativeMonoid[a3], CommutativeMonoid[a4]

    instance CommutativeMonoid[(a1, a2, a3, a4, a5)] with CommutativeMonoid[a1], CommutativeMonoid[a2], CommutativeMonoid[a3], CommutativeMonoid[a4], CommutativeMonoid[a5]

    instance CommutativeMonoid[(a1, a2, a3, a4, a5, a6)] with CommutativeMonoid[a1], CommutativeMonoid[a2], CommutativeMonoid[a3], CommutativeMonoid[a4], CommutativeMonoid[a5], CommutativeMonoid[a6]

    instance CommutativeMonoid[(a1, a2, a3, a4, a5, a6, a7)] with CommutativeMonoid[a1], CommutativeMonoid[a2], CommutativeMonoid[a3], CommutativeMonoid[a4], CommutativeMonoid[a5], CommutativeMonoid[a6], CommutativeMonoid[a7]

    instance CommutativeMonoid[(a1, a2, a3, a4, a5, a6, a7, a8)] with CommutativeMonoid[a1], CommutativeMonoid[a2], CommutativeMonoid[a3], CommutativeMonoid[a4], CommutativeMonoid[a5], CommutativeMonoid[a6], CommutativeMonoid[a7], CommutativeMonoid[a8]

    instance CommutativeMonoid[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with CommutativeMonoid[a1], CommutativeMonoid[a2], CommutativeMonoid[a3], CommutativeMonoid[a4], CommutativeMonoid[a5], CommutativeMonoid[a6], CommutativeMonoid[a7], CommutativeMonoid[a8], CommutativeMonoid[a9]

    instance CommutativeMonoid[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with CommutativeMonoid[a1], CommutativeMonoid[a2], CommutativeMonoid[a3], CommutativeMonoid[a4], CommutativeMonoid[a5], CommutativeMonoid[a6], CommutativeMonoid[a7], CommutativeMonoid[a8], CommutativeMonoid[a9], CommutativeMonoid[a10]

}
