/*
 *  Copyright 2022 Jakob Schneider Villumsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

///
/// Alias for `CommutativeSemiGroup.combine`.
///
pub def |+|(x: a, y: a): a with CommutativeSemiGroup[a] = CommutativeSemiGroup.combine(x, y)

pub mod CommutativeSemiGroup {

    ///
    /// A trait for types that form a commutative semigroup.
    ///
    pub trait CommutativeSemiGroup[a] with SemiGroup[a] {

        ///
        /// An associative & commutative binary operation on `a`.
        ///
        pub def combine(x: a, y: a): a = SemiGroup.combine(x, y)
    }

    instance CommutativeSemiGroup[Unit]

    instance CommutativeSemiGroup[Int8]

    instance CommutativeSemiGroup[Int16]

    instance CommutativeSemiGroup[Int32]

    instance CommutativeSemiGroup[Int64]

    instance CommutativeSemiGroup[BigInt]

    instance CommutativeSemiGroup[Float32]

    instance CommutativeSemiGroup[Float64]

    instance CommutativeSemiGroup[BigDecimal]

    instance CommutativeSemiGroup[(a1, a2)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2]

    instance CommutativeSemiGroup[(a1, a2, a3)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2], CommutativeSemiGroup[a3]

    instance CommutativeSemiGroup[(a1, a2, a3, a4)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2], CommutativeSemiGroup[a3], CommutativeSemiGroup[a4]

    instance CommutativeSemiGroup[(a1, a2, a3, a4, a5)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2], CommutativeSemiGroup[a3], CommutativeSemiGroup[a4], CommutativeSemiGroup[a5]

    instance CommutativeSemiGroup[(a1, a2, a3, a4, a5, a6)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2], CommutativeSemiGroup[a3], CommutativeSemiGroup[a4], CommutativeSemiGroup[a5], CommutativeSemiGroup[a6]

    instance CommutativeSemiGroup[(a1, a2, a3, a4, a5, a6, a7)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2], CommutativeSemiGroup[a3], CommutativeSemiGroup[a4], CommutativeSemiGroup[a5], CommutativeSemiGroup[a6], CommutativeSemiGroup[a7]

    instance CommutativeSemiGroup[(a1, a2, a3, a4, a5, a6, a7, a8)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2], CommutativeSemiGroup[a3], CommutativeSemiGroup[a4], CommutativeSemiGroup[a5], CommutativeSemiGroup[a6], CommutativeSemiGroup[a7], CommutativeSemiGroup[a8]

    instance CommutativeSemiGroup[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2], CommutativeSemiGroup[a3], CommutativeSemiGroup[a4], CommutativeSemiGroup[a5], CommutativeSemiGroup[a6], CommutativeSemiGroup[a7], CommutativeSemiGroup[a8], CommutativeSemiGroup[a9]

    instance CommutativeSemiGroup[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2], CommutativeSemiGroup[a3], CommutativeSemiGroup[a4], CommutativeSemiGroup[a5], CommutativeSemiGroup[a6], CommutativeSemiGroup[a7], CommutativeSemiGroup[a8], CommutativeSemiGroup[a9], CommutativeSemiGroup[a10]

}
