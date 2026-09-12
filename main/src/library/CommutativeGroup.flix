/*
 *  Copyright 2022 Jakob Schneider Villumsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod CommutativeGroup {

    ///
    /// A trait for types that form a commutative group (abelian group)
    /// i.e. groups where the `combine` function is commutative.
    ///
    /// The default instances for number define the additive inverse in the real numbers.
    ///
    pub trait CommutativeGroup[a] with Group[a], CommutativeMonoid[a] {}

    instance CommutativeGroup[Unit]

    instance CommutativeGroup[Int8]

    instance CommutativeGroup[Int16]

    instance CommutativeGroup[Int32]

    instance CommutativeGroup[Int64]

    instance CommutativeGroup[BigInt]

    instance CommutativeGroup[Float32]

    instance CommutativeGroup[Float64]

    instance CommutativeGroup[BigDecimal]

    instance CommutativeGroup[(a1, a2)] with CommutativeGroup[a1], CommutativeGroup[a2]

    instance CommutativeGroup[(a1, a2, a3)] with CommutativeGroup[a1], CommutativeGroup[a2], CommutativeGroup[a3]

    instance CommutativeGroup[(a1, a2, a3, a4)] with CommutativeGroup[a1], CommutativeGroup[a2], CommutativeGroup[a3], CommutativeGroup[a4]

    instance CommutativeGroup[(a1, a2, a3, a4, a5)] with CommutativeGroup[a1], CommutativeGroup[a2], CommutativeGroup[a3], CommutativeGroup[a4], CommutativeGroup[a5]

    instance CommutativeGroup[(a1, a2, a3, a4, a5, a6)] with CommutativeGroup[a1], CommutativeGroup[a2], CommutativeGroup[a3], CommutativeGroup[a4], CommutativeGroup[a5], CommutativeGroup[a6]

    instance CommutativeGroup[(a1, a2, a3, a4, a5, a6, a7)] with CommutativeGroup[a1], CommutativeGroup[a2], CommutativeGroup[a3], CommutativeGroup[a4], CommutativeGroup[a5], CommutativeGroup[a6], CommutativeGroup[a7]

    instance CommutativeGroup[(a1, a2, a3, a4, a5, a6, a7, a8)] with CommutativeGroup[a1], CommutativeGroup[a2], CommutativeGroup[a3], CommutativeGroup[a4], CommutativeGroup[a5], CommutativeGroup[a6], CommutativeGroup[a7], CommutativeGroup[a8]

    instance CommutativeGroup[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with CommutativeGroup[a1], CommutativeGroup[a2], CommutativeGroup[a3], CommutativeGroup[a4], CommutativeGroup[a5], CommutativeGroup[a6], CommutativeGroup[a7], CommutativeGroup[a8], CommutativeGroup[a9]

    instance CommutativeGroup[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with CommutativeGroup[a1], CommutativeGroup[a2], CommutativeGroup[a3], CommutativeGroup[a4], CommutativeGroup[a5], CommutativeGroup[a6], CommutativeGroup[a7], CommutativeGroup[a8], CommutativeGroup[a9], CommutativeGroup[a10]

}
