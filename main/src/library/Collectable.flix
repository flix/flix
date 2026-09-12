/*
 *  Copyright 2023 Stephen Tetley
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Collectable {

    ///
    /// A trait representing collections that can be produced from an Iterator.
    ///
    pub trait Collectable[t: Type] {
        ///
        /// The element type of the Collectable.
        ///
        type Elm[t]: Type

        ///
        /// The associated effect of the Collectable which represents the effect of accessing its elements.
        ///
        type Aef[t]: Eff = {}

        ///
        /// Run an Iterator collecting the results.
        ///
        pub def collect(iter: Iterator[Collectable.Elm[t], ef, r]): t \ (ef + Collectable.Aef[t] + r)

    }

}
