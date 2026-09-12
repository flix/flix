/*
 *  Copyright 2022 Nina Andrup Pedersen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Iterable {

    ///
    /// A trait for immutable data structures that can be iterated.
    ///
    pub trait Iterable[t] {
        ///
        /// The element type of the Iterable.
        ///
        type Elm[t]: Type

        ///
        /// The associated effect of creating the Iterable and accessing the elements
        /// in the underlying data structure or generator.
        ///
        /// The effect is embedded in the type of the Iterator.
        ///
        type Aef[t]: Eff = {}

        ///
        /// Returns an iterator over `t`.
        ///
        pub def iterator(rc: Region[r], t: t): Iterator[Iterable.Elm[t], r + aef, r] \ (r + aef) where Iterable.Aef[t] ~ aef

    }

}
