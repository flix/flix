/*
 *  Copyright 2024 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Coerce {

    ///
    /// A trait for type that can be coerced.
    ///
    /// Currently only supports newtype-style wrappers.
    ///
    pub trait Coerce[t] {
        ///
        /// The return type of the coerce function.
        ///
        type Out

        ///
        /// The associated effect of the `Coerce` instance.
        ///
        /// The effect describes the effect of coercing a value of type `t`.
        /// It is pure (i.e. `{}`) for the common case of newtype-style wrappers.
        ///
        type Aef: Eff = {}

        ///
        /// Returns the unwrapped value of `x`.
        ///
        pub def coerce(x: t): Coerce.Out[t] \ Coerce.Aef[t]
    }

}
