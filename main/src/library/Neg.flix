/*
 *  Copyright 2020 Esben Bjerre
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Neg {

    ///
    /// A trait for negation.
    ///
    pub trait Neg[a] {
        ///
        /// Returns -`x`.
        ///
        pub def neg(x: a): a
    }

    instance Neg[Float32] {
        pub def neg(x: Float32): Float32 = %%FLOAT32_NEG%%(x)
    }

    instance Neg[Float64] {
        pub def neg(x: Float64): Float64 = %%FLOAT64_NEG%%(x)
    }

    instance Neg[BigDecimal] {
        pub def neg(x: BigDecimal): BigDecimal = x.negate()
    }

    instance Neg[Int8] {
        pub def neg(x: Int8): Int8 = %%INT8_NEG%%(x)
    }

    instance Neg[Int16] {
        pub def neg(x: Int16): Int16 = %%INT16_NEG%%(x)
    }

    instance Neg[Int32] {
        pub def neg(x: Int32): Int32 = %%INT32_NEG%%(x)
    }

    instance Neg[Int64] {
        pub def neg(x: Int64): Int64 = %%INT64_NEG%%(x)
    }

    instance Neg[BigInt] {
        pub def neg(x: BigInt): BigInt = x.negate()
    }

}
