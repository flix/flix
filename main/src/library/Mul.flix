/*
 *  Copyright 2020 Esben Bjerre
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Mul {

    ///
    /// A trait for multiplication.
    ///
    pub trait Mul[a] {
        ///
        /// Returns `x` multiplied by `y`.
        ///
        pub def mul(x: a, y: a): a
    }

    instance Mul[Float32] {
        pub def mul(x: Float32, y: Float32): Float32 = %%FLOAT32_MUL%%(x, y)
    }

    instance Mul[Float64] {
        pub def mul(x: Float64, y: Float64): Float64 = %%FLOAT64_MUL%%(x, y)
    }

    instance Mul[BigDecimal] {
        pub def mul(x: BigDecimal, y: BigDecimal): BigDecimal = x.multiply(y)
    }

    instance Mul[Int8] {
        pub def mul(x: Int8, y: Int8): Int8 = %%INT8_MUL%%(x, y)
    }

    instance Mul[Int16] {
        pub def mul(x: Int16, y: Int16): Int16 = %%INT16_MUL%%(x, y)
    }

    instance Mul[Int32] {
        pub def mul(x: Int32, y: Int32): Int32 = %%INT32_MUL%%(x, y)
    }

    instance Mul[Int64] {
        pub def mul(x: Int64, y: Int64): Int64 = %%INT64_MUL%%(x, y)
    }

    instance Mul[BigInt] {
        pub def mul(x: BigInt, y: BigInt): BigInt = x.multiply(y)
    }

}
