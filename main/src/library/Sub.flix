/*
 *  Copyright 2020 Esben Bjerre
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Sub {

    ///
    /// A trait for subtraction.
    ///
    pub trait Sub[a] {
        ///
        /// Returns the difference of `x` and `y`.
        ///
        pub def sub(x: a, y: a): a
    }

    instance Sub[Float32] {
        pub def sub(x: Float32, y: Float32): Float32 = %%FLOAT32_SUB%%(x, y)
    }

    instance Sub[Float64] {
        pub def sub(x: Float64, y: Float64): Float64 = %%FLOAT64_SUB%%(x, y)
    }

    instance Sub[BigDecimal] {
        pub def sub(x: BigDecimal, y: BigDecimal): BigDecimal = x.subtract(y)
    }

    instance Sub[Int8] {
        pub def sub(x: Int8, y: Int8): Int8 = %%INT8_SUB%%(x, y)
    }

    instance Sub[Int16] {
        pub def sub(x: Int16, y: Int16): Int16 = %%INT16_SUB%%(x, y)
    }

    instance Sub[Int32] {
        pub def sub(x: Int32, y: Int32): Int32 = %%INT32_SUB%%(x, y)
    }

    instance Sub[Int64] {
        pub def sub(x: Int64, y: Int64): Int64 = %%INT64_SUB%%(x, y)
    }

    instance Sub[BigInt] {
        pub def sub(x: BigInt, y: BigInt): BigInt = x.subtract(y)
    }

}
