/*
 *  Copyright 2020 Esben Bjerre
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Add {

    ///
    /// A trait for addition.
    ///
    pub trait Add[a] {
        ///
        /// Returns the sum of `x` and `y`.
        ///
        pub def add(x: a, y: a): a
    }

    instance Add[Float32] {
        pub def add(x: Float32, y: Float32): Float32 = %%FLOAT32_ADD%%(x, y)
    }

    instance Add[Float64] {
        pub def add(x: Float64, y: Float64): Float64 = %%FLOAT64_ADD%%(x, y)
    }

    instance Add[BigDecimal] {
        pub def add(x: BigDecimal, y: BigDecimal): BigDecimal = x.add(y)
    }

    instance Add[Int8] {
        pub def add(x: Int8, y: Int8): Int8 = %%INT8_ADD%%(x, y)
    }

    instance Add[Int16] {
        pub def add(x: Int16, y: Int16): Int16 = %%INT16_ADD%%(x, y)
    }

    instance Add[Int32] {
        pub def add(x: Int32, y: Int32): Int32 = %%INT32_ADD%%(x, y)
    }

    instance Add[Int64] {
        pub def add(x: Int64, y: Int64): Int64 = %%INT64_ADD%%(x, y)
    }

    instance Add[BigInt] {
        pub def add(x: BigInt, y: BigInt): BigInt = x.add(y)
    }

    instance Add[String] {
        pub def add(x: String, y: String): String = x.concat(y)
    }

}
