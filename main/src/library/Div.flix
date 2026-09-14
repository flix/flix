/*
 *  Copyright 2020 Esben Bjerre
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Div {

    import java.io.File

    ///
    /// A trait for division.
    ///
    pub trait Div[a] {
        ///
        /// Returns `x` divided by `y`.
        ///
        pub def div(x: a, y: a): a
    }

    instance Div[Float32] {
        pub def div(x: Float32, y: Float32): Float32 = %%FLOAT32_DIV%%(x, y)
    }

    instance Div[Float64] {
        pub def div(x: Float64, y: Float64): Float64 = %%FLOAT64_DIV%%(x, y)
    }

    instance Div[BigDecimal] {
        pub def div(x: BigDecimal, y: BigDecimal): BigDecimal =
            if (y == 0ff)
                0ff
            else
                x.divide(y)
    }

    instance Div[Int8] {
        pub def div(x: Int8, y: Int8): Int8 =
            if (y == 0i8)
                0i8
            else
                %%INT8_DIV%%(x, y)
    }

    instance Div[Int16] {
        pub def div(x: Int16, y: Int16): Int16 =
            if (y == 0i16)
                0i16
            else
                %%INT16_DIV%%(x, y)
    }

    instance Div[Int32] {
        pub def div(x: Int32, y: Int32): Int32 =
            if (y == 0i32)
                0i32
            else
                %%INT32_DIV%%(x, y)
    }

    instance Div[Int64] {
        pub def div(x: Int64, y: Int64): Int64 =
            if (y == 0i64)
                0i64
            else
                %%INT64_DIV%%(x, y)
    }

    instance Div[BigInt] {
        pub def div(x: BigInt, y: BigInt): BigInt =
            if (y == 0ii)
                0ii
            else
                x.divide(y)
    }

    instance Div[String] {
        pub def div(x: String, y: String): String =
            let s = File.separator;
            x + s + y
    }

}
