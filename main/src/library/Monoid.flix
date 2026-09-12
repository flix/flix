/*
 *  Copyright 2020 Stephen Tetley
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Monoid {

    ///
    /// A trait for Monoids, objects that support an associative binary
    /// operation `combine` and neutral element `empty`.
    ///
    pub trait Monoid[a] with SemiGroup[a] {
        ///
        /// Returns a neutral element.
        ///
        pub def empty(): a

        ///
        /// Returns the result of combining `x` and `y` using SemiGroup's combine.
        ///
        pub def combine(x: a, y: a): a =
            SemiGroup.combine(x, y)
    }

    instance Monoid[Unit] {
        pub def empty(): Unit = ()
    }

    instance Monoid[Int8] {
        pub def empty(): Int8 = 0i8
    }

    instance Monoid[Int16] {
        pub def empty(): Int16 = 0i16
    }

    instance Monoid[Int32] {
        pub def empty(): Int32 = 0
    }

    instance Monoid[Int64] {
        pub def empty(): Int64 = 0i64
    }

    instance Monoid[BigInt] {
        pub def empty(): BigInt = 0ii
    }

    instance Monoid[Float32] {
        pub def empty(): Float32 = 0.0f32
    }

    instance Monoid[Float64] {
        pub def empty(): Float64 = 0.0f64
    }

    instance Monoid[BigDecimal] {
        pub def empty(): BigDecimal = 0.0ff
    }

    instance Monoid[String] {
        pub def empty(): String = ""
    }

    instance Monoid[(a1, a2)] with Monoid[a1], Monoid[a2] {
        pub def empty(): (a1, a2) = (Monoid.empty(), Monoid.empty())
    }

    instance Monoid[(a1, a2, a3)] with Monoid[a1], Monoid[a2], Monoid[a3] {
        pub def empty(): (a1, a2, a3) = (Monoid.empty(), Monoid.empty(), Monoid.empty())
    }

    instance Monoid[(a1, a2, a3, a4)] with Monoid[a1], Monoid[a2], Monoid[a3], Monoid[a4] {
        pub def empty(): (a1, a2, a3, a4) = (Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty())
    }

    instance Monoid[(a1, a2, a3, a4, a5)] with Monoid[a1], Monoid[a2], Monoid[a3], Monoid[a4], Monoid[a5] {
        pub def empty(): (a1, a2, a3, a4, a5) = (Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty())
    }

    instance Monoid[(a1, a2, a3, a4, a5, a6)] with Monoid[a1], Monoid[a2], Monoid[a3], Monoid[a4], Monoid[a5], Monoid[a6] {
        pub def empty(): (a1, a2, a3, a4, a5, a6) = (Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty())
    }

    instance Monoid[(a1, a2, a3, a4, a5, a6, a7)] with Monoid[a1], Monoid[a2], Monoid[a3], Monoid[a4], Monoid[a5], Monoid[a6], Monoid[a7] {
        pub def empty(): (a1, a2, a3, a4, a5, a6, a7) = (Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty())
    }

    instance Monoid[(a1, a2, a3, a4, a5, a6, a7, a8)] with Monoid[a1], Monoid[a2], Monoid[a3], Monoid[a4], Monoid[a5], Monoid[a6], Monoid[a7], Monoid[a8] {
        pub def empty(): (a1, a2, a3, a4, a5, a6, a7, a8) = (Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty())
    }

    instance Monoid[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with Monoid[a1], Monoid[a2], Monoid[a3], Monoid[a4], Monoid[a5], Monoid[a6], Monoid[a7], Monoid[a8], Monoid[a9] {
        pub def empty(): (a1, a2, a3, a4, a5, a6, a7, a8, a9) = (Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty())
    }

    instance Monoid[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with Monoid[a1], Monoid[a2], Monoid[a3], Monoid[a4], Monoid[a5], Monoid[a6], Monoid[a7], Monoid[a8], Monoid[a9], Monoid[a10] {
        pub def empty(): (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = (Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty(), Monoid.empty())
    }

    ///
    /// Returns the result of applying `combine` to all the elements in `t`, using `empty` as the initial value.
    ///
    pub def fold(t: t[a]): a \ Foldable.Aef[t] with Foldable[t], Monoid[a] = Foldable.fold(t)

}
