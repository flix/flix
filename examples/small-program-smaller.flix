use Bool.{==>, <==>}

///
/// A type class for equality and inequality.
///
pub lawful class Eq[a] {

    ///
    /// Returns `true` if and only if `x` is equal to `y`.
    ///
    pub def eq(x: a, y: a): Bool

    ///
    /// Returns `true` if and only if `x` is not equal to `y`.
    ///
    pub def neq(x: a, y: a): Bool = not Eq.eq(x, y)

    ///
    /// Reflexivity: An element `x` is equal to itself.
    ///
    law reflexivity: forall(x: a). x == x

    ///
    /// Symmetry: If `x` is equal to `y` then `y` must also be equal to `x`.
    ///
    law symmetry: forall(x: a, y: a). (x == y) ==> (y == x)

    ///
    /// Transitivity: If `x` is equal to `y` and `y` is equal to `z` then `x` must be equal to `z`.
    ///
    law transitivity: forall(x: a, y: a, z: a). ((x == y) and (y == z)) ==> (x == z)

    ///
    /// x != y is logically equivalent to not (x == y).
    ///
    law inverseNeq: forall(x: a, y: a). (x != y) <==> (not (x == y))
}

instance Eq[Unit] {
    pub def eq(_x: Unit, _y: Unit): Bool = true
}

instance Eq[Bool] {
    pub def eq(x: Bool, y: Bool): Bool = $BOOL_EQ$(x, y)
    pub override def neq(x: Bool, y: Bool): Bool = $BOOL_NEQ$(x, y)
}

instance Eq[Char] {
    pub def eq(x: Char, y: Char): Bool = $CHAR_EQ$(x, y)
    pub override def neq(x: Char, y: Char): Bool = $CHAR_NEQ$(x, y)
}

instance Eq[Float32] {
    pub def eq(x: Float32, y: Float32): Bool = $FLOAT32_EQ$(x, y)
    pub override def neq(x: Float32, y: Float32): Bool = $FLOAT32_NEQ$(x, y)
}

instance Eq[Float64] {
    pub def eq(x: Float64, y: Float64): Bool = $FLOAT64_EQ$(x, y)
    pub override def neq(x: Float64, y: Float64): Bool = $FLOAT64_NEQ$(x, y)
}

instance Eq[Int8] {
    pub def eq(x: Int8, y: Int8): Bool = $INT8_EQ$(x, y)
    pub override def neq(x: Int8, y: Int8): Bool = $INT8_NEQ$(x, y)
}

instance Eq[Int16] {
    pub def eq(x: Int16, y: Int16): Bool = $INT16_EQ$(x, y)
    pub override def neq(x: Int16, y: Int16): Bool = $INT16_NEQ$(x, y)
}

instance Eq[Int32] {
    pub def eq(x: Int32, y: Int32): Bool = $INT16_EQ$(x, y)
    pub override def neq(x: Int32, y: Int32): Bool = $INT16_NEQ$(x, y)
}

instance Eq[Int64] {
    pub def eq(x: Int64, y: Int64): Bool = $INT64_EQ$(x, y)
    pub override def neq(x: Int64, y: Int64): Bool = $INT64_NEQ$(x, y)
}

instance Eq[BigInt] {
    pub def eq(x: BigInt, y: BigInt): Bool = $BIGINT_EQ$(x, y)
    pub override def neq(x: BigInt, y: BigInt): Bool = $BIGINT_NEQ$(x, y)
}

instance Eq[String] {
    pub def eq(x: String, y: String): Bool = $STRING_EQ$(x, y)
    pub override def neq(x: String, y: String): Bool = $STRING_NEQ$(x, y)
}

instance Eq[(a1, a2)] with Eq[a1], Eq[a2] {

    pub def eq(t1: (a1, a2), t2: (a1, a2)): Bool =
        let (x1, x2) = t1;
        let (y1, y2) = t2;
            x1 == y1 and x2 == y2

}

instance Eq[(a1, a2, a3)] with Eq[a1], Eq[a2], Eq[a3] {

    pub def eq(t1: (a1, a2, a3), t2: (a1, a2, a3)): Bool =
        let (x1, x2, x3) = t1;
        let (y1, y2, y3) = t2;
            x1 == y1 and x2 == y2 and x3 == y3

}

instance Eq[(a1, a2, a3, a4)] with Eq[a1], Eq[a2], Eq[a3], Eq[a4] {

    pub def eq(t1: (a1, a2, a3, a4), t2: (a1, a2, a3, a4)): Bool =
        let (x1, x2, x3, x4) = t1;
        let (y1, y2, y3, y4) = t2;
            x1 == y1 and x2 == y2 and x3 == y3 and x4 == y4

}

instance Eq[(a1, a2, a3, a4, a5)] with Eq[a1], Eq[a2], Eq[a3], Eq[a4], Eq[a5] {

    pub def eq(t1: (a1, a2, a3, a4, a5), t2: (a1, a2, a3, a4, a5)): Bool =
        let (x1, x2, x3, x4, x5) = t1;
        let (y1, y2, y3, y4, y5) = t2;
            x1 == y1 and x2 == y2 and x3 == y3 and x4 == y4 and x5 == y5

}

instance Eq[(a1, a2, a3, a4, a5, a6)] with Eq[a1], Eq[a2], Eq[a3], Eq[a4], Eq[a5], Eq[a6] {

    pub def eq(t1: (a1, a2, a3, a4, a5, a6), t2: (a1, a2, a3, a4, a5, a6)): Bool =
        let (x1, x2, x3, x4, x5, x6) = t1;
        let (y1, y2, y3, y4, y5, y6) = t2;
            x1 == y1 and x2 == y2 and x3 == y3 and x4 == y4 and x5 == y5 and x6 == y6

}

instance Eq[(a1, a2, a3, a4, a5, a6, a7)] with Eq[a1], Eq[a2], Eq[a3], Eq[a4], Eq[a5], Eq[a6], Eq[a7] {

    pub def eq(t1: (a1, a2, a3, a4, a5, a6, a7), t2: (a1, a2, a3, a4, a5, a6, a7)): Bool =
        let (x1, x2, x3, x4, x5, x6, x7) = t1;
        let (y1, y2, y3, y4, y5, y6, y7) = t2;
            x1 == y1 and x2 == y2 and x3 == y3 and x4 == y4 and x5 == y5 and x6 == y6 and x7 == y7

}

instance Eq[(a1, a2, a3, a4, a5, a6, a7, a8)] with Eq[a1], Eq[a2], Eq[a3], Eq[a4], Eq[a5], Eq[a6], Eq[a7], Eq[a8] {

    pub def eq(t1: (a1, a2, a3, a4, a5, a6, a7, a8), t2: (a1, a2, a3, a4, a5, a6, a7, a8)): Bool =
        let (x1, x2, x3, x4, x5, x6, x7, x8) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8) = t2;
            x1 == y1 and x2 == y2 and x3 == y3 and x4 == y4 and x5 == y5 and x6 == y6 and x7 == y7 and x8 == y8
}

instance Eq[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with Eq[a1], Eq[a2], Eq[a3], Eq[a4], Eq[a5], Eq[a6], Eq[a7], Eq[a8], Eq[a9] {

    pub def eq(t1: (a1, a2, a3, a4, a5, a6, a7, a8, a9), t2: (a1, a2, a3, a4, a5, a6, a7, a8, a9)): Bool =
        let (x1, x2, x3, x4, x5, x6, x7, x8, x9) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8, y9) = t2;
            x1 == y1 and x2 == y2 and x3 == y3 and x4 == y4 and x5 == y5 and x6 == y6 and x7 == y7 and x8 == y8 and x9 == y9

}

instance Eq[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with Eq[a1], Eq[a2], Eq[a3], Eq[a4], Eq[a5], Eq[a6], Eq[a7], Eq[a8], Eq[a9], Eq[a10] {

    pub def eq(t1: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10), t2: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)): Bool =
        let (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10) = t2;
            x1 == y1 and x2 == y2 and x3 == y3 and x4 == y4 and x5 == y5 and x6 == y6 and x7 == y7 and x8 == y8 and x9 == y9 and x10 == y10

}

instance Eq[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)] with Eq[a1], Eq[a2], Eq[a3], Eq[a4], Eq[a5], Eq[a6], Eq[a7], Eq[a8], Eq[a9], Eq[a10], Eq[a11] {

    pub def eq(t1: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11), t2: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)): Bool =
        let (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11) = t2;
            x1 == y1 and x2 == y2 and x3 == y3 and x4 == y4 and x5 == y5 and x6 == y6 and x7 == y7 and x8 == y8 and x9 == y9 and x10 == y10 and x11 == y11

}

instance Eq[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)] with Eq[a1], Eq[a2], Eq[a3], Eq[a4], Eq[a5], Eq[a6], Eq[a7], Eq[a8], Eq[a9], Eq[a10], Eq[a11], Eq[a12] {

    pub def eq(t1: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12), t2: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)): Bool =
        let (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12) = t2;
            x1 == y1 and x2 == y2 and x3 == y3 and x4 == y4 and x5 == y5 and x6 == y6 and x7 == y7 and x8 == y8 and x9 == y9 and x10 == y10 and x11 == y11 and x12 == y12

}

instance Eq[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)] with Eq[a1], Eq[a2], Eq[a3], Eq[a4], Eq[a5], Eq[a6], Eq[a7], Eq[a8], Eq[a9], Eq[a10], Eq[a11], Eq[a12], Eq[a13] {

    pub def eq(t1: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13), t2: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)): Bool =
        let (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13) = t2;
            x1 == y1 and x2 == y2 and x3 == y3 and x4 == y4 and x5 == y5 and x6 == y6 and x7 == y7 and x8 == y8 and x9 == y9 and x10 == y10 and x11 == y11 and x12 == y12 and x13 == y13

}

instance Eq[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)] with Eq[a1], Eq[a2], Eq[a3], Eq[a4], Eq[a5], Eq[a6], Eq[a7], Eq[a8], Eq[a9], Eq[a10], Eq[a11], Eq[a12], Eq[a13], Eq[a14] {

    pub def eq(t1: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14), t2: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)): Bool =
        let (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14) = t2;
            x1 == y1 and x2 == y2 and x3 == y3 and x4 == y4 and x5 == y5 and x6 == y6 and x7 == y7 and x8 == y8 and x9 == y9 and x10 == y10 and x11 == y11 and x12 == y12 and x13 == y13 and x14 == y14

}

instance Eq[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)] with Eq[a1], Eq[a2], Eq[a3], Eq[a4], Eq[a5], Eq[a6], Eq[a7], Eq[a8], Eq[a9], Eq[a10], Eq[a11], Eq[a12], Eq[a13], Eq[a14], Eq[a15] {

    pub def eq(t1: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15), t2: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)): Bool =
        let (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15) = t2;
            x1 == y1 and x2 == y2 and x3 == y3 and x4 == y4 and x5 == y5 and x6 == y6 and x7 == y7 and x8 == y8 and x9 == y9 and x10 == y10 and x11 == y11 and x12 == y12 and x13 == y13 and x14 == y14 and x15 == y15

}

///
/// A type class for types that can be converted to strings.
///
pub class ToString[a] {
    ///
    /// Returns a string representation of the given x.
    ///
    pub def toString(x: a): String
}

instance ToString[Unit] {
    pub def toString(_x: Unit): String = "()"
}

instance ToString[Bool] {
    pub def toString(x: Bool): String = match x {
        case true  => "true"
        case false => "false"
    }
}

instance ToString[Char] {
    pub def toString(x: Char): String =
        import static java.lang.Character.toString(Char): String \ {};
        toString(x)
}

instance ToString[Float32] {
    pub def toString(x: Float32): String =
        import static java.lang.Float.toString(Float32): String \ {};
        toString(x)
}

instance ToString[Float64] {
    pub def toString(x: Float64): String =
        import static java.lang.Double.toString(Float64): String \ {};
        toString(x)
}

instance ToString[Int8] {
    pub def toString(x: Int8): String =
        import static java.lang.Byte.toString(Int8): String \ {};
        toString(x)
}

instance ToString[Int16] {
    pub def toString(x: Int16): String =
        import static java.lang.Short.toString(Int16): String \ {};
        toString(x)
}

instance ToString[Int32] {
    pub def toString(x: Int32): String =
        import static java.lang.Integer.toString(Int32): String \ {};
        toString(x)
}

instance ToString[Int64] {
    pub def toString(x: Int64): String =
        import static java.lang.Long.toString(Int64): String \ {};
        toString(x)
}

instance ToString[String] {
    pub def toString(x: String): String = x
}

instance ToString[BigInt] {
    pub def toString(x: BigInt): String =
        import java.math.BigInteger.toString(): String \ {};
        toString(x)
}

instance ToString[(a1, a2)] with ToString[a1], ToString[a2] {
    pub def toString(t: (a1, a2)): String = match t {
        case (x1, x2) =>
            "(${x1}, ${x2})"
    }
}

instance ToString[(a1, a2, a3)] with ToString[a1], ToString[a2], ToString[a3] {
    pub def toString(t: (a1, a2, a3)): String = match t {
        case (x1, x2, x3) =>
            "(${x1}, ${x2}, ${x3})"
    }
}

instance ToString[(a1, a2, a3, a4)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4] {
    pub def toString(t: (a1, a2, a3, a4)): String = match t {
        case (x1, x2, x3, x4) =>
            "(${x1}, ${x2}, ${x3}, ${x4})"
    }
}

instance ToString[(a1, a2, a3, a4, a5)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5] {
    pub def toString(t: (a1, a2, a3, a4, a5)): String = match t {
        case (x1, x2, x3, x4, x5) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6] {
    pub def toString(t: (a1, a2, a3, a4, a5, a6)): String = match t {
        case (x1, x2, x3, x4, x5, x6) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7] {
    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7)): String = match t {
        case (x1, x2, x3, x4, x5, x6, x7) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7, a8)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7], ToString[a8] {
    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8)): String = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7], ToString[a8], ToString[a9] {
    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9)): String = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8, x9) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8}, ${x9})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7], ToString[a8], ToString[a9], ToString[a10] {
    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)): String = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8}, ${x9}, ${x10})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7], ToString[a8], ToString[a9], ToString[a10], ToString[a11] {
    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)): String = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8}, ${x9}, ${x10}, ${x11})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7], ToString[a8], ToString[a9], ToString[a10], ToString[a11], ToString[a12] {
    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)): String = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8}, ${x9}, ${x10}, ${x11}, ${x12})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7], ToString[a8], ToString[a9], ToString[a10], ToString[a11], ToString[a12], ToString[a13] {
    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)): String = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8}, ${x9}, ${x10}, ${x11}, ${x12}, ${x13})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7], ToString[a8], ToString[a9], ToString[a10], ToString[a11], ToString[a12], ToString[a13], ToString[a14] {
    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)): String = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8}, ${x9}, ${x10}, ${x11}, ${x12}, ${x13}, ${x14})"
    }
}

instance ToString[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)] with ToString[a1], ToString[a2], ToString[a3], ToString[a4], ToString[a5], ToString[a6], ToString[a7], ToString[a8], ToString[a9], ToString[a10], ToString[a11], ToString[a12], ToString[a13], ToString[a14], ToString[a15] {
    pub def toString(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)): String = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) =>
            "(${x1}, ${x2}, ${x3}, ${x4}, ${x5}, ${x6}, ${x7}, ${x8}, ${x9}, ${x10}, ${x11}, ${x12}, ${x13}, ${x14}, ${x15})"
    }
}

instance ToString[Array[a, r]] with ToString[a] {
    pub def toString(a: Array[a, r]): String =
        def loop(i, acc) =
            if (i < a.length) {
                if (i == 0) loop(i + 1, "${a[i]}") as \ {}
                else loop(i + 1, "${acc}, ${a[i]}") as \ {}
            } else {
                "[${acc}]"
            };
        loop(0, "")
}

pub enum Comparison with Eq, ToString {
    case LessThan
    case EqualTo
    case GreaterThan
}

namespace Comparison {

    ///
    /// Converts a `Comparison` value to an integer.
    /// The return value is:
    /// `-1` if `c` is `LessThan`.
    /// `0` if `c` is `EqualTo`.
    /// `+1` if `c` is `GreaterThan`.
    ///
    pub def toInt(c: Comparison): Int32 = match c {
        case LessThan => -1
        case EqualTo => 0
        case GreaterThan => +1
    }

    ///
    /// Converts an integer to a `Comparison` value.
    /// The return value is:
    /// `LessThan` if `i < 0`.
    /// `EqualTo` if `i == 0`.
    /// `GreaterThan` if `i > 0`.
    ///
    pub def fromInt32(i: Int32): Comparison =
        if (i < 0) LessThan
        else if (i == 0) EqualTo
        else GreaterThan

}

///
/// A type class for types with a total order.
///
pub lawful class Order[a] with Eq[a] {

    ///
    /// Returns `LessThan` if `x` < `y`, `Equal` if `x` == `y` or `GreaterThan` `if `x` > `y`.
    ///
    pub def compare(x: a, y: a): Comparison

    ///
    /// Returns `true` if and only if `x < y`.
    ///
    pub def less(x: a, y: a): Bool = match Order.compare(x, y) {
        case LessThan   => true
        case _          => false
    }

    ///
    /// Returns `true` if and only if `x <= y`.
    ///
    pub def lessEqual(x: a, y: a): Bool = match Order.compare(x, y) {
        case LessThan   => true
        case EqualTo    => true
        case _          => false
    }

    ///
    /// Returns `true` if and only if `x > y`.
    ///
    pub def greater(x: a, y: a): Bool = match Order.compare(x, y) {
        case GreaterThan    => true
        case _              => false
    }

    ///
    /// Returns `true` if and only if `x >= y`.
    ///
    pub def greaterEqual(x: a, y: a): Bool = match Order.compare(x, y) {
        case GreaterThan    => true
        case EqualTo        => true
        case _              => false
    }

    ///
    /// Returns the minimum of `x` and `y`.
    ///
    pub def min(x: a, y: a): a = match (x <=> y) {
        case GreaterThan => y
        case _ => x
    }

    ///
    /// Returns the maximum of `x` and `y`.
    ///
    pub def max(x: a, y: a): a = match (x <=> y) {
        case LessThan => y
        case _ => x
    }

    ///
    /// Reflexivity: An element `x` is lower or equal to itself.
    ///
    law reflexivity: forall(x: a). x <= x

    ///
    /// Antisymmetry: If `x` is lower or equal to `y` and `y` is lower or equal to `x` then `x` must be equal to `y`.
    ///
    law symmetry: forall(x: a, y: a). ((x <= y) and (y <= x)) ==> (x == y)

    ///
    /// Transitivity: If `x` is lower or equal to `y` and `y` is lower equal to `z` then `x` must be lower or equal to `z`.
    ///
    law transitivity: forall(x: a, y: a, z: a). ((x <= y) and (y <= z)) ==> (x <= z)

    ///
    /// Totality: For each two elements `x` and `y` either `x` is lower or equal to `y` or the other way round.
    ///
    law totality: forall(x: a, y: a). ((x <= y) or (y <= x))

    ///
    /// Definition of the minimum function.
    ///
    law min: forall(x: a, y: a). ((x <= y) ==> (Order.min(x, y) == x)) and ((y <= x) ==> (Order.min(x, y) == y))

    ///
    /// Definition of the maximum function.
    ///
    law max: forall(x: a, y: a). ((y <= x) ==> (Order.max(x, y) == x)) and ((x <= y) ==> (Order.max(x, y) == y))

    ///
    /// x < y is logically equivalent to not (y <= x).
    /// This law defines the associated strict total order "<" associated with "<=".
    ///
    law strictTotalOrder: forall(x: a, y: a). (x < y) <==> (not (y <= x))

    ///
    /// This law defines ">" based on "<".
    ///
    law inverseOrder1: forall(x: a, y: a). (x > y) <==> (y < x)

    ///
    /// This law defines ">=" based on "<=".
    ///
    law inverseOrder2: forall(x: a, y: a). (x >= y) <==> (y <= x)

    ///
    /// Definition of `compare` based on the defined total order.
    ///
    law compare: forall(x: a, y: a). ((x < y) ==> (Order.compare(x, y) == LessThan)) and ((x == y) ==> (Order.compare(x, y) == EqualTo)) and ((x > y) ==> (Order.compare(x, y) == GreaterThan))
}

namespace Order {

    ///
    /// Returns the minimum of `x` and `y` according to the given comparator `cmp`.
    ///
    pub def minBy(cmp: (a, a) -> Comparison, x: a, y: a): a =
        if (cmp(x, y) == GreaterThan) y else x

    ///
    /// Returns the maximum of `x` and `y` according to the given comparator `cmp`.
    ///
    pub def maxBy(cmp: (a, a) -> Comparison, x: a, y: a): a =
        if (cmp(x, y) == LessThan) y else x

    ///
    /// Lazily combines two comparisons.
    ///
    /// If `c1` is either `LessThan` or `GreaterThan` then `c2` is never evaluated.
    ///
    pub def thenCompare(c1: Comparison, c2: Lazy[Comparison]): Comparison =
        if (c1 != EqualTo) c1 else force c2

}

instance Order[Unit] {

    pub def compare(_: Unit, _: Unit): Comparison = EqualTo

}

instance Order[Bool] {

    pub def compare(x: Bool, y: Bool): Comparison =
        if (x) {
            if (y)
                EqualTo
            else
                GreaterThan
        } else {
            if (y)
                LessThan
            else
                EqualTo
        }

}

instance Order[Char] {

    pub override def less(x: Char, y: Char): Bool = $CHAR_LT$(x, y)

    pub override def lessEqual(x: Char, y: Char): Bool = $CHAR_LE$(x, y)

    pub override def greater(x: Char, y: Char): Bool = $CHAR_GT$(x, y)

    pub override def greaterEqual(x: Char, y: Char): Bool = $CHAR_GE$(x, y)

    pub def compare(x: Char, y: Char): Comparison =
        if ($CHAR_LT$(x, y))
            LessThan
        else if ($CHAR_GT$(x, y))
            GreaterThan
        else
            EqualTo

}

instance Order[Float32] {

    pub override def less(x: Float32, y: Float32): Bool = $FLOAT32_LT$(x, y)

    pub override def lessEqual(x: Float32, y: Float32): Bool = $FLOAT32_LE$(x, y)

    pub override def greater(x: Float32, y: Float32): Bool = $FLOAT32_GT$(x, y)

    pub override def greaterEqual(x: Float32, y: Float32): Bool = $FLOAT32_GE$(x, y)

    pub def compare(x: Float32, y: Float32): Comparison =
        if ($FLOAT32_LT$(x, y))
            LessThan
        else if ($FLOAT32_GT$(x, y))
            GreaterThan
        else
            EqualTo

}

instance Order[Float64] {

    pub override def less(x: Float64, y: Float64): Bool = $FLOAT64_LT$(x, y)

    pub override def lessEqual(x: Float64, y: Float64): Bool = $FLOAT64_LE$(x, y)

    pub override def greater(x: Float64, y: Float64): Bool = $FLOAT64_GT$(x, y)

    pub override def greaterEqual(x: Float64, y: Float64): Bool = $FLOAT64_GE$(x, y)

    pub def compare(x: Float64, y: Float64): Comparison =
        if ($FLOAT64_LT$(x, y))
            LessThan
        else if ($FLOAT64_GT$(x, y))
            GreaterThan
        else
            EqualTo

}

instance Order[Int8] {

    pub override def less(x: Int8, y: Int8): Bool = $INT8_LT$(x, y)

    pub override def lessEqual(x: Int8, y: Int8): Bool = $INT8_LE$(x, y)

    pub override def greater(x: Int8, y: Int8): Bool = $INT8_GT$(x, y)

    pub override def greaterEqual(x: Int8, y: Int8): Bool = $INT8_GE$(x, y)

    pub def compare(x: Int8, y: Int8): Comparison =
        if ($INT8_LT$(x, y))
            LessThan
        else if ($INT8_GT$(x, y))
            GreaterThan
        else
            EqualTo

}

instance Order[Int16] {

    pub override def less(x: Int16, y: Int16): Bool = $INT16_LT$(x, y)

    pub override def lessEqual(x: Int16, y: Int16): Bool = $INT16_LE$(x, y)

    pub override def greater(x: Int16, y: Int16): Bool = $INT16_GT$(x, y)

    pub override def greaterEqual(x: Int16, y: Int16): Bool = $INT16_GE$(x, y)

    pub def compare(x: Int16, y: Int16): Comparison =
        if ($INT16_LT$(x, y))
            LessThan
        else if ($INT16_GT$(x, y))
            GreaterThan
        else
            EqualTo

}

instance Order[Int32] {

    pub override def less(x: Int32, y: Int32): Bool = $INT32_LT$(x, y)

    pub override def lessEqual(x: Int32, y: Int32): Bool = $INT32_LE$(x, y)

    pub override def greater(x: Int32, y: Int32): Bool = $INT32_GT$(x, y)

    pub override def greaterEqual(x: Int32, y: Int32): Bool = $INT32_GE$(x, y)

    pub def compare(x: Int32, y: Int32): Comparison =
        if ($INT32_LT$(x, y))
            LessThan
        else if ($INT32_GT$(x, y))
            GreaterThan
        else
            EqualTo

}

instance Order[Int64] {

    pub override def less(x: Int64, y: Int64): Bool = $INT64_LT$(x, y)

    pub override def lessEqual(x: Int64, y: Int64): Bool = $INT64_LE$(x, y)

    pub override def greater(x: Int64, y: Int64): Bool = $INT64_GT$(x, y)

    pub override def greaterEqual(x: Int64, y: Int64): Bool = $INT64_GE$(x, y)

    pub def compare(x: Int64, y: Int64): Comparison =
        if ($INT64_LT$(x, y))
            LessThan
        else if ($INT64_GT$(x, y))
            GreaterThan
        else
            EqualTo

}

instance Order[BigInt] {

    pub override def less(x: BigInt, y: BigInt): Bool = $BIGINT_LT$(x, y)

    pub override def lessEqual(x: BigInt, y: BigInt): Bool = $BIGINT_LE$(x, y)

    pub override def greater(x: BigInt, y: BigInt): Bool = $BIGINT_GT$(x, y)

    pub override def greaterEqual(x: BigInt, y: BigInt): Bool = $BIGINT_GE$(x, y)

    pub def compare(x: BigInt, y: BigInt): Comparison =
        if ($BIGINT_LT$(x, y))
            LessThan
        else if ($BIGINT_GT$(x, y))
            GreaterThan
        else
            EqualTo

}

instance Order[String] {

    pub def compare(x: String, y: String): Comparison =
        import java.lang.String.compareTo(String): Int32 \ {};
        Comparison.fromInt32((x `compareTo` y))

}

instance Order[(a1, a2)] with Order[a1], Order[a2] {

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    pub def compare(t1: (a1, a2), t2: (a1, a2)): Comparison =
        use Order.thenCompare;
        let (x1, x2) = t1;
        let (y1, y2) = t2;
             (x1 <=> y1) `thenCompare`
        lazy (x2 <=> y2)

}

instance Order[(a1, a2, a3)] with Order[a1], Order[a2], Order[a3] {

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    pub def compare(t1: (a1, a2, a3), t2: (a1, a2, a3)): Comparison =
        use Order.thenCompare;
        let (x1, x2, x3) = t1;
        let (y1, y2, y3) = t2;
             (x1 <=> y1) `thenCompare`
        lazy (x2 <=> y2) `thenCompare`
        lazy (x3 <=> y3)

}

instance Order[(a1, a2, a3, a4)] with Order[a1], Order[a2], Order[a3], Order[a4] {

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    pub def compare(t1: (a1, a2, a3, a4), t2: (a1, a2, a3, a4)): Comparison =
        use Order.thenCompare;
        let (x1, x2, x3, x4) = t1;
        let (y1, y2, y3, y4) = t2;
             (x1 <=> y1) `thenCompare`
        lazy (x2 <=> y2) `thenCompare`
        lazy (x3 <=> y3) `thenCompare`
        lazy (x4 <=> y4)

}

instance Order[(a1, a2, a3, a4, a5)] with Order[a1], Order[a2], Order[a3], Order[a4], Order[a5] {

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    pub def compare(t1: (a1, a2, a3, a4, a5), t2: (a1, a2, a3, a4, a5)): Comparison =
        use Order.thenCompare;
        let (x1, x2, x3, x4, x5) = t1;
        let (y1, y2, y3, y4, y5) = t2;
             (x1 <=> y1) `thenCompare`
        lazy (x2 <=> y2) `thenCompare`
        lazy (x3 <=> y3) `thenCompare`
        lazy (x4 <=> y4) `thenCompare`
        lazy (x5 <=> y5)

}

instance Order[(a1, a2, a3, a4, a5, a6)] with Order[a1], Order[a2], Order[a3], Order[a4], Order[a5], Order[a6] {

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    pub def compare(t1: (a1, a2, a3, a4, a5, a6), t2: (a1, a2, a3, a4, a5, a6)): Comparison =
        use Order.thenCompare;
        let (x1, x2, x3, x4, x5, x6) = t1;
        let (y1, y2, y3, y4, y5, y6) = t2;
             (x1 <=> y1) `thenCompare`
        lazy (x2 <=> y2) `thenCompare`
        lazy (x3 <=> y3) `thenCompare`
        lazy (x4 <=> y4) `thenCompare`
        lazy (x5 <=> y5) `thenCompare`
        lazy (x6 <=> y6)

}

instance Order[(a1, a2, a3, a4, a5, a6, a7)] with Order[a1], Order[a2], Order[a3], Order[a4], Order[a5], Order[a6], Order[a7] {

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    pub def compare(t1: (a1, a2, a3, a4, a5, a6, a7), t2: (a1, a2, a3, a4, a5, a6, a7)): Comparison =
        use Order.thenCompare;
        let (x1, x2, x3, x4, x5, x6, x7) = t1;
        let (y1, y2, y3, y4, y5, y6, y7) = t2;
             (x1 <=> y1) `thenCompare`
        lazy (x2 <=> y2) `thenCompare`
        lazy (x3 <=> y3) `thenCompare`
        lazy (x4 <=> y4) `thenCompare`
        lazy (x5 <=> y5) `thenCompare`
        lazy (x6 <=> y6) `thenCompare`
        lazy (x7 <=> y7)

}

instance Order[(a1, a2, a3, a4, a5, a6, a7, a8)] with Order[a1], Order[a2], Order[a3], Order[a4], Order[a5], Order[a6], Order[a7], Order[a8] {

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    pub def compare(t1: (a1, a2, a3, a4, a5, a6, a7, a8), t2: (a1, a2, a3, a4, a5, a6, a7, a8)): Comparison =
        use Order.thenCompare;
        let (x1, x2, x3, x4, x5, x6, x7, x8) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8) = t2;
             (x1 <=> y1) `thenCompare`
        lazy (x2 <=> y2) `thenCompare`
        lazy (x3 <=> y3) `thenCompare`
        lazy (x4 <=> y4) `thenCompare`
        lazy (x5 <=> y5) `thenCompare`
        lazy (x6 <=> y6) `thenCompare`
        lazy (x7 <=> y7) `thenCompare`
        lazy (x8 <=> y8)

}

instance Order[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with Order[a1], Order[a2], Order[a3], Order[a4], Order[a5], Order[a6], Order[a7], Order[a8], Order[a9] {

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    pub def compare(t1: (a1, a2, a3, a4, a5, a6, a7, a8, a9), t2: (a1, a2, a3, a4, a5, a6, a7, a8, a9)): Comparison =
        use Order.thenCompare;
        let (x1, x2, x3, x4, x5, x6, x7, x8, x9) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8, y9) = t2;
             (x1 <=> y1) `thenCompare`
        lazy (x2 <=> y2) `thenCompare`
        lazy (x3 <=> y3) `thenCompare`
        lazy (x4 <=> y4) `thenCompare`
        lazy (x5 <=> y5) `thenCompare`
        lazy (x6 <=> y6) `thenCompare`
        lazy (x7 <=> y7) `thenCompare`
        lazy (x8 <=> y8) `thenCompare`
        lazy (x9 <=> y9)

}

instance Order[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with Order[a1], Order[a2], Order[a3], Order[a4], Order[a5], Order[a6], Order[a7], Order[a8], Order[a9], Order[a10] {

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    pub def compare(t1: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10), t2: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)): Comparison =
        use Order.thenCompare;
        let (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10) = t2;
             (x1 <=> y1) `thenCompare`
        lazy (x2 <=> y2) `thenCompare`
        lazy (x3 <=> y3) `thenCompare`
        lazy (x4 <=> y4) `thenCompare`
        lazy (x5 <=> y5) `thenCompare`
        lazy (x6 <=> y6) `thenCompare`
        lazy (x7 <=> y7) `thenCompare`
        lazy (x8 <=> y8) `thenCompare`
        lazy (x9 <=> y9) `thenCompare`
        lazy (x10 <=> y10)

}

instance Order[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)] with Order[a1], Order[a2], Order[a3], Order[a4], Order[a5], Order[a6], Order[a7], Order[a8], Order[a9], Order[a10], Order[a11] {

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    pub def compare(t1: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11), t2: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)): Comparison =
        use Order.thenCompare;
        let (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11) = t2;
             (x1 <=> y1) `thenCompare`
        lazy (x2 <=> y2) `thenCompare`
        lazy (x3 <=> y3) `thenCompare`
        lazy (x4 <=> y4) `thenCompare`
        lazy (x5 <=> y5) `thenCompare`
        lazy (x6 <=> y6) `thenCompare`
        lazy (x7 <=> y7) `thenCompare`
        lazy (x8 <=> y8) `thenCompare`
        lazy (x9 <=> y9) `thenCompare`
        lazy (x10 <=> y10) `thenCompare`
        lazy (x11 <=> y11)
}

instance Order[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)] with Order[a1], Order[a2], Order[a3], Order[a4], Order[a5], Order[a6], Order[a7], Order[a8], Order[a9], Order[a10], Order[a11], Order[a12] {

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    pub def compare(t1: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12), t2: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)): Comparison =
        use Order.thenCompare;
        let (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12) = t2;
             (x1 <=> y1) `thenCompare`
        lazy (x2 <=> y2) `thenCompare`
        lazy (x3 <=> y3) `thenCompare`
        lazy (x4 <=> y4) `thenCompare`
        lazy (x5 <=> y5) `thenCompare`
        lazy (x6 <=> y6) `thenCompare`
        lazy (x7 <=> y7) `thenCompare`
        lazy (x8 <=> y8) `thenCompare`
        lazy (x9 <=> y9) `thenCompare`
        lazy (x10 <=> y10) `thenCompare`
        lazy (x11 <=> y11) `thenCompare`
        lazy (x12 <=> y12)
}

instance Order[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)] with Order[a1], Order[a2], Order[a3], Order[a4], Order[a5], Order[a6], Order[a7], Order[a8], Order[a9], Order[a10], Order[a11], Order[a12], Order[a13] {

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    pub def compare(t1: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13), t2: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)): Comparison =
        use Order.thenCompare;
        let (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13) = t2;
             (x1 <=> y1) `thenCompare`
        lazy (x2 <=> y2) `thenCompare`
        lazy (x3 <=> y3) `thenCompare`
        lazy (x4 <=> y4) `thenCompare`
        lazy (x5 <=> y5) `thenCompare`
        lazy (x6 <=> y6) `thenCompare`
        lazy (x7 <=> y7) `thenCompare`
        lazy (x8 <=> y8) `thenCompare`
        lazy (x9 <=> y9) `thenCompare`
        lazy (x10 <=> y10) `thenCompare`
        lazy (x11 <=> y11) `thenCompare`
        lazy (x12 <=> y12) `thenCompare`
        lazy (x13 <=> y13)
}

instance Order[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)] with Order[a1], Order[a2], Order[a3], Order[a4], Order[a5], Order[a6], Order[a7], Order[a8], Order[a9], Order[a10], Order[a11], Order[a12], Order[a13], Order[a14] {

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    pub def compare(t1: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14), t2: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)): Comparison =
        use Order.thenCompare;
        let (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14) = t2;
             (x1 <=> y1) `thenCompare`
        lazy (x2 <=> y2) `thenCompare`
        lazy (x3 <=> y3) `thenCompare`
        lazy (x4 <=> y4) `thenCompare`
        lazy (x5 <=> y5) `thenCompare`
        lazy (x6 <=> y6) `thenCompare`
        lazy (x7 <=> y7) `thenCompare`
        lazy (x8 <=> y8) `thenCompare`
        lazy (x9 <=> y9) `thenCompare`
        lazy (x10 <=> y10) `thenCompare`
        lazy (x11 <=> y11) `thenCompare`
        lazy (x12 <=> y12) `thenCompare`
        lazy (x13 <=> y13) `thenCompare`
        lazy (x14 <=> y14)

}

instance Order[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)] with Order[a1], Order[a2], Order[a3], Order[a4], Order[a5], Order[a6], Order[a7], Order[a8], Order[a9], Order[a10], Order[a11], Order[a12], Order[a13], Order[a14], Order[a15] {

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    pub def compare(t1: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15), t2: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)): Comparison =
        use Order.thenCompare;
        let (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) = t1;
        let (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15) = t2;
             (x1 <=> y1) `thenCompare`
        lazy (x2 <=> y2) `thenCompare`
        lazy (x3 <=> y3) `thenCompare`
        lazy (x4 <=> y4) `thenCompare`
        lazy (x5 <=> y5) `thenCompare`
        lazy (x6 <=> y6) `thenCompare`
        lazy (x7 <=> y7) `thenCompare`
        lazy (x8 <=> y8) `thenCompare`
        lazy (x9 <=> y9) `thenCompare`
        lazy (x10 <=> y10) `thenCompare`
        lazy (x11 <=> y11) `thenCompare`
        lazy (x12 <=> y12) `thenCompare`
        lazy (x13 <=> y13) `thenCompare`
        lazy (x14 <=> y14) `thenCompare`
        lazy (x15 <=> y15)

}

namespace Bool {

    ///
    /// Alias for logical conjunction.
    ///
    @Time(1) @Space(1)
    pub def ∧(x: Bool, y: Bool): Bool = x and y

    ///
    /// Alias for logical disjunction.
    ///
    @Time(1) @Space(1)
    pub def ∨(x: Bool, y: Bool): Bool = x or y

    ///
    /// Alias for logical implication.
    ///
    @Time(1) @Space(1)
    pub def →(x: Bool, y: Bool): Bool = not x ∨ y

    ///
    /// Alias for logical bi-implication.
    ///
    @Time(1) @Space(1)
    pub def ↔(x: Bool, y: Bool): Bool = (x → y) ∧ (y → x)

    ///
    /// Alias for exclusive or.
    ///
    @Time(1) @Space(1)
    pub def ⊕(x: Bool, y: Bool): Bool = (x ∨ y) ∧ not (x ∧ y)

    ///
    /// Negation of `x`
    ///
    pub def not(x: Bool): Bool = not x

    ///
    /// Logical and.
    ///
    pub def and(x: Bool, y: Lazy[Bool]): Bool = if(x) force y else x

    ///
    /// Logical or.
    ///
    pub def or(x: Bool, y: Lazy[Bool]): Bool = if(x) x else force y

    ///
    /// Returns `true` if `x` implies `y` logically holds.
    ///
    pub def ==>(x: Bool, y: Bool): Bool = not x or y

    ///
    /// Returns true if `x` implies `y` and vise versa.
    ///
    pub def <==>(x: Bool, y: Bool): Bool = x == y

}

///
/// A type class for addition.
///
pub class Add[a] {
    ///
    /// Returns the sum of `x` and `y`.
    ///
    pub def add(x: a, y: a): a
}

instance Add[Float32] {
    pub def add(x: Float32, y: Float32): Float32 = $FLOAT32_ADD$(x, y)
}

instance Add[Float64] {
    pub def add(x: Float64, y: Float64): Float64 = $FLOAT64_ADD$(x, y)
}

instance Add[Int8] {
    pub def add(x: Int8, y: Int8): Int8 = $INT8_ADD$(x, y)
}

instance Add[Int16] {
    pub def add(x: Int16, y: Int16): Int16 = $INT16_ADD$(x, y)
}

instance Add[Int32] {
    pub def add(x: Int32, y: Int32): Int32 = $INT32_ADD$(x, y)
}

instance Add[Int64] {
    pub def add(x: Int64, y: Int64): Int64 = $INT64_ADD$(x, y)
}

instance Add[BigInt] {
    pub def add(x: BigInt, y: BigInt): BigInt = $BIGINT_ADD$(x, y)
}

instance Add[String] {
    pub def add(x: String, y: String): String =
        import java.lang.String.concat(String): String \ {};
        (x `concat` y)
}

///
/// A type class for subtraction.
///
pub class Sub[a] {
    ///
    /// Returns the difference of `x` and `y`.
    ///
    pub def sub(x: a, y: a): a
}

instance Sub[Float32] {
    pub def sub(x: Float32, y: Float32): Float32 = $FLOAT32_SUB$(x, y)
}

instance Sub[Float64] {
    pub def sub(x: Float64, y: Float64): Float64 = $FLOAT64_SUB$(x, y)
}

instance Sub[Int8] {
    pub def sub(x: Int8, y: Int8): Int8 = $INT8_SUB$(x, y)
}

instance Sub[Int16] {
    pub def sub(x: Int16, y: Int16): Int16 = $INT16_SUB$(x, y)
}

instance Sub[Int32] {
    pub def sub(x: Int32, y: Int32): Int32 = $INT32_SUB$(x, y)
}

instance Sub[Int64] {
    pub def sub(x: Int64, y: Int64): Int64 = $INT64_SUB$(x, y)
}

instance Sub[BigInt] {
    pub def sub(x: BigInt, y: BigInt): BigInt = $BIGINT_SUB$(x, y)
}

///
/// A type class for multiplication.
///
pub class Mul[a] {
    ///
    /// Returns `x` multiplied by `y`.
    ///
    pub def mul(x: a, y: a): a
}

instance Mul[Float32] {
    pub def mul(x: Float32, y: Float32): Float32 = $FLOAT32_MUL$(x, y)
}

instance Mul[Float64] {
    pub def mul(x: Float64, y: Float64): Float64 = $FLOAT64_MUL$(x, y)
}

instance Mul[Int8] {
    pub def mul(x: Int8, y: Int8): Int8 = $INT8_MUL$(x, y)
}

instance Mul[Int16] {
    pub def mul(x: Int16, y: Int16): Int16 = $INT16_MUL$(x, y)
}

instance Mul[Int32] {
    pub def mul(x: Int32, y: Int32): Int32 = $INT32_MUL$(x, y)
}

instance Mul[Int64] {
    pub def mul(x: Int64, y: Int64): Int64 = $INT64_MUL$(x, y)
}

instance Mul[BigInt] {
    pub def mul(x: BigInt, y: BigInt): BigInt = $BIGINT_MUL$(x, y)
}

///
/// A type class for remainder.
///
pub class Rem[a] {
    ///
    /// Returns the remainder of `x / n`.
    ///
    pub def rem(x: a, n: a): a
}

instance Rem[Int8] {
    pub def rem(x: Int8, n: Int8): Int8 =
        if (n == 0i8)
            0i8
        else
            $INT8_REM$(x, n)
}

instance Rem[Int16] {
    pub def rem(x: Int16, n: Int16): Int16 =
        if (n == 0i16)
            0i16
        else
            $INT16_REM$(x, n)
}

instance Rem[Int32] {
    pub def rem(x: Int32, n: Int32): Int32 =
        if (n == 0i32)
            0i32
        else
            $INT32_REM$(x, n)
}

instance Rem[Int64] {
    pub def rem(x: Int64, n: Int64): Int64 =
        if (n == 0i64)
            0i64
        else
            $INT64_REM$(x, n)
}

instance Rem[BigInt] {
    pub def rem(x: BigInt, n: BigInt): BigInt =
        if (n == 0ii)
            0ii
        else
            $BIGINT_REM$(x, n)
}

///
/// A type class for negation.
///
pub class Neg[a] {
    ///
    /// Returns -`x`.
    ///
    pub def neg(x: a): a
}

instance Neg[Float32] {
    pub def neg(x: Float32): Float32 = $FLOAT32_NEG$(x)
}

instance Neg[Float64] {
    pub def neg(x: Float64): Float64 = $FLOAT64_NEG$(x)
}

instance Neg[Int8] {
    pub def neg(x: Int8): Int8 = $INT8_NEG$(x)
}

instance Neg[Int16] {
    pub def neg(x: Int16): Int16 = $INT16_NEG$(x)
}

instance Neg[Int32] {
    pub def neg(x: Int32): Int32 = $INT32_NEG$(x)
}

instance Neg[Int64] {
    pub def neg(x: Int64): Int64 = $INT64_NEG$(x)
}

instance Neg[BigInt] {
    pub def neg(x: BigInt): BigInt = $BIGINT_NEG$(x)
}

///
/// Static is a type alias for false and denotes the global lifetime.
///
pub type alias Static = false

///
/// The Reified Flix Bools.
///
pub enum ReifiedBool with Eq, Order, ToString {
    case ErasedBool,
    case ReifiedTrue,
    case ReifiedFalse
}

///
/// The Reified Flix Types.
///
pub enum ReifiedType with Eq, Order, ToString {
    case ErasedType,
    case ReifiedUnit,
    case ReifiedBool,
    case ReifiedChar,
    case ReifiedFloat32,
    case ReifiedFloat64,
    case ReifiedInt8,
    case ReifiedInt16,
    case ReifiedInt32,
    case ReifiedInt64,
    case ReifiedBigInt,
    case ReifiedString,
    case ReifiedArray(ReifiedType)
}

///
/// The Generic IO Effect.
///
/// The `IO` effect is the most general effect of any
/// operation that interacts with the environment.
///
pub eff IO

///
/// The Non-Deterministic Effect.
///
/// The `NonDet` effect represents "functions" that behave like relations,
/// i.e. they have no side-effects, and given the same input their output
//// there is a fixed set of output.
///
pub eff NonDet

///
/// The identity function.
///
pub def identity(x: a): a = x

///
/// Returns the function `f` with input arguments swapped.
/// That is, given the function `f: (a, b) -> c`, returns a function `(b, a) -> c`
///
pub def flip(f: (a, b) -> c \ ef): (b, a) -> c \ ef = (x, y) -> f(y, x)

///
/// Partially applying this function in the form ``f `on` g`` lifts the binary function `f` to work on
/// inputs transformed by `g`.
///
pub def on(f: (b, b) -> c \ ef1, g: a -> b \ ef2, x: a, y: a): c \ { ef1, ef2 } = f(g(x), g(y))

///
/// Returns the first component of `t`.
///
pub def fst(p: (a, b)): a =
    let (x, _) = p;
        x

///
/// Returns the second component of `t`.
///
pub def snd(p: (a, b)): b =
    let (_, y) = p;
        y

///
/// Returns the pair `p` with the components swapped.
/// That is, returns `(y, x)` if `p = (x, y)`.
///
pub def swap(p: (a, b)): (b, a) =
    let (x, y) = p;
        (y, x)

///
/// Forwards function composition. Applies the function on the left first.
///
/// Given the functions `f: a -> b` and `g: b -> c` returns a function `a -> c`
///
pub def >>(f: a -> b \ ef1, g: b -> c \ ef2): a -> c \ { ef1, ef2 } = x -> g(f(x))

///
/// Pipes the given value `x` into the function `f`.
///
/// Given a value `x` and a function `f` returns `f(x)`.
///
pub def |>(x: a, f: a -> b \ ef): b \ ef = f(x)

///
/// Pipes the given pair `x` into the function `f`.
///
/// Given a pair `x` and a function `f` returns `f(x)`.
///
pub def ||>(x: (a, b), f: a -> (b -> c \ ef)): c \ ef = f(fst(x), snd(x))

///
/// Pipes the given value `x` into the function `f`.
///
/// Given a value `x` and a function `f` returns `x`.
///
pub def !>( x: a, f: a -> Unit \ ef): a \ ef = f(x); x

///
/// Converts `x` to a string and prints it to standard out.
///
pub def print(x: a): Unit \ IO with ToString[a] =
    import static get java.lang.System.out: ##java.io.PrintStream \ IO as getOut;
    import java.io.PrintStream.print(String): Unit \ IO;
    x |> ToString.toString |> print(getOut())

///
/// Converts `x` to a string and prints it to standard out followed by a new line.
///
pub def println(x: a): Unit \ IO with ToString[a] =
    import static get java.lang.System.out: ##java.io.PrintStream \ IO as getOut;
    import java.io.PrintStream.println(String): Unit \ IO;
    x |> ToString.toString |> println(getOut())

///
/// Prints the stringified representation of `x`.
///
/// WARNING: The debug function is pure hence if the
/// result is unused it may be removed by the compiler!
///
def debug(x: a): a = debugWithPrefix("", x) as \ {}

///
/// Prints the stringified representation of `x`.
///
/// WARNING: The debug function is pure hence if the
/// result is unused it may be removed by the compiler!
///
def debug!(x: a): a = debugWithPrefix("", x) as \ {}

///
/// Prints the stringified representation of `x`.
///
/// WARNING: The debug function is pure hence if the
/// result is unused it may be removed by the compiler!
///
def debug!!(x: a): a = debugWithPrefix("", x) as \ {}

///
/// Prints the stringified representation of `x` with the given `prefix`.
///
/// Note: For internal use only. The actual `debug(exp)` expression is parsed and ultimately rewritten to call this function.
///
@Internal
def debugWithPrefix(prefix: String, x: a): a \ IO =
    import static get java.lang.System.out: ##java.io.PrintStream \ IO as getOut;
    import java.io.PrintStream.println(String): Unit \ IO;
    println(getOut(), "${prefix}%{x}");
    x

///
/// Escapes the given string per Flix's escaping rules.
///
@Internal
pub def escape(s: String): String = {
    import java.lang.String.replace(##java.lang.CharSequence, ##java.lang.CharSequence): String \ {} as jReplace;
    def replace(old: String, new: String, subject: String): String = jReplace(subject, old as ##java.lang.CharSequence, new as ##java.lang.CharSequence);
    s
        // NB: \\ must come first to avoid clobbering other cases
        |> replace("\\", "\\\\")
        |> replace("\n", "\\n")
        |> replace("\r", "\\r")
        |> replace("\"", "\\\"")
        |> replace("\'", "\\\'")
        |> replace("\t", "\\t")
}

///
/// Returns an automatic string representation of `x`.
///
@Internal
pub def stringify(x: a): String = {
    let magic = match reifyType a {
        case ErasedType =>
            import java.lang.Object.toString(): String;
            toString as a -> String
        case ReifiedUnit =>
            _ -> "()"
        case ReifiedBool =>
            (b -> if (b) "true" else "false") as a -> String
        case ReifiedChar =>
            (c -> "\'" + escape("${c: Char}") + "\'") as a -> String
        case ReifiedFloat32 =>
            import static java.lang.Float.toString(Float32): String \ {};
            (y -> toString(y) + "f32") as a -> String
        case ReifiedFloat64 =>
            import static java.lang.Double.toString(Float64): String \ {};
            toString as a -> String
        case ReifiedInt8 =>
            import static java.lang.Byte.toString(Int8): String \ {};
            (y -> toString(y) + "i8") as a -> String
        case ReifiedInt16 =>
            import static java.lang.Short.toString(Int16): String \ {};
            (y -> toString(y) + "i16") as a -> String
        case ReifiedInt32 =>
            import static java.lang.Integer.toString(Int32): String \ {};
            toString as a -> String
        case ReifiedInt64 =>
            import static java.lang.Long.toString(Int64): String \ {};
            (y -> toString(y) + "i64") as a -> String
        case ReifiedBigInt =>
            import java.lang.Object.toString(): String;
            (y -> toString(y) + "ii") as a -> String
        case ReifiedString =>
            (s -> "\"" + escape(s) + "\"") as a -> String
        case ReifiedArray(tpe) =>
            match tpe {
                case ErasedType =>
                    import static java.util.Arrays.toString(Array[##java.lang.Object, Static]): String;
                    toString as a -> String
                case ReifiedUnit =>
                    import static java.util.Arrays.toString(Array[##java.lang.Object, Static]): String;
                    toString as a -> String
                case ReifiedBool =>
                    import static java.util.Arrays.toString(Array[Bool, Static]): String;
                    toString as a -> String
                case ReifiedChar =>
                    import static java.util.Arrays.toString(Array[Char, Static]): String;
                    toString as a -> String
                case ReifiedFloat32 =>
                    import static java.util.Arrays.toString(Array[Float32, Static]): String;
                    toString as a -> String
                case ReifiedFloat64 =>
                    import static java.util.Arrays.toString(Array[Float64, Static]): String;
                    toString as a -> String
                case ReifiedInt8 =>
                    import static java.util.Arrays.toString(Array[Int8, Static]): String;
                    toString as a -> String
                case ReifiedInt16 =>
                    import static java.util.Arrays.toString(Array[Int16, Static]): String;
                    toString as a -> String
                case ReifiedInt32 =>
                    import static java.util.Arrays.toString(Array[Int32, Static]): String;
                    toString as a -> String
                case ReifiedInt64 =>
                    import static java.util.Arrays.toString(Array[Int64, Static]): String;
                    toString as a -> String
                case ReifiedBigInt =>
                    import static java.util.Arrays.toString(Array[##java.lang.Object, Static]): String;
                    toString as a -> String
                case ReifiedString =>
                    import static java.util.Arrays.toString(Array[##java.lang.Object, Static]): String;
                    toString as a -> String
                case ReifiedArray(_) =>
                    import static java.util.Arrays.deepToString(Array[##java.lang.Object, Static]): String;
                    deepToString as a -> String
            }
    };
    magic(x)
}

///
/// Crashes the current process with the given message `m`.
///
pub def bug!(m: String): a = {
    import static get java.lang.System.err: ##java.io.PrintStream \ {} as getErr;
    import java.io.PrintStream.println(String): Unit \ IO;
    import java.lang.String.repeat(Int32): ##java.lang.String \ {};
    let prt = println(getErr());
    prt(repeat("*", 80));
    prt("**") ;
    prt("**  BUG: ${m}") ;
    prt("**") ;
    prt(repeat("*", 80));
    prt("");
    ?panic
} as \ {}

///
/// Asserts that this expression is unreachable. Crashes if not.
///
pub def unreachable!(): a = bug!("Reached unreachable expression.")

///
/// Casts a pure function to `ef`.
///
/// `upcast` is safe to use anywhere.
///
pub def upcast(f: a -> b): a -> b \ ef =
    x -> f(x) as \ ef

///
/// Meta-programming facility to express the time complexity of a function `f`.
///
@Internal
pub def time(_: a -> Int32): Int32 = 1

///
/// Meta-programming facility to express the space complexity of a function `f`.
///
@Internal
pub def space(_: a -> Int32): Int32 = 1

///
/// Prints the argument unless it is ().
///
@Internal
pub def printUnlessUnit(x: a): Unit \ IO with ToString[a] = {
    match (reifyType a) {
        case ReifiedUnit => ()
        case _ => println(x)
    }
}

///
/// Represents a boxed value.
///
pub enum Boxed {
    case BoxedBool(Bool)
    case BoxedChar(Char)
    case BoxedInt8(Int8)
    case BoxedInt16(Int16)
    case BoxedInt32(Int32)
    case BoxedInt64(Int64)
    case BoxedFloat32(Float32)
    case BoxedFloat64(Float64)
    case BoxedValue(##java.lang.Object)
    case BoxedObject(
        ##java.lang.Object,
        (##java.lang.Object, ##java.lang.Object) -> Comparison,
        ##java.lang.Object -> String)
}

instance Eq[Boxed] {
    pub def eq(b1: Boxed, b2: Boxed): Bool = match (b1, b2) {
        case (BoxedBool(v1),            BoxedBool(v2))          => v1 == v2
        case (BoxedChar(v1),            BoxedChar(v2))          => v1 == v2
        case (BoxedInt8(v1),            BoxedInt8(v2))          => v1 == v2
        case (BoxedInt16(v1),           BoxedInt16(v2))         => v1 == v2
        case (BoxedInt32(v1),           BoxedInt32(v2))         => v1 == v2
        case (BoxedInt64(v1),           BoxedInt64(v2))         => v1 == v2
        case (BoxedFloat32(v1),         BoxedFloat32(v2))       => v1 == v2
        case (BoxedFloat64(v1),         BoxedFloat64(v2))       => v1 == v2
        case (_,                        BoxedValue(_))          => bug!("Eq is not supported for BoxedValue")
        case (BoxedValue(_),            _)                      => bug!("Eq is not supported for BoxedValue")
        case (BoxedObject(v1, cmp1, _), BoxedObject(v2, _, _))  => cmp1(v1, v2) == EqualTo
        case _                                                  => bug!("Mismatched types.")
    }
}

instance Order[Boxed] {
    pub def compare(b1: Boxed, b2: Boxed): Comparison = match (b1, b2) {
        case (BoxedBool(v1),            BoxedBool(v2))          => v1 <=> v2
        case (BoxedChar(v1),            BoxedChar(v2))          => v1 <=> v2
        case (BoxedInt8(v1),            BoxedInt8(v2))          => v1 <=> v2
        case (BoxedInt16(v1),           BoxedInt16(v2))         => v1 <=> v2
        case (BoxedInt32(v1),           BoxedInt32(v2))         => v1 <=> v2
        case (BoxedInt64(v1),           BoxedInt64(v2))         => v1 <=> v2
        case (BoxedFloat32(v1),         BoxedFloat32(v2))       => v1 <=> v2
        case (BoxedFloat64(v1),         BoxedFloat64(v2))       => v1 <=> v2
        case (_,                        BoxedValue(_))          => bug!("Order is not supported for BoxedValue")
        case (BoxedValue(_),            _)                      => bug!("Order is not supported for BoxedValue")
        case (BoxedObject(v1, cmp1, _), BoxedObject(v2, _, _))  => cmp1(v1, v2)
        case _                                                  => bug!("Mismatched types.")
    }
}

instance ToString[Boxed] {
    pub def toString(b: Boxed): String = match b {
        case BoxedBool(v)           => ToString.toString(v)
        case BoxedChar(v)           => ToString.toString(v)
        case BoxedInt8(v)           => ToString.toString(v)
        case BoxedInt16(v)          => ToString.toString(v)
        case BoxedInt32(v)          => ToString.toString(v)
        case BoxedInt64(v)          => ToString.toString(v)
        case BoxedFloat32(v)        => ToString.toString(v)
        case BoxedFloat64(v)        => ToString.toString(v)
        case BoxedValue(_)          => bug!("ToString is not supported for BoxedValue")
        case BoxedObject(v, _, to)  => to(v)
    }
}

///
/// A type class for types that can be boxed.
///
pub class Boxable[a] with Order[a], ToString[a] {

    ///
    /// Boxes the given `x`.
    ///
    pub def box(x: a): Boxed = {
        let value = x as ##java.lang.Object;
        let compare = (o1, o2) -> Order.compare(o1 as a, o2 as a);
        let toString = o -> ToString.toString(o as a);
        BoxedObject(value, compare, toString)
    }

    ///
    /// Unboxes the given `x`.
    ///
    pub def unbox(x: Boxed): a = match x {
        case BoxedObject(v, _, _) => v as a
        case _                    => ?bug
    }

}

namespace Boxable {

    ///
    /// Lifts the given function `f` to operate on boxed values.
    ///
    /// Note: Accepts a curried function, but returns a non-curried function.
    ///
    @Internal
    pub def lift1(f: t1 -> t): Boxed -> Boxed with Boxable[t1], Boxable[t] =
        (b1: Boxed) -> box(f(unbox(b1)))

    ///
    /// Lifts the given function `f` to operate on boxed values.
    ///
    /// Note: Accepts a curried function, but returns a non-curried function.
    ///
    @Internal
    pub def lift2(f: t1 -> t2 -> t): Boxed -> Boxed -> Boxed with Boxable[t1], Boxable[t2], Boxable[t] =
        (b1: Boxed) -> (b2: Boxed) -> box(f(unbox(b1), unbox(b2)))

    ///
    /// Lifts the given function `f` to operate on boxed values.
    ///
    /// Note: Accepts a curried function, but returns a non-curried function.
    ///
    @Internal
    pub def lift3(f: t1 -> t2 -> t3 -> t): Boxed -> Boxed -> Boxed -> Boxed with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t] =
        (b1: Boxed) -> (b2: Boxed) -> (b3: Boxed) -> box(f(unbox(b1), unbox(b2), unbox(b3)))

    ///
    /// Lifts the given function `f` to operate on boxed values.
    ///
    /// Note: Accepts a curried function, but returns a non-curried function.
    ///
    @Internal
    pub def lift4(f: t1 -> t2 -> t3 -> t4 -> t): Boxed -> Boxed -> Boxed -> Boxed -> Boxed with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t] =
        (b1: Boxed) -> (b2: Boxed) -> (b3: Boxed) -> (b4: Boxed) -> box(f(unbox(b1), unbox(b2), unbox(b3), unbox(b4)))

    ///
    /// Lifts the given function `f` to operate on boxed values.
    ///
    /// Note: Accepts a curried function, but returns a non-curried function.
    ///
    @Internal
    pub def lift5(f: t1 -> t2 -> t3 -> t4 -> t5 -> t): Boxed -> Boxed -> Boxed -> Boxed -> Boxed -> Boxed with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t] =
        (b1: Boxed) -> (b2: Boxed) -> (b3: Boxed) -> (b4: Boxed) -> (b5: Boxed) -> box(f(unbox(b1), unbox(b2), unbox(b3), unbox(b4), unbox(b5)))

    ///
    /// Lifts the given Boolean-valued function `f` to operate on boxed values.
    ///
    @Internal
    pub def lift1b(f: t1 -> Bool): Boxed -> Bool with Boxable[t1] =
        (b1: Boxed) -> f(unbox(b1))

    ///
    /// Lifts the given Boolean-valued function `f` to operate on boxed values.
    ///
    /// Note: Accepts a curried function, but returns a non-curried function.
    ///
    @Internal
    pub def lift2b(f: t1 -> t2 -> Bool): Boxed -> Boxed -> Bool with Boxable[t1], Boxable[t2] =
        (b1: Boxed) -> (b2: Boxed) -> f(unbox(b1), unbox(b2))

    ///
    /// Lifts the given Boolean-valued function `f` to operate on boxed values.
    ///
    /// Note: Accepts a curried function, but returns a non-curried function.
    ///
    @Internal
    pub def lift3b(f: t1 -> t2 -> t3 -> Bool): Boxed -> Boxed -> Boxed -> Bool with Boxable[t1], Boxable[t2], Boxable[t3] =
        (b1: Boxed) -> (b2: Boxed) -> (b3: Boxed) -> f(unbox(b1), unbox(b2), unbox(b3))

    ///
    /// Lifts the given Boolean-valued function `f` to operate on boxed values.
    ///
    /// Note: Accepts a curried function, but returns a non-curried function.
    ///
    @Internal
    pub def lift4b(f: t1 -> t2 -> t3 -> t4 -> Bool): Boxed -> Boxed -> Boxed -> Boxed -> Bool with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4] =
        (b1: Boxed) -> (b2: Boxed) -> (b3: Boxed) -> (b4: Boxed) -> f(unbox(b1), unbox(b2), unbox(b3), unbox(b4))

    ///
    /// Lifts the given Boolean-valued function `f` to operate on boxed values.
    ///
    /// Note: Accepts a curried function, but returns a non-curried function.
    ///
    @Internal
    pub def lift5b(f: t1 -> t2 -> t3 -> t4 -> t5 -> Bool): Boxed -> Boxed -> Boxed -> Boxed -> Boxed -> Bool with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5] =
        (b1: Boxed) -> (b2: Boxed) -> (b3: Boxed) -> (b4: Boxed) -> (b5: Boxed) -> f(unbox(b1), unbox(b2), unbox(b3), unbox(b4), unbox(b5))

}

instance Boxable[Unit] {
    pub override def box(x: Unit): Boxed = {
        let value = x as ##java.lang.Object;
        let compare = (o1, o2) -> Order.compare(o1 as Unit, o2 as Unit);
        let toString = o -> ToString.toString(o as Unit);
        BoxedObject(value, compare, toString)
    }
    pub override def unbox(_: Boxed): Unit = ()
}

instance Boxable[Bool] {
    pub override def box(x: Bool): Boxed = BoxedBool(x)
    pub override def unbox(x: Boxed): Bool = match x {
        case BoxedBool(v) => v
        case _            => ?bug
    }
}

instance Boxable[Char] {
    pub override def box(x: Char): Boxed = BoxedChar(x)
    pub override def unbox(x: Boxed): Char = match x {
        case BoxedChar(v) => v
        case _            => ?bug
    }
}

instance Boxable[Int8] {
    pub override def box(x: Int8): Boxed = BoxedInt8(x)
    pub override def unbox(x: Boxed): Int8 = match x {
        case BoxedInt8(v) => v
        case _            => ?bug
    }
}

instance Boxable[Int16] {
    pub override def box(x: Int16): Boxed = BoxedInt16(x)
    pub override def unbox(x: Boxed): Int16 = match x {
        case BoxedInt16(v) => v
        case _             => ?bug
    }
}

instance Boxable[Int32] {
    pub override def box(x: Int32): Boxed = BoxedInt32(x)
    pub override def unbox(x: Boxed): Int32 = match x {
        case BoxedInt32(v) => v
        case _             => ?bug
    }
}

instance Boxable[Int64] {
    pub override def box(x: Int64): Boxed = BoxedInt64(x)
    pub override def unbox(x: Boxed): Int64 = match x {
        case BoxedInt64(v) => v
        case _             => ?bug
    }
}

instance Boxable[Float32] {
    pub override def box(x: Float32): Boxed = BoxedFloat32(x)
    pub override def unbox(x: Boxed): Float32 = match x {
        case BoxedFloat32(v) => v
        case _               => ?bug
    }
}

instance Boxable[Float64] {
    pub override def box(x: Float64): Boxed = BoxedFloat64(x)
    pub override def unbox(x: Boxed): Float64 = match x {
        case BoxedFloat64(v) => v
        case _               => ?bug
    }
}

instance Boxable[BigInt]

instance Boxable[String]

instance Boxable[(a, b)] with Boxable[a], Boxable[b]

instance Boxable[(a, b, c)] with Boxable[a], Boxable[b], Boxable[c]

instance Boxable[(a, b, c, d)] with Boxable[a], Boxable[b], Boxable[c], Boxable[d]

instance Boxable[(a, b, c, d, e)] with Boxable[a], Boxable[b], Boxable[c], Boxable[d], Boxable[e]

instance Boxable[(a, b, c, d, e, f)] with Boxable[a], Boxable[b], Boxable[c], Boxable[d], Boxable[e], Boxable[f]

instance Boxable[(a, b, c, d, e, f, g)] with Boxable[a], Boxable[b], Boxable[c], Boxable[d], Boxable[e], Boxable[f], Boxable[g]

instance Boxable[(a, b, c, d, e, f, g, h)] with Boxable[a], Boxable[b], Boxable[c], Boxable[d], Boxable[e], Boxable[f], Boxable[g], Boxable[h]

instance Boxable[(a, b, c, d, e, f, g, h, i)] with Boxable[a], Boxable[b], Boxable[c], Boxable[d], Boxable[e], Boxable[f], Boxable[g], Boxable[h], Boxable[i]

instance Boxable[(a, b, c, d, e, f, g, h, i, j)] with Boxable[a], Boxable[b], Boxable[c], Boxable[d], Boxable[e], Boxable[f], Boxable[g], Boxable[h], Boxable[i], Boxable[j]

instance Boxable[(a, b, c, d, e, f, g, h, i, j, k)] with Boxable[a], Boxable[b], Boxable[c], Boxable[d], Boxable[e], Boxable[f], Boxable[g], Boxable[h], Boxable[i], Boxable[j], Boxable[k]

instance Boxable[(a, b, c, d, e, f, g, h, i, j, k, l)] with Boxable[a], Boxable[b], Boxable[c], Boxable[d], Boxable[e], Boxable[f], Boxable[g], Boxable[h], Boxable[i], Boxable[j], Boxable[k], Boxable[l]

instance Boxable[(a, b, c, d, e, f, g, h, i, j, k, l, m)] with Boxable[a], Boxable[b], Boxable[c], Boxable[d], Boxable[e], Boxable[f], Boxable[g], Boxable[h], Boxable[i], Boxable[j], Boxable[k], Boxable[l], Boxable[m]

instance Boxable[(a, b, c, d, e, f, g, h, i, j, k, l, m, n)] with Boxable[a], Boxable[b], Boxable[c], Boxable[d], Boxable[e], Boxable[f], Boxable[g], Boxable[h], Boxable[i], Boxable[j], Boxable[k], Boxable[l], Boxable[m], Boxable[n]

instance Boxable[(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)] with Boxable[a], Boxable[b], Boxable[c], Boxable[d], Boxable[e], Boxable[f], Boxable[g], Boxable[h], Boxable[i], Boxable[j], Boxable[k], Boxable[l], Boxable[m], Boxable[n], Boxable[o]

///
/// A type class for types that can be hashed.
///
pub class Hash[a] {
    ///
    /// Returns a hash value for the given x.
    ///
    pub def hash(x: a): Int32

    // TODO: Add a law that hash is consistent with equals.
}

instance Hash[Unit] {
    pub def hash(_x: Unit): Int32 = 0
}

instance Hash[Bool] {
    pub def hash(x: Bool): Int32 = match x {
        case true => 1231
        case false => 1237
    }
}

instance Hash[Char] {
    pub def hash(x: Char): Int32 =
        import static java.lang.Character.hashCode(Char): Int32 \ {};
        hashCode(x)
}

instance Hash[Float32] {
    pub def hash(x: Float32): Int32 =
        import static java.lang.Float.hashCode(Float32): Int32 \ {};
        hashCode(x)
}

instance Hash[Float64] {
    pub def hash(x: Float64): Int32 =
        import static java.lang.Double.hashCode(Float64): Int32 \ {};
        hashCode(x)
}

instance Hash[Int8] {
    pub def hash(x: Int8): Int32 =
        import static java.lang.Byte.hashCode(Int8): Int32 \ {};
        hashCode(x)
}

instance Hash[Int16] {
    pub def hash(x: Int16): Int32 =
        import static java.lang.Short.hashCode(Int16): Int32 \ {};
        hashCode(x)
}

instance Hash[Int32] {
    pub def hash(x: Int32): Int32 =
        import static java.lang.Integer.hashCode(Int32): Int32 \ {};
        hashCode(x)
}

instance Hash[Int64] {
    pub def hash(x: Int64): Int32 =
        import static java.lang.Long.hashCode(Int64): Int32 \ {};
        hashCode(x)
}

instance Hash[String] {
    pub def hash(x: String): Int32 =
        import java.lang.String.hashCode(): Int32 \ {};
        hashCode(x)
}

instance Hash[BigInt] {
    pub def hash(x: BigInt): Int32 =
        import java.math.BigInteger.hashCode(): Int32 \ {};
        hashCode(x)
}

instance Hash[(a1, a2)] with Hash[a1], Hash[a2] {
    pub def hash(t: (a1, a2)): Int32 = match t {
        case (x1, x2) => Hash.hash(x1) `Hash.combine` Hash.hash(x2)
    }
}

instance Hash[(a1, a2, a3)] with Hash[a1], Hash[a2], Hash[a3] {
    pub def hash(t: (a1, a2, a3)): Int32 = match t {
        case (x1, x2, x3) => Hash.hash(x1) `Hash.combine` Hash.hash(x2) `Hash.combine` Hash.hash(x3)
    }
}

instance Hash[(a1, a2, a3, a4)] with Hash[a1], Hash[a2], Hash[a3], Hash[a4] {
    pub def hash(t: (a1, a2, a3, a4)): Int32 = match t {
        case (x1, x2, x3, x4) =>
            Hash.hash(x1) `Hash.combine` Hash.hash(x2) `Hash.combine` Hash.hash(x3) `Hash.combine` Hash.hash(x4)
    }
}

instance Hash[(a1, a2, a3, a4, a5)] with Hash[a1], Hash[a2], Hash[a3], Hash[a4], Hash[a5] {
    pub def hash(t: (a1, a2, a3, a4, a5)): Int32 = match t {
        case (x1, x2, x3, x4, x5) =>
            Hash.hash(x1) `Hash.combine` Hash.hash(x2) `Hash.combine` Hash.hash(x3) `Hash.combine` Hash.hash(x4)
                `Hash.combine` Hash.hash(x5)
    }
}

instance Hash[(a1, a2, a3, a4, a5, a6)] with Hash[a1], Hash[a2], Hash[a3], Hash[a4], Hash[a5], Hash[a6] {
    pub def hash(t: (a1, a2, a3, a4, a5, a6)): Int32 = match t {
        case (x1, x2, x3, x4, x5, x6) =>
            Hash.hash(x1) `Hash.combine` Hash.hash(x2) `Hash.combine` Hash.hash(x3) `Hash.combine` Hash.hash(x4)
                `Hash.combine` Hash.hash(x5) `Hash.combine` Hash.hash(x6)
    }
}

instance Hash[(a1, a2, a3, a4, a5, a6, a7)] with Hash[a1], Hash[a2], Hash[a3], Hash[a4], Hash[a5], Hash[a6], Hash[a7] {
    pub def hash(t: (a1, a2, a3, a4, a5, a6, a7)): Int32 = match t {
        case (x1, x2, x3, x4, x5, x6, x7) =>
            Hash.hash(x1) `Hash.combine` Hash.hash(x2) `Hash.combine` Hash.hash(x3) `Hash.combine` Hash.hash(x4)
                `Hash.combine` Hash.hash(x5) `Hash.combine` Hash.hash(x6) `Hash.combine` Hash.hash(x7)
    }
}

instance Hash[(a1, a2, a3, a4, a5, a6, a7, a8)] with Hash[a1], Hash[a2], Hash[a3], Hash[a4], Hash[a5], Hash[a6], Hash[a7], Hash[a8] {
    pub def hash(t: (a1, a2, a3, a4, a5, a6, a7, a8)): Int32 = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8) =>
            Hash.hash(x1) `Hash.combine` Hash.hash(x2) `Hash.combine` Hash.hash(x3) `Hash.combine` Hash.hash(x4)
                `Hash.combine` Hash.hash(x5) `Hash.combine` Hash.hash(x6) `Hash.combine` Hash.hash(x7) `Hash.combine` Hash.hash(x8)
    }
}

instance Hash[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with Hash[a1], Hash[a2], Hash[a3], Hash[a4], Hash[a5], Hash[a6], Hash[a7], Hash[a8], Hash[a9] {
    pub def hash(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9)): Int32 = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8, x9) =>
            Hash.hash(x1) `Hash.combine` Hash.hash(x2) `Hash.combine` Hash.hash(x3) `Hash.combine` Hash.hash(x4)
                `Hash.combine` Hash.hash(x5) `Hash.combine` Hash.hash(x6) `Hash.combine` Hash.hash(x7) `Hash.combine` Hash.hash(x8)
                `Hash.combine` Hash.hash(x9)
    }
}

instance Hash[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with Hash[a1], Hash[a2], Hash[a3], Hash[a4], Hash[a5], Hash[a6], Hash[a7], Hash[a8], Hash[a9], Hash[a10] {
    pub def hash(t: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)): Int32 = match t {
        case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10) =>
            Hash.hash(x1) `Hash.combine` Hash.hash(x2) `Hash.combine` Hash.hash(x3) `Hash.combine` Hash.hash(x4)
                `Hash.combine` Hash.hash(x5) `Hash.combine` Hash.hash(x6) `Hash.combine` Hash.hash(x7) `Hash.combine` Hash.hash(x8)
                `Hash.combine` Hash.hash(x9) `Hash.combine` Hash.hash(x10)
    }
}

namespace Hash {

    ///
    /// Combines the two given hash values.
    ///
    pub def combine(h1: Int32, h2: Int32): Int32 = {
        h1 * magic() + h2
    }

    ///
    /// Returns a number used for combining hash values.
    ///
    pub def magic(): Int32 = -1640531535
}

///
/// A type class for types that can be mapped over.
///
pub lawful class Functor[m : Type -> Type] {
    ///
    /// Applies the function `f` to `x` preserving its structure.
    ///
    pub def map(f: a -> b \ ef, x: m[a]): m[b] \ ef

    ///
    /// Mapping the identity function over a functor preserves the functor.
    ///
    law identity: forall(x: m[a]) with Eq[m[a]] . Functor.map(identity, x) == x

    ///
    /// Composing two functions and mapping the resulting function over a functor is the same as
    /// mapping the functions one after the other over the functor.
    ///
    law composition: forall(f: b -> c, g: a -> b, v: m[a]) with Eq[m[c]] . Functor.map(g >> f, v) == Functor.map(f, Functor.map(g, v))
}

namespace Functor {
    ///
    /// Lifts the function `f` into the functor.
    ///
    pub def lift[a: Type, b: Type, ef: Bool, m: Type -> Type](f: a -> b \ ef): m[a] -> m[b] \ ef with Functor[m] = Functor.map(f)

    ///
    /// Replaces the value `a` in `s` by the given value `x` preserving the structure of `s`.
    ///
    pub def as[a: Type, b: Type, m: Type -> Type](s: m[a], x: b): m[b] with Functor[m] = Functor.map(_ -> x, s)

    ///
    /// Replaces the value `a` in `s` with `Unit` preserving the structure of `s`.
    ///
    /// This function is typically used to discard the return value of computing `s`.
    ///
    pub def ignore(s: m[a]): m[Unit] with Functor[m] = s `Functor.as` ()

    ///
    /// `<$>` is an operator alias for `map`.
    ///
    pub def <$>(f: a -> b \ ef, x: m[a]): m[b] \ ef with Functor[m]= Functor.map(f, x)

    ///
    /// `<$$>` is the operator `<$>` with its arguments flipped.
    ///
    pub def <$$>(x: m[a], f: a -> b \ ef): m[b] \ ef with Functor[m]= Functor.map(f, x)

    ///
    /// `<$` is an operator alias for `as`.
    ///
    pub def <$(x: a, ma: f[b]): f[a] with Functor[f] = Functor.as(ma, x)

    ///
    /// `<$` is the operator `$>` with its arguments flipped.
    ///
    pub def $>(ma: f[a], x: b): f[b] with Functor[f] = Functor.as(ma, x)

}

///
/// A type class for functors that support application, i.e. allow to:
///
/// - Make an applicative value out of a normal value (embed it into a default context), e.g. embed `5` into `Some(5)`.
/// - Apply a function-type applicative to a matching argument-type applicative, resulting in an applicative of
/// the function's result type.
///
/// The meaning of the application realized by the `ap` function is defined by the respective instance.
/// Conceptually this can be understood as applying functions "contained" in the first applicative to arguments
/// in the second applicative, where the possible quantity of functions/arguments depends on the type `m`.
/// For example, an `Option[a -> b]` can be `None`, or contain a function of type `a -> b`, and only in the
/// latter case a function is applied. A `List[a -> b]` is an applicative that contains a list of functions,
/// which are all to be applied to all arguments contained in the arguments list.
///
/// A minimal implementation must define `point` and at least one of `ap` and `liftA2` (if `liftA2` is implemented,
/// `ap` can be defined based on `liftA2` as shown below). If both `ap` and `liftA2` are defined,
/// they must be equivalent to their default definitions:
///     `ap(f: m[a -> b & e], x: m[a]): m[b] \ ef = liftA2(identity, f, x)`
///     `liftA2(f: a -> b -> c & e, x: m[a], y: m[b]): m[c] \ ef = ap(Functor.map(f, x), y)`
///
pub lawful class Applicative[m : Type -> Type] with Functor[m] {
    ///
    /// Puts `x` into a default context.
    ///
    pub def point(x: a): m[a]

    ///
    /// Apply the function-type applicative `f` to the argument-type applicative `x`.
    ///
    pub def ap(f: m[a -> b \ ef], x: m[a]): m[b] \ ef

    ///
    /// Lift a binary function to work on `Applicative`s.
    /// Instances can define more efficient implementations than the default implementation
    /// (which is `Applicative.ap(Functor.map(f, x1), x2)`).
    ///
    pub def liftA2(f: t1 -> t2 -> r \ ef, x1: m[t1], x2: m[t2]): m[r] \ ef = Applicative.ap(Functor.map(f, x1), x2)

    ///
    /// Lift a ternary function to work on `Applicative`s.
    /// Instances can define more efficient implementations than the default implementation
    /// (which is `Applicative.ap(Applicative.liftA2(f, x1, x2), x3)`).
    ///
    pub def liftA3(f: t1 -> t2 -> t3 -> r \ ef, x1: m[t1], x2: m[t2], x3: m[t3]): m[r] \ ef = Applicative.ap(Applicative.liftA2(f, x1, x2), x3)

    ///
    /// Lift a 4-ary function to work on `Applicative`s.
    /// Instances can define more efficient implementations than the default implementation
    /// (which is `Applicative.ap(Applicative.liftA3(f, x1, x2, x3), x4)`).
    ///
    pub def liftA4(f: t1 -> t2 -> t3 -> t4 -> r \ ef, x1: m[t1], x2: m[t2], x3: m[t3], x4: m[t4]): m[r] \ ef = Applicative.ap(Applicative.liftA3(f, x1, x2, x3), x4)

    ///
    /// Lift a 5-ary function to work on `Applicative`s.
    /// Instances can define more efficient implementations than the default implementation
    /// (which is `Applicative.ap(Applicative.liftA3(f, x1, x2, x3), x4)`).
    ///
    pub def liftA5(f: t1 -> t2 -> t3 -> t4 -> t5 -> r \ ef, x1: m[t1], x2: m[t2], x3: m[t3], x4: m[t4], x5: m[t5]): m[r] \ ef = Applicative.ap(Applicative.liftA4(f, x1, x2, x3, x4), x5)

    ///
    /// Applying the identity function wrapped into an applicative preserves every applicative value.
    ///
    law identity: forall(x: m[a]) with Eq[m[a]] . Applicative.ap(Applicative.point(identity), x) == x

    ///
    /// Applicatively composing two functions and then applicatively applying the resulting function is the same
    /// as applicatively applying the functions one by one.
    ///
    law composition: forall(f: m[b -> c], g: m[a -> b], v: m[a]) with Eq[m[c]] . Applicative.ap(Applicative.ap(Applicative.ap(Applicative.point((f, g) -> g >> f), f), g), v) == Applicative.ap(f, Applicative.ap(g, v))

    ///
    /// `point` is a homomorphism from normal to applicative values regarding application (normal vs. applicative).
    ///
    // TODO error in type checker does not allow to compile this, see #2099
    // law homomorphism: forall(f: a -> b, x: a) with Eq[m[b]] . Applicative.ap(Applicative.point(f), Applicative.point(x)) == Applicative.point(f(x))

    ///
    /// Directly applicatively applying a function `f` to an argument `x` put into a default context is the same as putting the function that takes a function and applies it to `x` into a default context and applicatively applying it to `f`.
    ///
    law interchange: forall(f: m[a -> b], x: a) with Eq[m[b]] . Applicative.ap(f, Applicative.point(x)) == Applicative.ap(Applicative.point(f -> f(x)), f)

    ///
    /// `liftA2` is equivalent to its default implementation.
    ///
    law liftA2Correspondence: forall(f: t1 -> t2 -> r, x1: m[t1], x2: m[t2]) with Eq[m[r]] . Applicative.liftA2(f, x1, x2) == Applicative.ap(Functor.map(f, x1), x2)

    ///
    /// `liftA3` is equivalent to its default implementation.
    ///
    law liftA3Correspondence: forall(f: t1 -> t2 -> t3 -> r, x1: m[t1], x2: m[t2], x3: m[t3]) with Eq[m[r]] . Applicative.liftA3(f, x1, x2, x3) == Applicative.ap(Applicative.liftA2(f, x1, x2), x3)

    ///
    /// `liftA4` is equivalent to its default implementation.
    ///
    law liftA4Correspondence: forall(f: t1 -> t2 -> t3 -> t4 -> r, x1: m[t1], x2: m[t2], x3: m[t3], x4: m[t4]) with Eq[m[r]] . Applicative.liftA4(f, x1, x2, x3, x4) == Applicative.ap(Applicative.liftA3(f, x1, x2, x3), x4)

    ///
    /// `liftA5` is equivalent to its default implementation.
    ///
    law liftA5Correspondence: forall(f: t1 -> t2 -> t3 -> t4 -> t5 -> r, x1: m[t1], x2: m[t2], x3: m[t3], x4: m[t4], x5: m[t5]) with Eq[m[r]] . Applicative.liftA5(f, x1, x2, x3, x4, x5) == Applicative.ap(Applicative.liftA4(f, x1, x2, x3, x4), x5)

    ///
    /// Mapping a function over an applicative (with `Functor.map`) is the same as putting the function
    /// into a default context and applying it to the applicative.
    ///
    law mapCorrespondence: forall(f: a -> b, x: m[a]) with Eq[m[b]] . Functor.map(f, x) == Applicative.ap(Applicative.point(f), x)
}

namespace Applicative {

    ///
    /// Chain two applicative actions, returns the product of their results.
    ///
    pub def product(fa: m[a], fb: m[b]): m[(a, b)] with Applicative[m] =
        Applicative.liftA2((a, b) -> (a, b), fa, fb)

    ///
    /// Chain two applicative actions, return only the result of the first.
    ///
    pub def productLeft(fa: m[a], fb: m[b]): m[a] with Applicative[m] =
        Applicative.liftA2((a, _) -> a, fa, fb)

    ///
    /// Chain two applicative actions, return only the result of the second.
    ///
    pub def productRight(fa: m[a], fb: m[b]): m[b] with Applicative[m] =
        Applicative.liftA2((_, b) -> b, fa, fb)

    ///
    /// Chain three applicative actions, return the 3-tuple of their results.
    ///
    pub def product3(x1: m[t1], x2: m[t2], x3: m[t3]): m[(t1, t2, t3)] with Applicative[m] =
        Applicative.liftA3((a, b, c) -> (a, b, c), x1, x2, x3)

    ///
    /// Chain four applicative actions, return the 4-tuple of their results.
    ///
    pub def product4(x1: m[t1], x2: m[t2], x3: m[t3], x4: m[t4]): m[(t1, t2, t3, t4)] with Applicative[m] =
        Applicative.liftA4((a, b, c, d) -> (a, b, c, d), x1, x2, x3, x4)

    ///
    /// Chain five applicative actions, return the 5-tuple of their results.
    ///
    pub def product5(x1: m[t1], x2: m[t2], x3: m[t3], x4: m[t4], x5: m[t5]): m[(t1, t2, t3, t4, t5)] with Applicative[m] =
        Applicative.liftA5((a, b, c, d, e) -> (a, b, c, d, e), x1, x2, x3, x4, x5)

    ///
    /// `<*>` is an operator alias for `ap`.
    ///
    pub def <*>(mf: m[a -> b \ ef], ma: m[a]): m[b] \ ef with Applicative[m] = Applicative.ap(mf, ma)

    ///
    /// `<**>` is a variant of the operator `<*>` with its arguments flipped.
    ///
    /// The order of evaluation is `ma` then `mf`.
    ///
    pub def <**>(ma: m[a], mf: m[a -> b \ ef]): m[b] \ ef with Applicative[m] = Applicative.liftA2((a, f) -> f(a), ma, mf)

    ///
    /// `<*` is an operator alias for `productLeft`.
    ///
    pub def <*(ma: m[a], mb: m[b]): m[a] with Applicative[m] = productLeft(ma, mb)

    ///
    /// `*>` is an operator alias for `productRight`.
    ///
    pub def *>(ma: m[a], mb: m[b]): m[b] with Applicative[m] = productRight(ma, mb)

}

///
/// A type class for applicatives that support monadic bind (`flatMap`), i.e., allow to apply a function
/// that takes a normal value and produces a monadic value to a monadic value. That is, the bind mechanism
/// supports extraction of monadic values, or, viewed differently, allows to combine (flatten) nested
/// monadic values (`flapMap` can be understood as a `Functor.map` followed by a `flatten`).
///
pub lawful class Monad[m : Type -> Type] with Applicative[m] {

    ///
    /// Apply function `f` to the monadic value `x`, resulting in a combined monadic value.
    ///
    pub def flatMap(f: a -> m[b] \ ef, x: m[a]) : m[b] \ ef

    ///
    /// Applying `flatMap` to the `point` function is the identity function.
    ///
    law leftIdentity: forall(f: a -> m[b], x: m[a]) with Eq[m[a]] . Monad.flatMap(Applicative.point, x) == x

    ///
    /// Applying `flatMap` to a function and to a non-monadic argument after applying `point` to it is the same
    /// as applying the function directly to that argument.
    ///
    law rightIdentity: forall(f: a -> m[b], x: a) with Eq[m[b]] . Monad.flatMap(f, Applicative.point(x)) == f(x)

    ///
    /// Subsequent `flatMap` calls (nested in the monadic argument) can be moved to the outside by
    /// providing a lambda as first argument which then calls `flatMap`.
    ///
    law associativity: forall(x: m[a], f: a -> m[b], g: b -> m[c]) with Eq[m[c]] . Monad.flatMap(g, Monad.flatMap(f, x)) == Monad.flatMap(y -> Monad.flatMap(g, f(y)), x)

    ///
    /// Applicatively applying a monadic function to a monadic argument is the same as using `flatMap`
    /// to extract function and argument, applying them normally and then applying `point` to the result.
    ///
    law apCorrespondence: forall(f: m[a -> b], x: m[a]) with Eq[m[b]] . Applicative.ap(f, x) == Monad.flatMap(g -> Monad.flatMap(y -> Applicative.point(g(y)), x), f)
}


namespace Monad {

    ///
    /// The monadic `join` operator.
    /// Flatten `x` - a monadic action nested in an outer monadic layer - to a single layer.
    ///
    /// E.g. for the Option monad: `flatten(Some(Some(1)))` becomes `Some(1)`.
    ///
    pub def flatten(x: m[m[a]]): m[a] with Monad[m] = flatMap(identity, x)

    ///
    /// The left-to-right Kleisli composition operator for monads.
    ///
    /// Map `x` with the monadic function `f1` and then map its result with the function `f2`.
    ///
    pub def kleisliLeft(f1: a -> m[b] \ ef1, f2: b -> m[c] \ ef2, x: a): m[c] \ { ef1, ef2 } with Monad[m] =
        flatMap(x1 -> f2(x1), f1(x))

    ///
    /// The right-to-left Kleisli composition operator for monads.
    ///
    /// Map `x` with the monadic function `f2` and then map its result with the function `f1`.
    ///
    pub def kleisliRight(f1: b -> m[c] \ ef1, f2: a -> m[b] \ ef2, x: a): m[c] \ { ef1, ef2 } with Monad[m] =
        flatMap(x1 -> f1(x1), f2(x))


    ///
    /// `=<<` is an operator alias for `flatMap`.
    ///
    pub def =<<(k: a -> m[b] \ ef, x: m[a]): m[b] \ ef with Monad[m] = flatMap(k, x)

    ///
    /// `>>=` is the operator `=<<` with its arguments flipped.
    ///
    /// `>>=` is the monadic bind operator.
    ///
    pub def >>=(x: m[a], k: a -> m[b] \ ef): m[b] \ ef with Monad[m] = flatMap(k, x)

    ///
    /// `>=>` is an operator alias for `kleisliLeft`.
    ///
    pub def >=>(f1: a -> m[b] \ ef1, f2: b -> m[c] \ ef2): a -> m[c] \ { ef1, ef2 } with Monad[m] = x ->
        kleisliLeft(f1, f2, x)


    ///
    /// `<=<` is an operator alias for `kleisliRight`.
    ///
    pub def <=<(f1: b -> m[c] \ ef1, f2: a -> m[b] \ ef2): a -> m[c] \ { ef1, ef2 } with Monad[m] = x ->
        kleisliRight(f1, f2, x)

}

///
/// A type class for Monads that have a zero element.
///
class MonadZero[m: Type -> Type] with Monad[m] {

    ///
    /// Returns the zero element of the monad.
    ///
    pub def empty(): m[t]

}

///
/// A type class for zipping Monads, typically container monads like `List`.
///
/// A minimal implementation must define `zipWith` and `zipWithA`.
///
class MonadZip[m: Type -> Type] with Monad[m] {

    ///
    /// Returns single monad where the element (or elements) of `ma` and `mb` are combined
    /// with the function `f`.
    ///
    pub def zipWith(f: (a, b) -> c \ ef, ma: m[a], mb: m[b]): m[c] \ ef

    ///
    /// Returns single monad where the element (or elements) of `ma` and `mb` are combined
    /// as pairs.
    ///
    pub def zip(ma: m[a], mb: m[b]): m[(a, b)] = MonadZip.zipWith((x, y) -> (x, y), ma, mb)

    ///
    /// Returns a pair of monads, the first containing the element (or elements) of the left part of `mx`
    /// the second containing the element (or elements) of the right part of `mx`.
    ///
    pub def unzip(mx: m[(a, b)]): (m[a], m[b]) = (Functor.map(fst, mx), Functor.map(snd, mx))

    ///
    /// Generalized version of `zipWith` where `f` zips an applicative functor across the
    /// (monadic) containers `ma` and `mb`.
    ///
    pub def zipWithA(f: (a, b) -> f[c] \ ef, ma: m[a], mb: m[b]): f[m[c]] \ ef with Applicative[f]

}

///
/// A type class for partially ordered types that have a lower bound.
///
pub class LowerBound[a] {
    ///
    /// Returns the smallest value of `a`.
    ///
    pub def minValue(): a
}

instance LowerBound[(a1, a2)] with LowerBound[a1], LowerBound[a2] {
    pub def minValue(): (a1, a2) = (LowerBound.minValue(), LowerBound.minValue())
}

instance LowerBound[(a1, a2, a3)] with LowerBound[a1], LowerBound[a2], LowerBound[a3] {
    pub def minValue(): (a1, a2, a3) = (LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue())
}

instance LowerBound[(a1, a2, a3, a4)] with LowerBound[a1], LowerBound[a2], LowerBound[a3], LowerBound[a4] {
    pub def minValue(): (a1, a2, a3, a4) = (LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue())
}

instance LowerBound[(a1, a2, a3, a4, a5)] with LowerBound[a1], LowerBound[a2], LowerBound[a3], LowerBound[a4], LowerBound[a5] {
    pub def minValue(): (a1, a2, a3, a4, a5) = (LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue())
}

instance LowerBound[(a1, a2, a3, a4, a5, a6)] with LowerBound[a1], LowerBound[a2], LowerBound[a3], LowerBound[a4], LowerBound[a5], LowerBound[a6] {
    pub def minValue(): (a1, a2, a3, a4, a5, a6) = (LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue())
}

instance LowerBound[(a1, a2, a3, a4, a5, a6, a7)] with LowerBound[a1], LowerBound[a2], LowerBound[a3], LowerBound[a4], LowerBound[a5], LowerBound[a6], LowerBound[a7] {
    pub def minValue(): (a1, a2, a3, a4, a5, a6, a7) = (LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue())
}

instance LowerBound[(a1, a2, a3, a4, a5, a6, a7, a8)] with LowerBound[a1], LowerBound[a2], LowerBound[a3], LowerBound[a4], LowerBound[a5], LowerBound[a6], LowerBound[a7], LowerBound[a8] {
    pub def minValue(): (a1, a2, a3, a4, a5, a6, a7, a8) = (LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue())
}

instance LowerBound[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with LowerBound[a1], LowerBound[a2], LowerBound[a3], LowerBound[a4], LowerBound[a5], LowerBound[a6], LowerBound[a7], LowerBound[a8], LowerBound[a9] {
    pub def minValue(): (a1, a2, a3, a4, a5, a6, a7, a8, a9) = (LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue())
}

instance LowerBound[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with LowerBound[a1], LowerBound[a2], LowerBound[a3], LowerBound[a4], LowerBound[a5], LowerBound[a6], LowerBound[a7], LowerBound[a8], LowerBound[a9], LowerBound[a10] {
    pub def minValue(): (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = (LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue())
}

instance LowerBound[Unit] {
    pub def minValue(): Unit = ()
}

///
/// A Partial Order is a function ⊑ which satisfies three properties: reflexivity, anti-symmetry, and transitivity.
///
pub lawful class PartialOrder[a] {

    ///
    /// Returns `true` if `x` is smaller or equal to `y`.
    ///
    pub def lessEqual(x: a, y: a): Bool
    // TODO This should not return a Boolean, but instead perform a three-way comparison.

    ///
    /// Reflexivity: An element `x` is lower or equal to itself.
    ///
    law reflexivity: forall(x: a). PartialOrder.lessEqual(x, x)

    ///
    /// Transitivity: If `x` is lower or equal to `y` and `y` is lower equal to `z` then `x` must be lower or equal to `z`.
    ///
    law transitivity: forall(x: a, y: a, z: a). (PartialOrder.lessEqual(x, y) and PartialOrder.lessEqual(y, z)) ==> PartialOrder.lessEqual(x, z)
}

instance PartialOrder[Int8] {
    pub def lessEqual(x: Int8, y: Int8): Bool = $INT8_LE$(x, y)
}

instance PartialOrder[Int16] {
    pub def lessEqual(x: Int16, y: Int16): Bool = $INT16_LE$(x, y)
}

instance PartialOrder[Int32] {
    pub def lessEqual(x: Int32, y: Int32): Bool = $INT32_LE$(x, y)
}

instance PartialOrder[Int64] {
    pub def lessEqual(x: Int64, y: Int64): Bool = $INT64_LE$(x, y)
}

instance PartialOrder[BigInt] {
    pub def lessEqual(x: BigInt, y: BigInt): Bool = $BIGINT_LE$(x, y)
}

instance PartialOrder[(a1, a2)] with PartialOrder[a1], PartialOrder[a2] {
    pub def lessEqual(x: (a1, a2),
                           y: (a1, a2)): Bool = match (x, y) {
        case ((x1, x2), (y1, y2)) =>
            PartialOrder.lessEqual(x1, y1) and
            PartialOrder.lessEqual(x2, y2)
    }
}

instance PartialOrder[(a1, a2, a3)] with PartialOrder[a1], PartialOrder[a2],
                                         PartialOrder[a3] {
    pub def lessEqual(x: (a1, a2, a3),
                           y: (a1, a2, a3)): Bool = match (x, y) {
        case ((x1, x2, x3), (y1, y2, y3)) =>
            PartialOrder.lessEqual(x1, y1) and
            PartialOrder.lessEqual(x2, y2) and
            PartialOrder.lessEqual(x3, y3)
    }
}

instance PartialOrder[(a1, a2, a3, a4)] with PartialOrder[a1], PartialOrder[a2],
                                             PartialOrder[a3], PartialOrder[a4] {
    pub def lessEqual(x: (a1, a2, a3, a4),
                           y: (a1, a2, a3, a4)): Bool = match (x, y) {
        case ((x1, x2, x3, x4), (y1, y2, y3, y4)) =>
            PartialOrder.lessEqual(x1, y1) and
            PartialOrder.lessEqual(x2, y2) and
            PartialOrder.lessEqual(x3, y3) and
            PartialOrder.lessEqual(x4, y4)
    }
}

instance PartialOrder[(a1, a2, a3, a4, a5)] with PartialOrder[a1], PartialOrder[a2],
                                                 PartialOrder[a3], PartialOrder[a4],
                                                 PartialOrder[a5] {
    pub def lessEqual(x: (a1, a2, a3, a4, a5),
                           y: (a1, a2, a3, a4, a5)): Bool = match (x, y) {
        case ((x1, x2, x3, x4, x5), (y1, y2, y3, y4, y5)) =>
            PartialOrder.lessEqual(x1, y1) and
            PartialOrder.lessEqual(x2, y2) and
            PartialOrder.lessEqual(x3, y3) and
            PartialOrder.lessEqual(x4, y4) and
            PartialOrder.lessEqual(x5, y5)
    }
}

instance PartialOrder[(a1, a2, a3, a4, a5, a6)] with PartialOrder[a1], PartialOrder[a2],
                                                     PartialOrder[a3], PartialOrder[a4],
                                                     PartialOrder[a5], PartialOrder[a6] {
    pub def lessEqual(x: (a1, a2, a3, a4, a5, a6),
                           y: (a1, a2, a3, a4, a5, a6)): Bool = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6), (y1, y2, y3, y4, y5, y6)) =>
            PartialOrder.lessEqual(x1, y1) and
            PartialOrder.lessEqual(x2, y2) and
            PartialOrder.lessEqual(x3, y3) and
            PartialOrder.lessEqual(x4, y4) and
            PartialOrder.lessEqual(x5, y5) and
            PartialOrder.lessEqual(x6, y6)
    }
}

instance PartialOrder[(a1, a2, a3, a4, a5, a6, a7)] with PartialOrder[a1], PartialOrder[a2],
                                                         PartialOrder[a3], PartialOrder[a4],
                                                         PartialOrder[a5], PartialOrder[a6],
                                                         PartialOrder[a7] {
    pub def lessEqual(x: (a1, a2, a3, a4, a5, a6, a7),
                           y: (a1, a2, a3, a4, a5, a6, a7)): Bool = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7), (y1, y2, y3, y4, y5, y6, y7)) =>
            PartialOrder.lessEqual(x1, y1) and
            PartialOrder.lessEqual(x2, y2) and
            PartialOrder.lessEqual(x3, y3) and
            PartialOrder.lessEqual(x4, y4) and
            PartialOrder.lessEqual(x5, y5) and
            PartialOrder.lessEqual(x6, y6) and
            PartialOrder.lessEqual(x7, y7)
    }
}

instance PartialOrder[(a1, a2, a3, a4, a5, a6, a7, a8)] with PartialOrder[a1], PartialOrder[a2],
                                                             PartialOrder[a3], PartialOrder[a4],
                                                             PartialOrder[a5], PartialOrder[a6],
                                                             PartialOrder[a7], PartialOrder[a8] {
    pub def lessEqual(x: (a1, a2, a3, a4, a5, a6, a7, a8),
                           y: (a1, a2, a3, a4, a5, a6, a7, a8)): Bool = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7, x8), (y1, y2, y3, y4, y5, y6, y7, y8)) =>
            PartialOrder.lessEqual(x1, y1) and
            PartialOrder.lessEqual(x2, y2) and
            PartialOrder.lessEqual(x3, y3) and
            PartialOrder.lessEqual(x4, y4) and
            PartialOrder.lessEqual(x5, y5) and
            PartialOrder.lessEqual(x6, y6) and
            PartialOrder.lessEqual(x7, y7) and
            PartialOrder.lessEqual(x8, y8)
    }
}

instance PartialOrder[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with PartialOrder[a1], PartialOrder[a2],
                                                                 PartialOrder[a3], PartialOrder[a4],
                                                                 PartialOrder[a5], PartialOrder[a6],
                                                                 PartialOrder[a7], PartialOrder[a8],
                                                                 PartialOrder[a9] {
    pub def lessEqual(x: (a1, a2, a3, a4, a5, a6, a7, a8, a9),
                           y: (a1, a2, a3, a4, a5, a6, a7, a8, a9)): Bool = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7, x8, x9), (y1, y2, y3, y4, y5, y6, y7, y8, y9)) =>
            PartialOrder.lessEqual(x1, y1) and
            PartialOrder.lessEqual(x2, y2) and
            PartialOrder.lessEqual(x3, y3) and
            PartialOrder.lessEqual(x4, y4) and
            PartialOrder.lessEqual(x5, y5) and
            PartialOrder.lessEqual(x6, y6) and
            PartialOrder.lessEqual(x7, y7) and
            PartialOrder.lessEqual(x8, y8) and
            PartialOrder.lessEqual(x9, y9)
    }
}

instance PartialOrder[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with PartialOrder[a1], PartialOrder[a2],
                                                                      PartialOrder[a3], PartialOrder[a4],
                                                                      PartialOrder[a5], PartialOrder[a6],
                                                                      PartialOrder[a7], PartialOrder[a8],
                                                                      PartialOrder[a9], PartialOrder[a10] {
    pub def lessEqual(x: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10),
                           y: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)): Bool = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10), (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10)) =>
            PartialOrder.lessEqual(x1, y1) and
            PartialOrder.lessEqual(x2, y2) and
            PartialOrder.lessEqual(x3, y3) and
            PartialOrder.lessEqual(x4, y4) and
            PartialOrder.lessEqual(x5, y5) and
            PartialOrder.lessEqual(x6, y6) and
            PartialOrder.lessEqual(x7, y7) and
            PartialOrder.lessEqual(x8, y8) and
            PartialOrder.lessEqual(x9, y9) and
            PartialOrder.lessEqual(x10, y10)
    }
}

///
/// A type class for join semi lattices.
///
pub lawful class JoinLattice[a] with PartialOrder[a] {

    ///
    /// Returns the least upper bound of `x` and `y`.
    ///
    pub def leastUpperBound(x: a, y: a): a

    ///
    /// The law asserts that the least upper bound operator returns
    /// an element that is greater than or equal to each of its arguments.
    ///
    law leastUpperBound1: forall(x: a, y: a).
        PartialOrder.lessEqual(x, JoinLattice.leastUpperBound(x, y)) and
        PartialOrder.lessEqual(y, JoinLattice.leastUpperBound(x, y))

    ///
    /// The law asserts that the least upper bound operator returns
    /// the smallest element that is larger than its two arguments.
    ///
    law leastUpperBound2: forall(x: a, y: a, z: a).
        (PartialOrder.lessEqual(x, z) and PartialOrder.lessEqual(y, z)) ==>
        PartialOrder.lessEqual(JoinLattice.leastUpperBound(x, y), z)

}

instance JoinLattice[Int8] {
    pub def leastUpperBound(x: Int8, y: Int8): Int8 = Order.max(x, y)
}

instance JoinLattice[Int16] {
    pub def leastUpperBound(x: Int16, y: Int16): Int16 = Order.max(x, y)
}

instance JoinLattice[Int32] {
    pub def leastUpperBound(x: Int32, y: Int32): Int32 = Order.max(x, y)
}

instance JoinLattice[Int64] {
    pub def leastUpperBound(x: Int64, y: Int64): Int64 = Order.max(x, y)
}

instance JoinLattice[BigInt] {
    pub def leastUpperBound(x: BigInt, y: BigInt): BigInt = Order.max(x, y)
}

instance JoinLattice[(a1, a2)] with JoinLattice[a1], JoinLattice[a2] {
    pub def leastUpperBound(x: (a1, a2),
                            y: (a1, a2)): (a1, a2) = match (x, y) {
        case ((x1, x2), (y1, y2)) => (
            JoinLattice.leastUpperBound(x1, y1),
            JoinLattice.leastUpperBound(x2, y2)
        )
    }
}

instance JoinLattice[(a1, a2, a3)] with JoinLattice[a1], JoinLattice[a2],
                                        JoinLattice[a3] {
    pub def leastUpperBound(x: (a1, a2, a3),
                            y: (a1, a2, a3)): (a1, a2, a3) = match (x, y) {
        case ((x1, x2, x3), (y1, y2, y3)) => (
            JoinLattice.leastUpperBound(x1, y1),
            JoinLattice.leastUpperBound(x2, y2),
            JoinLattice.leastUpperBound(x3, y3)
        )
    }
}

instance JoinLattice[(a1, a2, a3, a4)] with JoinLattice[a1], JoinLattice[a2],
                                            JoinLattice[a3], JoinLattice[a4] {
    pub def leastUpperBound(x: (a1, a2, a3, a4),
                            y: (a1, a2, a3, a4)): (a1, a2, a3, a4) = match (x, y) {
        case ((x1, x2, x3, x4), (y1, y2, y3, y4)) => (
            JoinLattice.leastUpperBound(x1, y1),
            JoinLattice.leastUpperBound(x2, y2),
            JoinLattice.leastUpperBound(x3, y3),
            JoinLattice.leastUpperBound(x4, y4)
        )
    }
}

instance JoinLattice[(a1, a2, a3, a4, a5)] with JoinLattice[a1], JoinLattice[a2],
                                                JoinLattice[a3], JoinLattice[a4],
                                                JoinLattice[a5] {
    pub def leastUpperBound(x: (a1, a2, a3, a4, a5),
                            y: (a1, a2, a3, a4, a5)): (a1, a2, a3, a4, a5) = match (x, y) {
        case ((x1, x2, x3, x4, x5), (y1, y2, y3, y4, y5)) => (
            JoinLattice.leastUpperBound(x1, y1),
            JoinLattice.leastUpperBound(x2, y2),
            JoinLattice.leastUpperBound(x3, y3),
            JoinLattice.leastUpperBound(x4, y4),
            JoinLattice.leastUpperBound(x5, y5)
        )
    }
}

instance JoinLattice[(a1, a2, a3, a4, a5, a6)] with JoinLattice[a1], JoinLattice[a2],
                                                    JoinLattice[a3], JoinLattice[a4],
                                                    JoinLattice[a5], JoinLattice[a6] {
    pub def leastUpperBound(x: (a1, a2, a3, a4, a5, a6),
                            y: (a1, a2, a3, a4, a5, a6)): (a1, a2, a3, a4, a5, a6) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6), (y1, y2, y3, y4, y5, y6)) => (
            JoinLattice.leastUpperBound(x1, y1),
            JoinLattice.leastUpperBound(x2, y2),
            JoinLattice.leastUpperBound(x3, y3),
            JoinLattice.leastUpperBound(x4, y4),
            JoinLattice.leastUpperBound(x5, y5),
            JoinLattice.leastUpperBound(x6, y6)
        )
    }
}

instance JoinLattice[(a1, a2, a3, a4, a5, a6, a7)] with JoinLattice[a1], JoinLattice[a2],
                                                        JoinLattice[a3], JoinLattice[a4],
                                                        JoinLattice[a5], JoinLattice[a6],
                                                        JoinLattice[a7] {
    pub def leastUpperBound(x: (a1, a2, a3, a4, a5, a6, a7),
                            y: (a1, a2, a3, a4, a5, a6, a7)): (a1, a2, a3, a4, a5, a6, a7) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7), (y1, y2, y3, y4, y5, y6, y7)) => (
            JoinLattice.leastUpperBound(x1, y1),
            JoinLattice.leastUpperBound(x2, y2),
            JoinLattice.leastUpperBound(x3, y3),
            JoinLattice.leastUpperBound(x4, y4),
            JoinLattice.leastUpperBound(x5, y5),
            JoinLattice.leastUpperBound(x6, y6),
            JoinLattice.leastUpperBound(x7, y7)
        )
    }
}

instance JoinLattice[(a1, a2, a3, a4, a5, a6, a7, a8)] with JoinLattice[a1], JoinLattice[a2],
                                                            JoinLattice[a3], JoinLattice[a4],
                                                            JoinLattice[a5], JoinLattice[a6],
                                                            JoinLattice[a7], JoinLattice[a8] {
    pub def leastUpperBound(x: (a1, a2, a3, a4, a5, a6, a7, a8),
                            y: (a1, a2, a3, a4, a5, a6, a7, a8)): (a1, a2, a3, a4, a5, a6, a7, a8) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7, x8), (y1, y2, y3, y4, y5, y6, y7, y8)) => (
            JoinLattice.leastUpperBound(x1, y1),
            JoinLattice.leastUpperBound(x2, y2),
            JoinLattice.leastUpperBound(x3, y3),
            JoinLattice.leastUpperBound(x4, y4),
            JoinLattice.leastUpperBound(x5, y5),
            JoinLattice.leastUpperBound(x6, y6),
            JoinLattice.leastUpperBound(x7, y7),
            JoinLattice.leastUpperBound(x8, y8)
        )
    }
}

instance JoinLattice[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with JoinLattice[a1], JoinLattice[a2],
                                                                JoinLattice[a3], JoinLattice[a4],
                                                                JoinLattice[a5], JoinLattice[a6],
                                                                JoinLattice[a7], JoinLattice[a8],
                                                                JoinLattice[a9] {
    pub def leastUpperBound(x: (a1, a2, a3, a4, a5, a6, a7, a8, a9),
                            y: (a1, a2, a3, a4, a5, a6, a7, a8, a9)): (a1, a2, a3, a4, a5, a6, a7, a8, a9) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7, x8, x9), (y1, y2, y3, y4, y5, y6, y7, y8, y9)) => (
            JoinLattice.leastUpperBound(x1, y1),
            JoinLattice.leastUpperBound(x2, y2),
            JoinLattice.leastUpperBound(x3, y3),
            JoinLattice.leastUpperBound(x4, y4),
            JoinLattice.leastUpperBound(x5, y5),
            JoinLattice.leastUpperBound(x6, y6),
            JoinLattice.leastUpperBound(x7, y7),
            JoinLattice.leastUpperBound(x8, y8),
            JoinLattice.leastUpperBound(x9, y9)
        )
    }
}

instance JoinLattice[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with JoinLattice[a1], JoinLattice[a2],
                                                                     JoinLattice[a3], JoinLattice[a4],
                                                                     JoinLattice[a5], JoinLattice[a6],
                                                                     JoinLattice[a7], JoinLattice[a8],
                                                                     JoinLattice[a9], JoinLattice[a10] {
    pub def leastUpperBound(x: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10),
                            y: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)): (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10), (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10)) => (
            JoinLattice.leastUpperBound(x1, y1),
            JoinLattice.leastUpperBound(x2, y2),
            JoinLattice.leastUpperBound(x3, y3),
            JoinLattice.leastUpperBound(x4, y4),
            JoinLattice.leastUpperBound(x5, y5),
            JoinLattice.leastUpperBound(x6, y6),
            JoinLattice.leastUpperBound(x7, y7),
            JoinLattice.leastUpperBound(x8, y8),
            JoinLattice.leastUpperBound(x9, y9),
            JoinLattice.leastUpperBound(x10, y10)
        )
    }
}

///
/// A type class for meet semi lattices.
/// A Meet Lattice is pair of functions (⊑, ⊓) where ⊑ is a partial order and ⊓ satisfy two properties:
/// lower-bound and greatest-lower-bound.
///
pub lawful class MeetLattice[a] with PartialOrder[a] {

    ///
    /// Returns the greatest lower bound of `x` and `y`.
    ///
    pub def greatestLowerBound(x: a, y: a): a

    ///
    /// The lower bound law asserts that the greatest lower bound operator returns an element that
    /// is less than or equal to each of its arguments.
    ///
    law greatestLowerBound1: forall(x: a, y: a).
        PartialOrder.lessEqual(MeetLattice.greatestLowerBound(x, y), x) and
        PartialOrder.lessEqual(MeetLattice.greatestLowerBound(x, y), y)

    ///
    /// The greatest lower bound law asserts that the greatest lower bound operator returns the
    /// largest element that is smaller than its two arguments.
    ///
    law greatestLowerBound2: forall(x: a, y: a, z: a).
        (PartialOrder.lessEqual(z, x) and PartialOrder.lessEqual(z, y)) ==>
        PartialOrder.lessEqual(z, MeetLattice.greatestLowerBound(x, y))

}

instance MeetLattice[Int8] {
    pub def greatestLowerBound(x: Int8, y: Int8): Int8 = Order.min(x, y)
}

instance MeetLattice[Int16] {
    pub def greatestLowerBound(x: Int16, y: Int16): Int16 = Order.min(x, y)
}

instance MeetLattice[Int32] {
    pub def greatestLowerBound(x: Int32, y: Int32): Int32 = Order.min(x, y)
}

instance MeetLattice[Int64] {
    pub def greatestLowerBound(x: Int64, y: Int64): Int64 = Order.min(x, y)
}

instance MeetLattice[BigInt] {
    pub def greatestLowerBound(x: BigInt, y: BigInt): BigInt = Order.min(x, y)
}

instance MeetLattice[(a1, a2)] with MeetLattice[a1], MeetLattice[a2] {
    pub def greatestLowerBound(x: (a1, a2),
                            y: (a1, a2)): (a1, a2) = match (x, y) {
        case ((x1, x2), (y1, y2)) => (
            MeetLattice.greatestLowerBound(x1, y1),
            MeetLattice.greatestLowerBound(x2, y2)
        )
    }
}

instance MeetLattice[(a1, a2, a3)] with MeetLattice[a1], MeetLattice[a2],
                                        MeetLattice[a3] {
    pub def greatestLowerBound(x: (a1, a2, a3),
                            y: (a1, a2, a3)): (a1, a2, a3) = match (x, y) {
        case ((x1, x2, x3), (y1, y2, y3)) => (
            MeetLattice.greatestLowerBound(x1, y1),
            MeetLattice.greatestLowerBound(x2, y2),
            MeetLattice.greatestLowerBound(x3, y3)
        )
    }
}

instance MeetLattice[(a1, a2, a3, a4)] with MeetLattice[a1], MeetLattice[a2],
                                            MeetLattice[a3], MeetLattice[a4] {
    pub def greatestLowerBound(x: (a1, a2, a3, a4),
                            y: (a1, a2, a3, a4)): (a1, a2, a3, a4) = match (x, y) {
        case ((x1, x2, x3, x4), (y1, y2, y3, y4)) => (
            MeetLattice.greatestLowerBound(x1, y1),
            MeetLattice.greatestLowerBound(x2, y2),
            MeetLattice.greatestLowerBound(x3, y3),
            MeetLattice.greatestLowerBound(x4, y4)
        )
    }
}

instance MeetLattice[(a1, a2, a3, a4, a5)] with MeetLattice[a1], MeetLattice[a2],
                                                MeetLattice[a3], MeetLattice[a4],
                                                MeetLattice[a5] {
    pub def greatestLowerBound(x: (a1, a2, a3, a4, a5),
                            y: (a1, a2, a3, a4, a5)): (a1, a2, a3, a4, a5) = match (x, y) {
        case ((x1, x2, x3, x4, x5), (y1, y2, y3, y4, y5)) => (
            MeetLattice.greatestLowerBound(x1, y1),
            MeetLattice.greatestLowerBound(x2, y2),
            MeetLattice.greatestLowerBound(x3, y3),
            MeetLattice.greatestLowerBound(x4, y4),
            MeetLattice.greatestLowerBound(x5, y5)
        )
    }
}

instance MeetLattice[(a1, a2, a3, a4, a5, a6)] with MeetLattice[a1], MeetLattice[a2],
                                                    MeetLattice[a3], MeetLattice[a4],
                                                    MeetLattice[a5], MeetLattice[a6] {
    pub def greatestLowerBound(x: (a1, a2, a3, a4, a5, a6),
                            y: (a1, a2, a3, a4, a5, a6)): (a1, a2, a3, a4, a5, a6) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6), (y1, y2, y3, y4, y5, y6)) => (
            MeetLattice.greatestLowerBound(x1, y1),
            MeetLattice.greatestLowerBound(x2, y2),
            MeetLattice.greatestLowerBound(x3, y3),
            MeetLattice.greatestLowerBound(x4, y4),
            MeetLattice.greatestLowerBound(x5, y5),
            MeetLattice.greatestLowerBound(x6, y6)
        )
    }
}

instance MeetLattice[(a1, a2, a3, a4, a5, a6, a7)] with MeetLattice[a1], MeetLattice[a2],
                                                        MeetLattice[a3], MeetLattice[a4],
                                                        MeetLattice[a5], MeetLattice[a6],
                                                        MeetLattice[a7] {
    pub def greatestLowerBound(x: (a1, a2, a3, a4, a5, a6, a7),
                            y: (a1, a2, a3, a4, a5, a6, a7)): (a1, a2, a3, a4, a5, a6, a7) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7), (y1, y2, y3, y4, y5, y6, y7)) => (
            MeetLattice.greatestLowerBound(x1, y1),
            MeetLattice.greatestLowerBound(x2, y2),
            MeetLattice.greatestLowerBound(x3, y3),
            MeetLattice.greatestLowerBound(x4, y4),
            MeetLattice.greatestLowerBound(x5, y5),
            MeetLattice.greatestLowerBound(x6, y6),
            MeetLattice.greatestLowerBound(x7, y7)
        )
    }
}

instance MeetLattice[(a1, a2, a3, a4, a5, a6, a7, a8)] with MeetLattice[a1], MeetLattice[a2],
                                                            MeetLattice[a3], MeetLattice[a4],
                                                            MeetLattice[a5], MeetLattice[a6],
                                                            MeetLattice[a7], MeetLattice[a8] {
    pub def greatestLowerBound(x: (a1, a2, a3, a4, a5, a6, a7, a8),
                            y: (a1, a2, a3, a4, a5, a6, a7, a8)): (a1, a2, a3, a4, a5, a6, a7, a8) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7, x8), (y1, y2, y3, y4, y5, y6, y7, y8)) => (
            MeetLattice.greatestLowerBound(x1, y1),
            MeetLattice.greatestLowerBound(x2, y2),
            MeetLattice.greatestLowerBound(x3, y3),
            MeetLattice.greatestLowerBound(x4, y4),
            MeetLattice.greatestLowerBound(x5, y5),
            MeetLattice.greatestLowerBound(x6, y6),
            MeetLattice.greatestLowerBound(x7, y7),
            MeetLattice.greatestLowerBound(x8, y8)
        )
    }
}

instance MeetLattice[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with MeetLattice[a1], MeetLattice[a2],
                                                                MeetLattice[a3], MeetLattice[a4],
                                                                MeetLattice[a5], MeetLattice[a6],
                                                                MeetLattice[a7], MeetLattice[a8],
                                                                MeetLattice[a9] {
    pub def greatestLowerBound(x: (a1, a2, a3, a4, a5, a6, a7, a8, a9),
                            y: (a1, a2, a3, a4, a5, a6, a7, a8, a9)): (a1, a2, a3, a4, a5, a6, a7, a8, a9) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7, x8, x9), (y1, y2, y3, y4, y5, y6, y7, y8, y9)) => (
            MeetLattice.greatestLowerBound(x1, y1),
            MeetLattice.greatestLowerBound(x2, y2),
            MeetLattice.greatestLowerBound(x3, y3),
            MeetLattice.greatestLowerBound(x4, y4),
            MeetLattice.greatestLowerBound(x5, y5),
            MeetLattice.greatestLowerBound(x6, y6),
            MeetLattice.greatestLowerBound(x7, y7),
            MeetLattice.greatestLowerBound(x8, y8),
            MeetLattice.greatestLowerBound(x9, y9)
        )
    }
}

instance MeetLattice[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with MeetLattice[a1], MeetLattice[a2],
                                                                     MeetLattice[a3], MeetLattice[a4],
                                                                     MeetLattice[a5], MeetLattice[a6],
                                                                     MeetLattice[a7], MeetLattice[a8],
                                                                     MeetLattice[a9], MeetLattice[a10] {
    pub def greatestLowerBound(x: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10),
                            y: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)): (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10), (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10)) => (
            MeetLattice.greatestLowerBound(x1, y1),
            MeetLattice.greatestLowerBound(x2, y2),
            MeetLattice.greatestLowerBound(x3, y3),
            MeetLattice.greatestLowerBound(x4, y4),
            MeetLattice.greatestLowerBound(x5, y5),
            MeetLattice.greatestLowerBound(x6, y6),
            MeetLattice.greatestLowerBound(x7, y7),
            MeetLattice.greatestLowerBound(x8, y8),
            MeetLattice.greatestLowerBound(x9, y9),
            MeetLattice.greatestLowerBound(x10, y10)
        )
    }
}

///
/// A type class for Monoids, objects that support an associative binary
/// operation `combine` and neutral element `empty`.
///
pub lawful class Monoid[a] with SemiGroup[a] {
    ///
    /// Returns a neutral element.
    ///
    pub def empty(): a

    ///
    /// Returns the result of combining `x` and `y` using SemiGroup's combine.
    ///
    pub def combine(x: a, y: a): a =
        SemiGroup.combine(x, y)

    law leftIdentity: forall(x: a) with Eq[a] . Monoid.combine(Monoid.empty(), x) == x

    law rightIdentity: forall(x: a) with Eq[a] . Monoid.combine(x, Monoid.empty()) == x

    law associative: forall(x: a, y: a, z: a) with Eq[a] . Monoid.combine(Monoid.combine(x, y), z) == Monoid.combine(x, Monoid.combine(y, z))

}

///
/// A type class for types that form a semigroup.
///
pub class SemiGroup[a] {
    ///
    /// An associative binary operation on `a`.
    ///
    pub def combine(x: a, y: a): a

    ///
    /// Returns `x` combined with itself `n` times.
    ///
    pub def combineN(x: a, n: Int32): a =
        if (n <= 1) x
        else SemiGroup.combine(x, SemiGroup.combineN(x, n - 1))

    law associative: forall(x: a, y: a, z: a) with Eq[a] . SemiGroup.combine(SemiGroup.combine(x, y), z) == SemiGroup.combine(x, SemiGroup.combine(y, z))

}

///
/// Alias for `SemiGroup.combine`.
///
pub def ++(x: a, y: a): a with SemiGroup[a] = SemiGroup.combine(x, y)

instance SemiGroup[Unit] {
    pub def combine(_: Unit, _: Unit): Unit = ()
}

instance SemiGroup[Int8] {
    pub def combine(x: Int8, y: Int8): Int8 = x + y
}

instance SemiGroup[Int16] {
    pub def combine(x: Int16, y: Int16): Int16 = x + y
}

instance SemiGroup[Int32] {
    pub def combine(x: Int32, y: Int32): Int32 = x + y
}

instance SemiGroup[Int64] {
    pub def combine(x: Int64, y: Int64): Int64 = x + y
}

instance SemiGroup[BigInt] {
    pub def combine(x: BigInt, y: BigInt): BigInt = x + y
}

instance SemiGroup[Float32] {
    pub def combine(x: Float32, y: Float32): Float32 = x + y
}

instance SemiGroup[Float64] {
    pub def combine(x: Float64, y: Float64): Float64 = x + y
}

instance SemiGroup[String] {
    pub def combine(x: String, y: String): String = x + y
}

instance SemiGroup[(a1, a2)] with SemiGroup[a1], SemiGroup[a2] {
    pub def combine(x: (a1, a2), y: (a1, a2)): (a1, a2) = match (x, y) {
        case ((x1, x2), (y1, y2)) => (SemiGroup.combine(x1, y1), SemiGroup.combine(x2, y2))
    }
}

instance SemiGroup[(a1, a2, a3)] with SemiGroup[a1], SemiGroup[a2], SemiGroup[a3] {
    pub def combine(x: (a1, a2, a3), y: (a1, a2, a3)): (a1, a2, a3) = match (x, y) {
        case ((x1, x2, x3), (y1, y2, y3)) => (SemiGroup.combine(x1, y1), SemiGroup.combine(x2, y2), SemiGroup.combine(x3, y3))
    }
}

instance SemiGroup[(a1, a2, a3, a4)] with SemiGroup[a1], SemiGroup[a2], SemiGroup[a3], SemiGroup[a4] {
    pub def combine(x: (a1, a2, a3, a4), y: (a1, a2, a3, a4)): (a1, a2, a3, a4) = match (x, y) {
        case ((x1, x2, x3, x4), (y1, y2, y3, y4)) => (SemiGroup.combine(x1, y1), SemiGroup.combine(x2, y2), SemiGroup.combine(x3, y3), SemiGroup.combine(x4, y4))
    }
}

instance SemiGroup[(a1, a2, a3, a4, a5)] with SemiGroup[a1], SemiGroup[a2], SemiGroup[a3], SemiGroup[a4], SemiGroup[a5] {
    pub def combine(x: (a1, a2, a3, a4, a5), y: (a1, a2, a3, a4, a5)): (a1, a2, a3, a4, a5) = match (x, y) {
        case ((x1, x2, x3, x4, x5), (y1, y2, y3, y4, y5)) => (SemiGroup.combine(x1, y1), SemiGroup.combine(x2, y2), SemiGroup.combine(x3, y3), SemiGroup.combine(x4, y4), SemiGroup.combine(x5, y5))
    }
}

instance SemiGroup[(a1, a2, a3, a4, a5, a6)] with SemiGroup[a1], SemiGroup[a2], SemiGroup[a3], SemiGroup[a4], SemiGroup[a5], SemiGroup[a6] {
    pub def combine(x: (a1, a2, a3, a4, a5, a6), y: (a1, a2, a3, a4, a5, a6)): (a1, a2, a3, a4, a5, a6) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6), (y1, y2, y3, y4, y5, y6)) => (SemiGroup.combine(x1, y1), SemiGroup.combine(x2, y2), SemiGroup.combine(x3, y3), SemiGroup.combine(x4, y4), SemiGroup.combine(x5, y5), SemiGroup.combine(x6, y6))
    }
}

instance SemiGroup[(a1, a2, a3, a4, a5, a6, a7)] with SemiGroup[a1], SemiGroup[a2], SemiGroup[a3], SemiGroup[a4], SemiGroup[a5], SemiGroup[a6], SemiGroup[a7] {
    pub def combine(x: (a1, a2, a3, a4, a5, a6, a7), y: (a1, a2, a3, a4, a5, a6, a7)): (a1, a2, a3, a4, a5, a6, a7) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7), (y1, y2, y3, y4, y5, y6, y7)) => (SemiGroup.combine(x1, y1), SemiGroup.combine(x2, y2), SemiGroup.combine(x3, y3), SemiGroup.combine(x4, y4), SemiGroup.combine(x5, y5), SemiGroup.combine(x6, y6), SemiGroup.combine(x7, y7))
    }
}

instance SemiGroup[(a1, a2, a3, a4, a5, a6, a7, a8)] with SemiGroup[a1], SemiGroup[a2], SemiGroup[a3], SemiGroup[a4], SemiGroup[a5], SemiGroup[a6], SemiGroup[a7], SemiGroup[a8] {
    pub def combine(x: (a1, a2, a3, a4, a5, a6, a7, a8), y: (a1, a2, a3, a4, a5, a6, a7, a8)): (a1, a2, a3, a4, a5, a6, a7, a8) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7, x8), (y1, y2, y3, y4, y5, y6, y7, y8)) => (SemiGroup.combine(x1, y1), SemiGroup.combine(x2, y2), SemiGroup.combine(x3, y3), SemiGroup.combine(x4, y4), SemiGroup.combine(x5, y5), SemiGroup.combine(x6, y6), SemiGroup.combine(x7, y7), SemiGroup.combine(x8, y8))
    }
}

instance SemiGroup[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with SemiGroup[a1], SemiGroup[a2], SemiGroup[a3], SemiGroup[a4], SemiGroup[a5], SemiGroup[a6], SemiGroup[a7], SemiGroup[a8], SemiGroup[a9] {
    pub def combine(x: (a1, a2, a3, a4, a5, a6, a7, a8, a9), y: (a1, a2, a3, a4, a5, a6, a7, a8, a9)): (a1, a2, a3, a4, a5, a6, a7, a8, a9) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7, x8, x9), (y1, y2, y3, y4, y5, y6, y7, y8, y9)) => (SemiGroup.combine(x1, y1), SemiGroup.combine(x2, y2), SemiGroup.combine(x3, y3), SemiGroup.combine(x4, y4), SemiGroup.combine(x5, y5), SemiGroup.combine(x6, y6), SemiGroup.combine(x7, y7), SemiGroup.combine(x8, y8), SemiGroup.combine(x9, y9))
    }
}

instance SemiGroup[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with SemiGroup[a1], SemiGroup[a2], SemiGroup[a3], SemiGroup[a4], SemiGroup[a5], SemiGroup[a6], SemiGroup[a7], SemiGroup[a8], SemiGroup[a9], SemiGroup[a10] {
    pub def combine(x: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10), y: (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)): (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = match (x, y) {
        case ((x1, x2, x3, x4, x5, x6, x7, x8, x9, x10), (y1, y2, y3, y4, y5, y6, y7, y8, y9, y10)) => (SemiGroup.combine(x1, y1), SemiGroup.combine(x2, y2), SemiGroup.combine(x3, y3), SemiGroup.combine(x4, y4), SemiGroup.combine(x5, y5), SemiGroup.combine(x6, y6), SemiGroup.combine(x7, y7), SemiGroup.combine(x8, y8), SemiGroup.combine(x9, y9), SemiGroup.combine(x10, y10))
    }
}

namespace SemiGroup {

    @Internal
    pub enum Any(Bool)

    instance SemiGroup[Any] {
        pub def combine(x: Any, y: Any): Any = match (x, y) {
            case (Any(a), Any(b)) => Any(a or b)
        }
    }

    instance Monoid[Any] {
        pub def empty(): Any = Any(false)
    }

    instance CommutativeMonoid[Any]


    @Internal
    pub enum All(Bool)

    instance SemiGroup[All] {
        pub def combine(x: All, y: All): All = match (x, y) {
            case (All(a), All(b)) => All(a and b)
        }
    }

    instance Monoid[All] {
        pub def empty(): All = All(true)
    }

    instance CommutativeMonoid[All]

}


namespace Monoid {

    ///
    /// Returns the result of applying `combine` to all the elements in `t`, using `empty` as the initial value.
    ///
    pub def fold[f: Type -> Type, a: Type](f: f[a]): a with Foldable[f], Monoid[a] = Foldable.fold(f)

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
/// A type class for types that form a commutative monoid.
///
pub lawful class CommutativeMonoid[a] with Monoid[a] {

    ///
    /// Returns a neutral element.
    ///
    pub def empty(): a = Monoid.empty()

    ///
    /// An associative & commutative binary operation on `a`.
    ///
    pub def combine(x: a, y: a): a =
        Monoid.combine(x, y)

    law leftIdentity: forall(x: a) with Eq[a] . CommutativeMonoid.combine(CommutativeMonoid.empty(), x) == x

    law rightIdentity: forall(x: a) with Eq[a] . CommutativeMonoid.combine(x, CommutativeMonoid.empty()) == x

    law associative: forall(x: a, y: a, z: a) with Eq[a] . CommutativeMonoid.combine(CommutativeMonoid.combine(x, y), z) == CommutativeMonoid.combine(x, CommutativeMonoid.combine(y, z))

    law commutative: forall(x: a, y: a) with Eq[a] . CommutativeMonoid.combine(x, y) == CommutativeMonoid.combine(y, x)

}
instance CommutativeMonoid[Unit]

instance CommutativeMonoid[Int8]

instance CommutativeMonoid[Int16]

instance CommutativeMonoid[Int32]

instance CommutativeMonoid[Int64]

instance CommutativeMonoid[BigInt]

instance CommutativeMonoid[Float32]

instance CommutativeMonoid[Float64]

instance CommutativeMonoid[(a1, a2)] with CommutativeMonoid[a1], CommutativeMonoid[a2]

instance CommutativeMonoid[(a1, a2, a3)] with CommutativeMonoid[a1], CommutativeMonoid[a2], CommutativeMonoid[a3]

instance CommutativeMonoid[(a1, a2, a3, a4)] with CommutativeMonoid[a1], CommutativeMonoid[a2], CommutativeMonoid[a3], CommutativeMonoid[a4]

instance CommutativeMonoid[(a1, a2, a3, a4, a5)] with CommutativeMonoid[a1], CommutativeMonoid[a2], CommutativeMonoid[a3], CommutativeMonoid[a4], CommutativeMonoid[a5]

instance CommutativeMonoid[(a1, a2, a3, a4, a5, a6)] with CommutativeMonoid[a1], CommutativeMonoid[a2], CommutativeMonoid[a3], CommutativeMonoid[a4], CommutativeMonoid[a5], CommutativeMonoid[a6]

instance CommutativeMonoid[(a1, a2, a3, a4, a5, a6, a7)] with CommutativeMonoid[a1], CommutativeMonoid[a2], CommutativeMonoid[a3], CommutativeMonoid[a4], CommutativeMonoid[a5], CommutativeMonoid[a6], CommutativeMonoid[a7]

instance CommutativeMonoid[(a1, a2, a3, a4, a5, a6, a7, a8)] with CommutativeMonoid[a1], CommutativeMonoid[a2], CommutativeMonoid[a3], CommutativeMonoid[a4], CommutativeMonoid[a5], CommutativeMonoid[a6], CommutativeMonoid[a7], CommutativeMonoid[a8]

instance CommutativeMonoid[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with CommutativeMonoid[a1], CommutativeMonoid[a2], CommutativeMonoid[a3], CommutativeMonoid[a4], CommutativeMonoid[a5], CommutativeMonoid[a6], CommutativeMonoid[a7], CommutativeMonoid[a8], CommutativeMonoid[a9]

instance CommutativeMonoid[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with CommutativeMonoid[a1], CommutativeMonoid[a2], CommutativeMonoid[a3], CommutativeMonoid[a4], CommutativeMonoid[a5], CommutativeMonoid[a6], CommutativeMonoid[a7], CommutativeMonoid[a8], CommutativeMonoid[a9], CommutativeMonoid[a10]

///
/// A type class for unordered data structures that can be folded.
///
pub class UnorderedFoldable[t : Type -> Type] {

    ///
    /// Unordered fold of `t`.
    ///
    /// Applies `f` to all elements in `t` and combines the results
    /// i.e. `CommutativeMonoid.combine(f(a))` for all `a` in `t`.
    ///
    pub def foldMap(f: a -> b \ ef, t: t[a]): b \ ef with CommutativeMonoid[b]

    ///
    /// Returns the result of applying `CommutativeMonoid.combine` to all the elements in `t`.
    ///
    pub def fold(t: t[a]): a with CommutativeMonoid[a] =
        UnorderedFoldable.foldMap(identity, t)

    ///
    /// Returns true if and only if `t` is empty.
    ///
    pub def isEmpty(t: t[a]): Bool =
        UnorderedFoldable.forall(_ -> false, t)

    ///
    /// Returns `true` if and only if at least one element in `t` satisfies the predicate `f`.
    ///
    /// Returns `false` if `t` is empty.
    ///
    pub def exists(f: a -> Bool \ ef, t: t[a]): Bool \ ef =
        use SemiGroup.Any.Any;
        let Any(x) = UnorderedFoldable.foldMap(a -> Any(f(a)), t);
        x

    ///
    /// Returns `true` if and only if all elements in `t` satisfy the predicate `f`.
    ///
    /// Returns `true` if `t` is empty.
    ///
    pub def forall(f: a -> Bool \ ef, t: t[a]): Bool \ ef =
        use SemiGroup.All.All;
        let All(x) = UnorderedFoldable.foldMap(a -> All(f(a)), t);
        x

    ///
    /// Returns `true` if and only if the element `x` is in `t`.
    ///
    pub def memberOf(x: a, t: t[a]): Bool with Eq[a] =
        UnorderedFoldable.exists(y -> x == y, t)

    ///
    /// Returns the number of elements in `t`.
    ///
    pub def size(t: t[a]): Int32 =
        UnorderedFoldable.foldMap(_ -> 1, t)

    ///
    /// Returns the number of elements in `t` that satisfy the predicate `f`.
    ///
    pub def count(f: a -> Bool \ ef, t: t[a]): Int32 \ ef =
        UnorderedFoldable.foldMap(a -> if (f(a)) 1 else 0, t)

}

///
/// A type class for data structures that can be traversed in left-to-right
/// order with an applicative functor.
///
pub class Traversable[t : Type -> Type] with Functor[t], Foldable[t] {

    ///
    /// Returns the result of applying the applicative mapping function `f` to all the elements of the
    /// data structure `t`.
    ///
    pub def traverse(f: a -> m[b] \ ef, t: t[a]): m[t[b]] \ ef with Applicative[m]

    ///
    /// Returns the result of running all the actions in the data structure `t`.
    ///
    pub def sequence(t: t[m[a]]): m[t[a]] with Applicative[m] =
        Traversable.traverse(identity, t)

    ///
    /// Traversing with the identity function wrapped into an applicative preserves the container `t`.
    ///
    law identity: forall(t: t[a], f: a -> m[a]) with Eq[m[t[a]]], Applicative[m] . Traversable.traverse(f, t) == Applicative.point(t)

    ///
    /// sequence identity.
    ///
    law identity: forall(t: t[a], f: a -> m[a]) with Eq[m[t[a]]], Applicative[m] . Traversable.sequence(Functor.map(f, t)) == Applicative.point(t)

    // Missing laws: naturality and composition.

}


namespace Traversable {

    ///
    /// Returns the result of applying the applicative mapping function `f` to all the elements of the
    /// data structure `t`.
    ///
    /// `for` is `traverse` with it's arguments flipped.
    ///
    pub def for(t: t[a], f: a -> m[b] \ ef): m[t[b]] \ ef with Applicative[m], Traversable[t] =
        Traversable.traverse(f, t)

}

///
/// A type class for filtering container functors.
///
pub class Filterable[m : Type -> Type] with Functor[m] {

    ///
    /// Applies the partial function `f` to every element in `x` collecting the results.
    ///
    pub def filterMap(f: a -> Option[b] \ ef, t: m[a]): m[b] \ ef

    ///
    /// Applies `f` to every element in `x`. Keeps every element that satisfies `f`.
    ///
    pub def filter(f: a -> Bool \ ef, t: m[a]): m[a] \ ef = Filterable.filterMap(x -> if (f(x)) Some(x) else None, t)

}

///
/// A type class for data structures that can be traversed in left-to-right
/// order with an applicative partial functor.
///
pub class Witherable[t : Type -> Type] with Traversable[t], Filterable[t] {

    ///
    /// Returns the result of applying the applicative partial mapping function `f` to all the elements of the
    /// data structure `t`.
    ///
    pub def wither(f: a -> m[Option[b]] \ ef, t: t[a]): m[t[b]] \ ef with Applicative[m] =
        use Functor.{<$>};
        use Filterable.filterMap;
        (identity |> filterMap) <$> Traversable.traverse(f, t)

    ///
    /// Returns the result of running all the actions in the data structure `t`.
    ///
    pub def sequenceWither(t: t[m[Option[a]]]): m[t[a]] with Applicative[m] =
        Witherable.wither(identity, t)

}

///
/// A type class for types that form a commutative semigroup.
///
pub lawful class CommutativeSemiGroup[a] with SemiGroup[a] {

    ///
    /// An associative & commutative binary operation on `a`.
    ///
    pub def combine(x: a, y: a): a = SemiGroup.combine(x, y)

    law associative: forall(x: a, y: a, z: a) with Eq[a] . CommutativeSemiGroup.combine(CommutativeSemiGroup.combine(x, y), z) == CommutativeSemiGroup.combine(x, CommutativeSemiGroup.combine(y, z))

    law commutative: forall(x: a, y: a) with Eq[a] . CommutativeSemiGroup.combine(x, y) == CommutativeSemiGroup.combine(y, x)

}

///
/// Alias for `CommutativeSemiGroup.combine`.
///
pub def |+|(x: a, y: a): a with CommutativeSemiGroup[a] = CommutativeSemiGroup.combine(x, y)

instance CommutativeSemiGroup[Unit]

instance CommutativeSemiGroup[Int8]

instance CommutativeSemiGroup[Int16]

instance CommutativeSemiGroup[Int32]

instance CommutativeSemiGroup[Int64]

instance CommutativeSemiGroup[BigInt]

instance CommutativeSemiGroup[Float32]

instance CommutativeSemiGroup[Float64]

instance CommutativeSemiGroup[(a1, a2)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2]

instance CommutativeSemiGroup[(a1, a2, a3)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2], CommutativeSemiGroup[a3]

instance CommutativeSemiGroup[(a1, a2, a3, a4)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2], CommutativeSemiGroup[a3], CommutativeSemiGroup[a4]

instance CommutativeSemiGroup[(a1, a2, a3, a4, a5)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2], CommutativeSemiGroup[a3], CommutativeSemiGroup[a4], CommutativeSemiGroup[a5]

instance CommutativeSemiGroup[(a1, a2, a3, a4, a5, a6)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2], CommutativeSemiGroup[a3], CommutativeSemiGroup[a4], CommutativeSemiGroup[a5], CommutativeSemiGroup[a6]

instance CommutativeSemiGroup[(a1, a2, a3, a4, a5, a6, a7)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2], CommutativeSemiGroup[a3], CommutativeSemiGroup[a4], CommutativeSemiGroup[a5], CommutativeSemiGroup[a6], CommutativeSemiGroup[a7]

instance CommutativeSemiGroup[(a1, a2, a3, a4, a5, a6, a7, a8)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2], CommutativeSemiGroup[a3], CommutativeSemiGroup[a4], CommutativeSemiGroup[a5], CommutativeSemiGroup[a6], CommutativeSemiGroup[a7], CommutativeSemiGroup[a8]

instance CommutativeSemiGroup[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2], CommutativeSemiGroup[a3], CommutativeSemiGroup[a4], CommutativeSemiGroup[a5], CommutativeSemiGroup[a6], CommutativeSemiGroup[a7], CommutativeSemiGroup[a8], CommutativeSemiGroup[a9]

instance CommutativeSemiGroup[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with CommutativeSemiGroup[a1], CommutativeSemiGroup[a2], CommutativeSemiGroup[a3], CommutativeSemiGroup[a4], CommutativeSemiGroup[a5], CommutativeSemiGroup[a6], CommutativeSemiGroup[a7], CommutativeSemiGroup[a8], CommutativeSemiGroup[a9], CommutativeSemiGroup[a10]

///
/// The Result type.
///
/// A result represents a successful value or an error value.
/// The constructor `Ok(v)` represents the successful value `v`,
/// whereas the constructor `Err(v)` represents the error value `v`.
///
pub enum Result[t, e] with Eq, Order, ToString {
    case Ok(t),
    case Err(e)
}

instance Hash[Result[t, e]] with Hash[t], Hash[e] {
    pub def hash(r: Result[t, e]): Int32 = match r {
        case Ok(v)  => 5381 + 113 * Hash.hash(v)
        case Err(v) => 5351 + 97 * Hash.hash(v)
    }
}

namespace Result {

    ///
    /// Returns `true` iff `r` is `Ok(v)`.
    ///
    @Time(1) @Space(1)
    pub def isOk(r: Result[t, e]): Bool = match r {
        case Ok(_)  => true
        case Err(_) => false
    }

    ///
    /// Returns `true` iff `r` is `Err(w)`.
    ///
    @Time(1) @Space(1)
    pub def isErr(r: Result[t, e]): Bool = match r {
        case Ok(_)  => false
        case Err(_) => true
    }

    ///
    /// Returns `v` if `r` is `Ok(v)`. Otherwise returns `d`.
    ///
    @Time(1) @Space(1)
    pub def getWithDefault(d: t, r: Result[t, e]): t = match r {
        case Ok(v)  => v
        case Err(_) => d
    }

    ///
    /// Returns `Ok(v)` if `r` is `Ok(v)`. Otherwise returns `default`.
    ///
    @Time(1) @Space(1)
    pub def withDefault(default: {default = Result[t, e2]}, r: Result[t, e1]): Result[t, e2] = match r {
        case Ok(v)  => Ok(v)
        case Err(_) => default.default
    }

    ///
    /// Returns `Ok(to)` if `r` is `Ok(from)`. Otherwise returns `r`.
    ///
    @Time(1) @Space(1)
    pub def replace(from: {from = t}, to: {to = t}, r: Result[t, e]): Result[t, e] with Eq[t] = match r {
        case Ok(v)  => Ok(if (v == from.from) to.to else v)
        case Err(_) => r
    }

    ///
    /// Returns `true` if `r` is `Ok(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `false`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def exists(f: t -> Bool \ ef, r: Result[t, e]): Bool \ ef = match r {
        case Ok(t)  => f(t)
        case Err(_) => false
    }

    ///
    /// Returns `true` if `r` is `Ok(v)` and the predicate `f(v)` evaluates to `true` or if `r` is `Err(w)`.
    /// Otherwise returns `false`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def forall(f: t -> Bool \ ef, r: Result[t, e]): Bool \ ef = match r {
        case Ok(t)  => f(t)
        case Err(_) => true
    }

    ///
    /// Returns `Ok(f(v))` if `r` is `Ok(v)`. Returns `Err(w)` if `r` is `Err(w)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def map(f: t1 -> t2 \ ef, r: Result[t1, e]): Result[t2, e] \ ef = match r {
        case Ok(v)  => Ok(f(v))
        case Err(w) => Err(w)
    }

    ///
    /// Returns `Err(f(e))` if `r` is `Err(e)`. Returns `Ok(v)` if `r` is `Ok(v)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def mapErr(f: e1 -> e2 \ ef, r: Result[t, e1]): Result[t, e2] \ ef = match r {
        case Ok(v)  => Ok(v)
        case Err(w) => Err(f(w))
    }


    ///
    /// Returns `f(v)` if `r` is `Ok(v)`. Returns `Err(w)` if `r` is `Err(w)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def flatMap(f: t1 -> Result[t2, e] \ ef, r: Result[t1, e]): Result[t2, e] \ ef = match r {
        case Ok(v)  => f(v)
        case Err(w) => Err(w)
    }

    ///
    /// Returns `1` if `r` is `Ok(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `0`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def count(f: t -> Bool \ ef, r: Result[t, e]): Int32 \ ef = match r {
        case Ok(v)  => if (f(v)) 1 else 0
        case Err(_) => 0
    }

    ///
    /// Returns `v` if `r` is `Ok(v)` else `0`.
    ///
    pub def sum(r: Result[Int32, e]): Int32 =
        foldLeft((acc, x) -> acc + x, 0, r)

    ///
    /// Returns `f(v)` if `r` is `Ok(v)` else `0`.
    ///
    pub def sumWith(f: t -> Int32 \ ef, r: Result[t, e]): Int32 \ ef =
        foldLeft((acc, x) -> acc + f(x), 0, r)

    ///
    /// Returns `v` if `r` is `Ok(v)` else `0`.
    ///
    pub def product(r: Result[Int32, e]): Int32 =
        if (isErr(r))
            1
        else
            foldLeft((acc, x) -> acc * x, 1, r)

    ///
    /// Returns `f(v)` if `r` is `Ok(v)` else `0`.
    ///
    pub def productWith(f: t -> Int32 \ ef, r: Result[t, e]): Int32 \ ef =
        if (isErr(r))
            1
        else
            foldLeft((acc, x) -> acc * f(x), 1, r)

    ///
    /// Returns `Some(v)` if `r` is `Ok(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `None`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f)) @Space(space(f))
    pub def find(f: t -> Bool, r: Result[t, e]): Option[t] = match r {
        case Ok(v)  => if (f(v)) Some(v) else None
        case Err(_) => None
    }

    ///
    /// Returns `f(z, v)` if `r` is `Ok(v)`. Otherwise returns `z`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def foldLeft(f: (a, t) -> a \ ef, z: a, r: Result[t, e]): a \ ef = match r {
        case Ok(v)  => f(z, v)
        case Err(_) => z
    }

    ///
    /// Returns `f(v, z)` if `r` is `Ok(v)`. Otherwise returns `z`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def foldRight(f: (t, a) -> a \ ef, z: a, r: Result[t, e]): a \ ef = match r {
        case Ok(v)  => f(v, z)
        case Err(_) => z
    }

    ///
    /// Returns `f(v, z)` if `r` is `Ok(v)`. Otherwise returns `z`.
    ///
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    @Time(time(f)) @Space(space(f))
    pub def foldRightWithCont(f: (t, Unit -> a \ ef) -> a \ ef, z: a, r: Result[t, e]): a \ ef = match r {
        case Ok(v)  => f(v, upcast(_ -> z))
        case Err(_) => z
    }

    ///
    /// Returns `Ok(v1 :: v2 :: ... :: vn)` if each of `l_i` is `Ok(v_i)`.
    /// Otherwise returns the first `Err` encountered.
    ///
    pub def sequence(l: List[Result[a, e]]): Result[List[a], e] =
        def loop(ll, k) = match ll {
            case Nil         => k(Nil)
            case Err(e) :: _ => Err(e)
            case Ok(x) :: xs => loop(xs, ks -> k(x :: ks))
        };
        loop(l, ks -> Ok(ks))

    ///
    /// Returns `Some(v1 :: v2 :: ... v :: vn)` if each of `f(l_i)` is `Ok(v_i)`.
    /// Otherwise returns the first `Err` encountered.
    ///
    pub def traverse(f: a -> Result[b, e] \ ef, l: List[a]): Result[List[b], e] \ ef =
        def loop(ll, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs => match f(x) {
                case Ok(y)  => loop(xs, ks -> k(y :: ks))
                case Err(e) => Err(e)
            }
        };
        loop(l, ks -> Ok(ks))

    ///
    /// Returns `Ok()` if each of `f(l_i)` is `Ok(_)`. Otherwise returns `None`.
    ///
    /// This function is the "forgetful" version of `traverse`, use it when the you want the effect
    /// of applying `f` to each element but do not care about collecting the results.
    ///
    pub def traverseX(f: a -> Result[b, e] \ ef, l: List[a]): Result[Unit, e] \ ef = match l {
        case Nil     => Ok()
        case x :: xs => match f(x) {
            case Ok(_)  => traverseX(f, xs)
            case Err(e) => Err(e)
        }
    }

    ///
    /// Returns the result of applying `f` to a start value `s` and the elements in `l`
    /// going from left to right.
    ///
    /// If at any step applying `f` fails (i.e. it produces a `Err(e)` value) the traversal
    /// of `l` is short-circuited and `Err(e)` is returned.
    ///
    /// If `f` is successfully applied to all elements in `l` the result is of the form:
    /// `Ok(f(...f(f(s, x1), x2)..., xn))`.
    ///
    pub def foldLeftM(f: (b, a) -> Result[b, e] \ ef, s: b, l: List[a]): Result[b, e] \ ef = match l {
        case Nil     => Ok(s)
        case x :: xs => match f(s, x) {
            case Ok(s1) => foldLeftM(f, s1, xs)
            case Err(e) => Err(e)
        }
    }

    ///
    /// Returns a one-element list of the value `v` if `r` is `Ok(v)`. Otherwise returns the empty list.
    ///
    @Time(1) @Space(1)
    pub def toList(r: Result[t, e]): List[t] = match r {
        case Ok(v)  => v :: Nil
        case Err(_) => Nil
    }

    ///
    /// Returns `Some(v)` if `r` is `Ok(v)`. Otherwise returns `None`.
    ///
    @Time(1) @Space(1)
    pub def toOption(r: Result[t, e]): Option[t] = match r {
        case Ok(v)  => Some(v)
        case Err(_) => None
    }

    ///
    /// Applies `f` to `v` if `r` is `Ok(v)`. Otherwise does nothing.
    ///
    @Time(time(f)) @Space(space(f))
    pub def foreach(f: t -> Unit \ ef, r: Result[t, e]): Unit \ ef = match r {
        case Ok(v)  => f(v)
        case Err(_) => ()
    }

    ///
    /// Applies the function in `r1` to the value in `r2`.
    ///
    @Time(foldLeft((_, f) -> time(f), 1, r1)) @Space(foldLeft((_, f) -> time(f), 1, r1))
    pub def ap(r1: Result[t -> u \ ef, e], r2: Result[t, e]): Result[u, e] \ ef = match r1 {
        case Err(e) => Err(e)
        case Ok(f)  => match r2 {
            case Ok(a)  => Ok(f(a))
            case Err(e) => Err(e)
        }
    }

    ///
    /// Applies the binary function `f` to the values in `r1` and `r2`.
    ///
    /// Returns the first `Err(e)` value if either of `r1` and `r2` are `Err(e)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift2(f: (t1, t2) -> u \ ef, r1: Result[t1, e], r2: Result[t2, e]): Result[u, e] \ ef =
        ap(map(f, r1), r2)

    ///
    /// Applies the ternary function `f` to the values in `r1`, `r2` and `r3`.
    ///
    /// Returns the first `Err(e)` value if any of `r1`, `r2` and `r3` are `Err(e)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift3(f: (t1, t2, t3) -> u \ ef, r1: Result[t1, e], r2: Result[t2, e], r3: Result[t3, e]): Result[u, e] \ ef =
        ap(lift2(f, r1, r2), r3)

    ///
    /// Applies the 4-ary function `f` to the values in `r1`, `r2`, `r3` and `r4`.
    ///
    /// Returns the first `Err(e)` value if any of `r1`, `r2`, `r3` and `r4` are `Err(e)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift4(f: (t1, t2, t3, t4) -> u \ ef, r1: Result[t1, e], r2: Result[t2, e], r3: Result[t3, e], r4: Result[t4, e]): Result[u, e] \ ef =
        ap(lift3(f, r1, r2, r3), r4)

    ///
    /// Applies the 5-ary function `f` to the values in `r1`, `r2`, ... `r5`.
    ///
    /// Returns the first `Err(e)` value if any of `r1`, `r2`, ... `r5` are `Err(e)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift5(f: (t1, t2, t3, t4, t5) -> u \ ef, r1: Result[t1, e], r2: Result[t2, e], r3: Result[t3, e], r4: Result[t4, e], r5: Result[t5, e]): Result[u, e] \ ef =
        ap(lift4(f, r1, r2, r3, r4), r5)

    ///
    /// Applies the 6-ary function `f` to the values in `r1`, `r2`, ... `r6`.
    ///
    /// Returns the first `Err(e)` value if any of `r1`, `r2`, ... `r6` are `Err(e)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift6(f: (t1, t2, t3, t4, t5, t6) -> u \ ef, r1: Result[t1, e], r2: Result[t2, e], r3: Result[t3, e], r4: Result[t4, e], r5: Result[t5, e], r6: Result[t6, e]): Result[u, e] \ ef =
        ap(lift5(f, r1, r2, r3, r4, r5), r6)

    ///
    /// Applies the 7-ary function `f` to the values in `r1`, `r2`, ... `r7`.
    ///
    /// Returns the first `Err(e)` value if any of `r1`, `r2`, ... `r7` are `Err(e)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift7(f: (t1, t2, t3, t4, t5, t6, t7) -> u \ ef, r1: Result[t1, e], r2: Result[t2, e], r3: Result[t3, e], r4: Result[t4, e], r5: Result[t5, e], r6: Result[t6, e], r7: Result[t7, e]): Result[u, e] \ ef=
        ap(lift6(f, r1, r2, r3, r4, r5, r6), r7)

    ///
    /// Applies the 8-ary function `f` to the values in `r1`, `r2`, ... `r8`.
    ///
    /// Returns the first `Err(e)` value if any of `r1`, `r2`, ... `r8` are `Err(e)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift8(f: (t1, t2, t3, t4, t5, t6, t7, t8) -> u \ ef, r1: Result[t1, e], r2: Result[t2, e], r3: Result[t3, e], r4: Result[t4, e], r5: Result[t5, e], r6: Result[t6, e], r7: Result[t7, e], r8: Result[t8, e]): Result[u, e] \ ef =
        ap(lift7(f, r1, r2, r3, r4, r5, r6, r7), r8)

    ///
    /// Applies the 9-ary function `f` to the values in `r1`, `r2`, ... `r9`.
    ///
    /// Returns the first `Err(e)` value if any of `r1`, `r2`, ... `r9` are `Err(e)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift9(f: (t1, t2, t3, t4, t5, t6, t7, t8, t9) -> u \ ef, r1: Result[t1, e], r2: Result[t2, e], r3: Result[t3, e], r4: Result[t4, e], r5: Result[t5, e], r6: Result[t6, e], r7: Result[t7, e], r8: Result[t8, e], r9: Result[t9, e]): Result[u, e] \ ef =
        ap(lift8(f, r1, r2, r3, r4, r5, r6, r7, r8), r9)

    ///
    /// Applies the 10-ary function `f` to the values in `r1`, `r2`, ... `r10`.
    ///
    /// Returns the first `Err(e)` value if any of `r1`, `r2`, ... `r10` are `Err(e)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift10(f: (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) -> u \ ef, r1: Result[t1, e], r2: Result[t2, e], r3: Result[t3, e], r4: Result[t4, e], r5: Result[t5, e], r6: Result[t6, e], r7: Result[t7, e], r8: Result[t8, e], r9: Result[t9, e], r10: Result[t10, e]): Result[u, e] \ ef =
        ap(lift9(f, r1, r2, r3, r4, r5, r6, r7, r8, r9), r10)

    ///
    /// Returns `Ok(x)` if `f` was invoked without throwing an exception.
    ///
    /// If `f` throws a Java `RuntimeException`, `Err(e)` is returned
    /// where `e` is the error message.
    ///
    pub def try(f: Unit -> a \ ef): Result[a, String] \ ef =
        try {
            Ok(f())
        } catch {
            case e: ##java.lang.RuntimeException =>
                import java.lang.Throwable.getMessage(): String \ {};
                Err(getMessage(e))
        }

}

///
/// A type class for partially ordered types that have an upper bound.
///
pub class UpperBound[a] {
    ///
    /// Returns the largest value of `a`.
    ///
    pub def maxValue(): a
}

instance UpperBound[(a1, a2)] with UpperBound[a1], UpperBound[a2] {
    pub def maxValue(): (a1, a2) = (UpperBound.maxValue(), UpperBound.maxValue())
}

instance UpperBound[(a1, a2, a3)] with UpperBound[a1], UpperBound[a2], UpperBound[a3] {
    pub def maxValue(): (a1, a2, a3) = (UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue())
}

instance UpperBound[(a1, a2, a3, a4)] with UpperBound[a1], UpperBound[a2], UpperBound[a3], UpperBound[a4] {
    pub def maxValue(): (a1, a2, a3, a4) = (UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue())
}

instance UpperBound[(a1, a2, a3, a4, a5)] with UpperBound[a1], UpperBound[a2], UpperBound[a3], UpperBound[a4], UpperBound[a5] {
    pub def maxValue(): (a1, a2, a3, a4, a5) = (UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue())
}

instance UpperBound[(a1, a2, a3, a4, a5, a6)] with UpperBound[a1], UpperBound[a2], UpperBound[a3], UpperBound[a4], UpperBound[a5], UpperBound[a6] {
    pub def maxValue(): (a1, a2, a3, a4, a5, a6) = (UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue())
}

instance UpperBound[(a1, a2, a3, a4, a5, a6, a7)] with UpperBound[a1], UpperBound[a2], UpperBound[a3], UpperBound[a4], UpperBound[a5], UpperBound[a6], UpperBound[a7] {
    pub def maxValue(): (a1, a2, a3, a4, a5, a6, a7) = (UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue())
}

instance UpperBound[(a1, a2, a3, a4, a5, a6, a7, a8)] with UpperBound[a1], UpperBound[a2], UpperBound[a3], UpperBound[a4], UpperBound[a5], UpperBound[a6], UpperBound[a7], UpperBound[a8] {
    pub def maxValue(): (a1, a2, a3, a4, a5, a6, a7, a8) = (UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue())
}

instance UpperBound[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with UpperBound[a1], UpperBound[a2], UpperBound[a3], UpperBound[a4], UpperBound[a5], UpperBound[a6], UpperBound[a7], UpperBound[a8], UpperBound[a9] {
    pub def maxValue(): (a1, a2, a3, a4, a5, a6, a7, a8, a9) = (UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue())
}

instance UpperBound[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with UpperBound[a1], UpperBound[a2], UpperBound[a3], UpperBound[a4], UpperBound[a5], UpperBound[a6], UpperBound[a7], UpperBound[a8], UpperBound[a9], UpperBound[a10] {
    pub def maxValue(): (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = (UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue())
}

instance UpperBound[Unit] {
    pub def maxValue(): Unit = ()
}

///
/// A type class for bitwise left shift.
///
pub class BitwiseShl[a] {
    ///
    /// Returns `x` shifted left by `n` bits.
    ///
    pub def shl(x: a, n: Int32): a
}

instance BitwiseShl[Int8] {
    pub def shl(x: Int8, n: Int32): Int8 = $INT8_SHL$(x, n)
}

instance BitwiseShl[Int16] {
    pub def shl(x: Int16, n: Int32): Int16 = $INT16_SHL$(x, n)
}

instance BitwiseShl[Int32] {
    pub def shl(x: Int32, n: Int32): Int32 = $INT32_SHL$(x, n)
}

instance BitwiseShl[Int64] {
    pub def shl(x: Int64, n: Int32): Int64 = $INT64_SHL$(x, n)
}

instance BitwiseShl[BigInt] {
    pub def shl(x: BigInt, n: Int32): BigInt = $BIGINT_SHL$(x, n)
}

///
/// A type class for bitwise not.
///
pub class BitwiseNot[a] {
    ///
    /// Returns the bitwise NOT of `x`.
    ///
    pub def not(x: a): a
}

instance BitwiseNot[Int8] {
    pub def not(x: Int8): Int8 = $INT8_NOT$(x)
}

instance BitwiseNot[Int16] {
    pub def not(x: Int16): Int16 = $INT16_NOT$(x)
}

instance BitwiseNot[Int32] {
    pub def not(x: Int32): Int32 = $INT32_NOT$(x)
}

instance BitwiseNot[Int64] {
    pub def not(x: Int64): Int64 = $INT64_NOT$(x)
}

instance BitwiseNot[BigInt] {
    pub def not(x: BigInt): BigInt = $BIGINT_NOT$(x)
}

///
/// A type class for bitwise right shift.
///
pub class BitwiseShr[a] {
    ///
    /// Returns `x` shifted right by `n` bits.
    ///
    pub def shr(x: a, n: Int32): a
}

instance BitwiseShr[Int8] {
    pub def shr(x: Int8, n: Int32): Int8 = $INT8_SHR$(x, n)
}

instance BitwiseShr[Int16] {
    pub def shr(x: Int16, n: Int32): Int16 = $INT16_SHR$(x, n)
}

instance BitwiseShr[Int32] {
    pub def shr(x: Int32, n: Int32): Int32 = $INT32_SHR$(x, n)
}

instance BitwiseShr[Int64] {
    pub def shr(x: Int64, n: Int32): Int64 = $INT64_SHR$(x, n)
}

instance BitwiseShr[BigInt] {
    pub def shr(x: BigInt, n: Int32): BigInt = $BIGINT_SHR$(x, n)
}

///
/// A type class for bitwise or.
///
pub class BitwiseOr[a] {
    ///
    /// Returns the bitwise OR of `x` and `y`.
    ///
    pub def or(x: a, y: a): a
}

instance BitwiseOr[Int8] {
    pub def or(x: Int8, y: Int8): Int8 = $INT8_OR$(x, y)
}

instance BitwiseOr[Int16] {
    pub def or(x: Int16, y: Int16): Int16 = $INT16_OR$(x, y)
}

instance BitwiseOr[Int32] {
    pub def or(x: Int32, y: Int32): Int32 = $INT32_OR$(x, y)
}

instance BitwiseOr[Int64] {
    pub def or(x: Int64, y: Int64): Int64 = $INT64_OR$(x, y)
}

instance BitwiseOr[BigInt] {
    pub def or(x: BigInt, y: BigInt): BigInt = $BIGINT_OR$(x, y)
}

///
/// A type class for bitwise and.
///
pub class BitwiseAnd[a] {
    ///
    /// Returns the bitwise AND of `x` and `y`.
    ///
    pub def and(x: a, y: a): a
}

instance BitwiseAnd[Int8] {
    pub def and(x: Int8, y: Int8): Int8 = $INT8_AND$(x, y)
}

instance BitwiseAnd[Int16] {
    pub def and(x: Int16, y: Int16): Int16 = $INT16_AND$(x, y)
}

instance BitwiseAnd[Int32] {
    pub def and(x: Int32, y: Int32): Int32 = $INT32_AND$(x, y)
}

instance BitwiseAnd[Int64] {
    pub def and(x: Int64, y: Int64): Int64 = $INT64_AND$(x, y)
}

instance BitwiseAnd[BigInt] {
    pub def and(x: BigInt, y: BigInt): BigInt = $BIGINT_AND$(x, y)
}

///
/// A type class for bitwise xor.
///
pub class BitwiseXor[a] {
    ///
    /// Returns the bitwise XOR of `x` and `y`.
    ///
    pub def xor(x: a, y: a): a
}

instance BitwiseXor[Int8] {
    pub def xor(x: Int8, y: Int8): Int8 = $INT8_XOR$(x, y)
}

instance BitwiseXor[Int16] {
    pub def xor(x: Int16, y: Int16): Int16 = $INT16_XOR$(x, y)
}

instance BitwiseXor[Int32] {
    pub def xor(x: Int32, y: Int32): Int32 = $INT32_XOR$(x, y)
}

instance BitwiseXor[Int64] {
    pub def xor(x: Int64, y: Int64): Int64 = $INT64_XOR$(x, y)
}

instance BitwiseXor[BigInt] {
    pub def xor(x: BigInt, y: BigInt): BigInt = $BIGINT_XOR$(x, y)
}


instance LowerBound[Int32] {
    pub def minValue(): Int32 = Int32.minValue()
}

instance UpperBound[Int32] {
    pub def maxValue(): Int32 = Int32.maxValue()
}

namespace Int32 {

    ///
    /// Returns the number of bits used to represent an `Int32`.
    ///
    @Time(1) @Space(1)
    pub def size(): Int32 = 32

    ///
    /// Returns the minimum number representable by an `Int32`.
    ///
    @Time(1) @Space(1)
    pub def minValue(): Int32 = 1 <<< (size() - 1)

    ///
    /// Returns the maximum number representable by an `Int32`.
    ///
    @Time(1) @Space(1)
    pub def maxValue(): Int32 = ~~~minValue()

    ///
    /// Returns the smaller of `x` and `y`.
    ///
    @Time(1) @Space(1)
    pub def min(x: Int32, y: Int32): Int32 = if (x <= y) x else y

    ///
    /// Returns the larger of `x` and `y`.
    ///
    @Time(1) @Space(1)
    pub def max(x: Int32, y: Int32): Int32 = if (x >= y) x else y

    ///
    /// Returns the absolute value of `x`.
    /// If the absolute value exceeds maxValue(), -1 is returned.
    ///
    @Time(1) @Space(1)
    pub def abs(x: Int32): Int32 = {
        if      (x >= 0)            x
        else if (x == minValue())   -1
        else                        -x
    }

    ///
    /// Returns the distance between `x` and `y`.
    /// If this distance exceeds maxValue(), -1 is returned.
    ///
    @Time(1) @Space(1)
    pub def dist(x: Int32, y: Int32): Int32 = {
        if      (x >= 0 and y >= 0)                      abs(x - y)
        else if (x < 0 and y < 0)                        abs(x - y)
        else if (x == minValue() or y == minValue())    -1
        else if (minValue() + abs(x) + abs(y) >= 0)     -1
        else                                            abs(x - y)
    }

    ///
    /// Returns 1 if x > y, -1 if x < y, and 0 if x = y.
    /// The sign of x - y.
    ///
    @Time(1) @Space(1)
    pub def compare(x: Int32, y: Int32): Int32 = {
        if      (x == y)    0
        else if (x < y)     -1
        else                1
    }

    ///
    /// Returns 1 if x > 0, -1 if x < 0, and 0 if x = 0.
    /// The sign of x.
    ///
    @Time(1) @Space(1)
    pub def signum(x: Int32): Int32 = compare(x, 0)

    ///
    /// Returns the logical right shift of `x` by `dist`.
    /// Only the rightmost 5 bits of `dist` are considered (ie. `dist rem 32`).
    /// A zero is shifted into the leftmost position regardless of sign extension.
    ///
    @Time(1) @Space(1)
    pub def logicalRightShift(dist: {dist = Int32}, x: Int32): Int32 =
        if (x < 0 and dist.dist rem size() != 0)
            ((x >>> 1) &&& maxValue()) >>> (dist.dist - 1)
        else
            x >>> dist.dist

    ///
    /// Returns the number of one-bits in the two's complement binary
    /// representation of `x`.
    ///
    @Time(1) @Space(1)
    pub def bitCount(x: Int32): Int32 = {
        if      (x == 0)       0
        else if (x rem 2 != 0) bitCount(logicalRightShift(dist = 1, x)) + 1
        else                   bitCount(logicalRightShift(dist = 1, x))
    }

    ///
    /// Returns the the value obtained by rotating the two's complement
    /// binary representation of `x` right by `dist` bits.
    ///
    @Time(1) @Space(1)
    pub def rotateRight(dist: {dist = Int32}, x: Int32): Int32 =
        (logicalRightShift(dist = dist.dist, x)) ||| (x <<< -dist.dist)

    ///
    /// Returns the the value obtained by rotating the two's complement
    /// binary representation of `x` left by `dist` bits.
    ///
    @Time(1) @Space(1)
    pub def rotateLeft(dist: {dist = Int32}, x: Int32): Int32 =
        (logicalRightShift(dist = -dist.dist, x)) ||| (x <<< dist.dist)

    ///
    /// Returns the value obtained by reversing the bits in the
    /// two's complement binary representation of `x`.
    ///
    @Time(1) @Space(1)
    pub def reverse(x: Int32): Int32 = reverseHelper(x, 0, size()-1)

    ///
    /// Helper function for `reverse`.
    ///
    def reverseHelper(x: Int32, l: Int32, r: Int32): Int32 =
        if (l >= r) x else reverseHelper(swap(x, l, r), l+1, r-1)

    ///
    /// Helper function for `reverse`.
    ///
    @Time(1) @Space(1)
    def swap(x: Int32, l: Int32, r: Int32): Int32 = match (getBit(pos = l, x), getBit(pos = r, x)) {
        case (1, 0) => clearBit(pos = l, setBit(pos = r, x))
        case (0, 1) => clearBit(pos = r, setBit(pos = l, x))
        case _ => x
    }

    ///
    /// Returns the position of the highest-order/leftmost one-bit in `x`.
    /// Possible return values: 0 (rightmost bit) - 31 (leftmost bit)
    ///                         -1 if x = 0
    ///
    @Time(1) @Space(1)
    pub def highestOneBitPosition(x: Int32): Int32 =
        // Start at bit 31 and scan right
        oneBitPositionHelper(x, size() - 1, -1)

    ///
    /// Returns the position of the lowest-order/rightmost one-bit in `x`.
    /// Possible return values: 0 (rightmost bit) - 31 (leftmost bit)
    ///                         -1 if x = 0
    ///
    @Time(1) @Space(1)
    pub def lowestOneBitPosition(x: Int32): Int32 =
        // Start at bit 0 and scan left
        oneBitPositionHelper(x, 0, 1)

    ///
    /// Helper function for highestOneBitPosition and lowestOneBitPosition.
    /// If `x` has a one-bit at `position`, return that position.
    /// Otherwise recursively check the next bit in the same way.
    ///
    def oneBitPositionHelper(x: Int32, position: Int32, delta: Int32): Int32 = {
        if      (position < 0)                   -1
        else if (position > size() - 1)          -1
        else if (getBit(pos = position, x) == 1) position
        else if (delta == 0)                     -1
        else                                     oneBitPositionHelper(x, position + delta, delta)
    }

    ///
    /// Returns a value with at most a single one-bit, in the position
    /// of the highest-order/leftmost one-bit in `x`.
    /// Returns 0 if x=0.
    ///
    @Time(1) @Space(1)
    pub def highestOneBit(x: Int32): Int32 =
        bitPositionToInt32(highestOneBitPosition(x))

    ///
    /// Returns a value with at most a single one-bit, in the position
    /// of the highest-order/leftmost one-bit in `x`.
    /// Returns 0 if x=0.
    ///
    @Time(1) @Space(1)
    pub def lowestOneBit(x: Int32): Int32 =
        bitPositionToInt32(lowestOneBitPosition(x))

    ///
    /// Helper function for highestOneBit and lowestOneBit.
    /// Returns a value with a single one-bit at bit number `position`.
    /// Returns 0 if `position` is outside the range 0-31 inclusive.
    ///
    @Time(1) @Space(1)
    def bitPositionToInt32(position: Int32): Int32 =
        if (position < 0 or position > size() - 1) 0 else 1 <<< position

    ///
    /// Returns the number of zero bits preceding the
    /// highest-order/leftmost one-bit in `x`.
    /// Returns 32 if x=0.
    ///
    @Time(1) @Space(1)
    pub def numberOfLeadingZeros(x: Int32): Int32 =
        size() - 1 - highestOneBitPosition(x)

    ///
    /// Returns the number of zero bits following the
    /// lowest-order/rightmost one-bit in `x`.
    /// Returns 32 if x=0.
    ///
    @Time(1) @Space(1)
    pub def numberOfTrailingZeros(x: Int32): Int32 =
        if (x == 0) 32 else lowestOneBitPosition(x)

    ///
    /// Returns the bit of `x` at position `pos` (either 0 or 1).
    /// Considers the 5 rightmost bits of `pos` (`pos` mod 32).
    /// The bits of x have positions: 0 (rightmost bit) - 31 (leftmost bit)
    ///
    @Time(1) @Space(1)
    pub def getBit(pos: {pos = Int32}, x: Int32): Int32 =
        if ((x >>> pos.pos) rem 2 == 0) 0 else 1

    ///
    /// Returns `x` with the bit at position `pos` set (to 1).
    /// Considers the 5 rightmost bits of `pos` (`pos` mod 32).
    /// The bits of x have positions: 0 (rightmost bit) - 31 (leftmost bit)
    ///
    @Time(1) @Space(1)
    pub def setBit(pos: {pos = Int32}, x: Int32): Int32 = x ||| (1 <<< pos.pos)

    ///
    /// Returns `x` with the bit at position `pos` cleared (to 0).
    /// Considers the 5 rightmost bits of `pos` (`pos` mod 32).
    /// The bits of x have positions: 0 (rightmost bit) - 31 (leftmost bit)
    ///
    @Time(1) @Space(1)
    pub def clearBit(pos: {pos = Int32}, x: Int32): Int32 = x &&& ~~~(1 <<< pos.pos)

    ///
    /// Returns `x` with the bit at position `pos` flipped.
    /// Considers the 5 rightmost bits of `pos` (`pos` mod 32).
    /// The bits of x have positions: 0 (rightmost bit) - 31 (leftmost bit)
    ///
    @Time(1) @Space(1)
    pub def flipBit(pos: {pos = Int32}, x: Int32): Int32 = x ^^^ (1 <<< pos.pos)

    ///
    /// Returns the integer binary logarithm of `x`.
    /// If the given value is 0 or negative, 0 is returned.
    ///
    @Time(1) @Space(1)
    pub def log2(x: Int32): Int32 =
        if (x <= 0) {
            0
        } else {
            highestOneBitPosition(x)
        }

    ///
    /// Returns the factorial of `x`.
    /// If the given value is negative, 0 is returned.
    ///
    @Time(factorial(x)) @Space(1)
    pub def factorial(x: Int32): Int32 =
        if (x < 0) {
            0
        } else {
            def loop(y, acc) = match y {
                case 0 => acc
                case _ => loop(y - 1, y * acc)
            };
            loop(x, 1)
        }

    ///
    /// Return a string representation of `x`.
    ///
    @Time(1) @Space(1)
    pub def toString(x: Int32): String = ToString.toString(x)

    ///
    /// Parse the string `s` as an Int32, leading or trailing whitespace is trimmed.
    /// A successful parse is wrapped with `Some(x)`, a parse failure is indicated by `None`.
    ///
    @Time(1) @Space(1)
    pub def fromString(s: String): Option[Int32] = try {
        import java.lang.String.strip(): String \ {};
        import static java.lang.Integer.parseInt(String): Int32 \ {};
        Some(s |> strip |> parseInt)
    } catch {
        case _: ##java.lang.NumberFormatException => None
    }

    ///
    /// Parse the string `s` as an Int32, where the `radix` is used while parsing.
    /// Leading or trailing whitespace is trimmed.
    /// A successful parse is wrapped with `Ok(x)`, a parse failure is indicated by `Err(_)`.
    ///
    pub def parse(radix: Int32, s: String): Result[Int32, String] =
        try {
            import java.lang.String.strip(): String \ {};
            import static java.lang.Integer.parseInt(String, Int32): Int32 \ {};
            Ok(parseInt(strip(s), radix))
        } catch {
            case _: ##java.lang.NumberFormatException => Err("Int32.parse")
        }


    ///
    /// Convert `x` to a Int64.
    ///
    /// The numeric value of `x` is preserved exactly.
    ///
    @Time(1) @Space(1)
    pub def toInt64(x: Int32): Int64 =
        import static java.lang.Integer.valueOf(Int32): ##java.lang.Integer \ {};
        import java.lang.Integer.longValue(): Int64 \ {};
        (valueOf(x) |> longValue)

    ///
    /// Convert `x` to a BigInt.
    ///
    /// The numeric value of `x` is preserved exactly.
    ///
    @Time(1) @Space(1)
    pub def toBigInt(x: Int32): BigInt =
        import static java.lang.Integer.valueOf(Int32): ##java.lang.Integer \ {} as i16ValueOf;
        import java.lang.Integer.longValue(): Int64 \ {};
        import static java.math.BigInteger.valueOf(Int64): BigInt \ {} as asBigInt;
        (i16ValueOf(x) |> longValue |> asBigInt)

    ///
    /// Convert `x` to an Float32.
    ///
    /// The numeric value of `x` may lose precision.
    ///
    @Time(1) @Space(1)
    pub def toFloat32(x: Int32): Float32 =
        import static java.lang.Integer.valueOf(Int32): ##java.lang.Integer \ {};
        import java.lang.Integer.floatValue(): Float32 \ {};
        (valueOf(x) |> floatValue)

    ///
    /// Convert `x` to a Float64.
    ///
    /// The numeric value of `x` is preserved exactly.
    ///
    @Time(1) @Space(1)
    pub def toFloat64(x: Int32): Float64 =
        import static java.lang.Integer.valueOf(Int32): ##java.lang.Integer \ {};
        import java.lang.Integer.doubleValue(): Float64 \ {};
        (valueOf(x) |> doubleValue)

}


///
/// The NonEmptyList type.
///
pub enum Nel[a] {
    case Nel(a, List[a])
}

instance Hash[Nel[a]] with Hash[a] {
    pub def hash(l: Nel[a]): Int32 = match l {
        case Nel(x, xs) => Hash.hash(xs) `Hash.combine` Hash.hash(x)
    }
}

instance Eq[Nel[a]] with Eq[a] {
    pub def eq(l1: Nel[a], l2: Nel[a]): Bool = match (l1, l2) {
        case (Nel(x, xs), Nel(y, ys)) => x == y and xs == ys
    }
}

instance Order[Nel[a]] with Order[a] {
    ///
    /// Compares `l1` and `l2` lexicographically.
    ///
    pub def compare(l1: Nel[a], l2: Nel[a]): Comparison = match (l1, l2) {
        case (Nel(x, xs), Nel(y, ys)) =>
            let cmp = x <=> y;
            if (cmp == EqualTo) xs <=> ys else cmp
    }
}

instance Functor[Nel] {
    pub def map(f: a -> b \ ef, l: Nel[a]): Nel[b] \ ef = Nel.map(f, l)
}

instance UnorderedFoldable[Nel] {
    pub def foldMap(f: a -> b \ ef, l: Nel[a]): b \ ef with CommutativeMonoid[b] = Nel.foldMap(f, l)
    override pub def isEmpty(_: Nel[a]): Bool = false
    override pub def exists(f: a -> Bool \ ef, l: Nel[a]): Bool \ ef = Nel.exists(f, l)
    override pub def forall(f: a -> Bool \ ef, l: Nel[a]): Bool \ ef = Nel.forall(f, l)
    override pub def memberOf(x: a, l: Nel[a]): Bool with Eq[a] = Nel.memberOf(x, l)
}

instance SemiGroup[Nel[a]] {
    pub def combine(x: Nel[a], y: Nel[a]): Nel[a] = Nel.append(x, y)
}

namespace Nel {

    ///
    /// Returns a new non-empty list containing the single element `x`.
    ///
    @Time(1) @Space(1)
    pub def singleton(x: a): Nel[a] = Nel(x, Nil)

    ///
    /// Returns the non-empty list `l` prefixed with the new element `x`.
    ///
    @Time(1) @Space(1)
    pub def cons(x: a, l: Nel[a]): Nel[a] = match l {
        case Nel(y, ys) => Nel(x, y :: ys)
    }

    ///
    /// Returns the first element of `l`.
    ///
    @Time(1) @Space(1)
    pub def head(l: Nel[a]): a = match l {
        case Nel(x, _) => x
    }

    ///
    /// Returns the last element of `l`.
    ///
    @Time(length(l)) @Space(1)
    pub def last(l: Nel[a]): a = match l {
        case Nel(x, xs) => Option.getWithDefault(x, List.last(xs))
    }

    ///
    /// Returns all elements in `l` without the last element.
    ///
    @Time(length(l)) @Space(length(l))
    pub def init(l: Nel[a]): List[a] = match l {
        case Nel(_, Nil) => Nil
        case Nel(x, xs)   => match List.reverse(xs) {
            case Nil     => x :: Nil
            case _ :: ys => x :: List.reverse(ys)
        }
    }

    ///
    /// Returns all elements in `l` without the first element.
    ///
    @Time(1) @Space(1)
    pub def tail(l: Nel[a]): List[a] = match l {
        case Nel(_, xs) => xs
    }

    ///
    /// Returns the length of `l`.
    ///
    @Time(length(l)) @Space(1)
    pub def length(l: Nel[a]): Int32 = match l {
        case Nel(_, xs) => 1 + List.length(xs)
    }

    ///
    /// Returns `l2` appended to `l1`.
    ///
    @Time(length(l1)) @Space(length(l1))
    pub def append(l1: Nel[a], l2: Nel[a]): Nel[a] = match (l1, l2) {
        case (Nel(x, xs), Nel(y, ys)) => Nel(x, xs ::: (y :: ys))
    }

    ///
    /// Returns `true` if and only if `l` contains the element `a`.
    ///
    @Time(length(l)) @Space(1)
    pub def memberOf(a: a, l: Nel[a]): Bool with Eq[a] = match l {
        case Nel(x, xs) => if (x == a) true else List.memberOf(a, xs)
    }

    ///
    /// Finds the smallest element of `l` according to the `Order` on `a`.
    ///
    pub def minimum(l: Nel[a]): a with Order[a] =
        reduceLeft(Order.min, l)

    ///
    /// Finds the smallest element of `l` according to the given comparator `cmp`.
    ///
    pub def minimumBy(cmp: (a, a) -> Comparison, l: Nel[a]): a =
        reduceLeft(Order.minBy(cmp), l)

    ///
    /// Finds the largest element of `l` according to the `Order` on `a`.
    ///
    pub def maximum(l: Nel[a]): a with Order[a] =
        reduceLeft(Order.max, l)

    ///
    /// Finds the largest element of `l` according to the given comparator `cmp`.
    ///
    pub def maximumBy(cmp: (a, a) -> Comparison, l: Nel[a]): a =
        reduceLeft(Order.maxBy(cmp), l)

    ///
    /// Alias for `findLeft`.
    ///
    @Time(time(f) * length(l)) @Space(space(f))
    pub def find(f: a -> Bool \ ef, l: Nel[a]): Option[a] \ ef = findLeft(f, l)

    ///
    /// Optionally returns the first element of `l` that satisfies the predicate `f` when searching from left to right.
    ///
    @Time(time(f) * length(l)) @Space(space(f))
    pub def findLeft(f: a -> Bool \ ef, l: Nel[a]): Option[a] \ ef = match l {
        case Nel(x, xs) => if (f(x)) Some(x) else List.findLeft(f, xs)
    }

    ///
    /// Returns the result of applying `f` to every element in `l`.
    ///
    /// That is, the result is of the form: `f(x1) :: f(x2) :: ...`.
    ///
    @Time(time(f) * length(l)) @Space(space(f) * length(l))
    pub def map(f: a -> b \ ef, l: Nel[a]): Nel[b] \ ef = match l {
        case Nel(x, xs) => Nel(f(x), List.map(f, xs))
    }

    ///
    /// Returns the result of applying `f` to every element in `l` along with that element's index.
    ///
    /// That is, the result is of the form: `f(x1, 0) :: f(x2, 1) :: ...`.
    ///
    @Time(time(f) * length(l)) @Space(space(f) * length(l))
    pub def mapWithIndex(f: (Int32, a) -> b \ ef, l: Nel[a]): Nel[b] \ ef =
        let Nel(x, xs) = l;
        match List.mapWithIndex(f, x :: xs) {
            case y :: ys => Nel(y, ys)
            case _       => unreachable!()
        }

    ///
    /// Returns the result of applying `f` to every element in `l` and concatenating the results.
    ///
    @Time(time(f) * length(l)) @Space(time(f) * length(l))
    pub def flatMap(f: a -> Nel[b] \ ef, l: Nel[a]): Nel[b] \ ef = match l {
        case Nel(x, xs) => match f(x) {
            case Nel(y, ys) => Nel(y, ys ::: List.flatMap(z -> toList(f(z)), xs))
        }
    }

    ///
    /// Returns the reverse of `l`.
    ///
    @Time(length(l)) @Space(length(l))
    pub def reverse(l: Nel[a]): Nel[a] = match l {
        case Nel(x, xs) => match List.reverse(x :: xs) {
            case y :: ys => Nel(y, ys)
            case _       => unreachable!()
        }
    }

    ///
    /// Returns `l` with every occurrence of `from` replaced by `to`.
    ///
    @Time(length(l)) @Space(length(l))
    pub def replace(from: {from = a}, to: {to = a}, l: Nel[a]): Nel[a] with Eq[a] =
        map(e -> if (e == from.from) to.to else e, l)

    ///
    /// Returns all permutations of `l` in lexicographical order by element indices in `l`.
    ///
    /// That is, `l` is the first permutation and `reverse(l)` is the last permutation.
    ///
    @Time(Int32.factorial(length(l))) @Space(Int32.factorial(length(l)))
    pub def permutations(l: Nel[a]): Nel[List[a]] = match l {
        case Nel(x, xs) => match List.permutations(x :: xs) {
            case y :: ys => Nel(y, ys)
            case Nil => unreachable!()
        }
    }

    ///
    /// Returns all subsequences of `l` in lexicographical order by element indices in `l`.
    ///
    /// That is, `l` is the first subsequence and `Nil` is the last subsequence.
    ///
    @Time(length(l) * length(l)) @Space(length(l) * length(l))
    pub def subsequences(l: Nel[a]): Nel[List[a]] = match l {
        case Nel(x, xs) => match List.subsequences(x :: xs) {
            case y :: ys => Nel(y, ys)
            case Nil => unreachable!()
        }
    }

    ///
    /// Returns `l` with `a` inserted between every two adjacent elements.
    ///
    @Time(length(l)) @Space(length(l))
    pub def intersperse(a: a, l: Nel[a]): Nel[a] = match l {
        case Nel(x, Nil)  => Nel(x, Nil)
        case Nel(x, xs)   => Nel(x, a :: List.intersperse(a, xs))
    }

    ///
    /// Applies `f` to a start value `s` and all elements in `l` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b \ ef, s: b, l: Nel[a]): b \ ef = match l {
        case Nel(x, xs) => List.foldLeft(f, f(s, x), xs)
    }

    ///
    /// Applies `f` to a start value `z` and all elements in `l` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, z))...)`.
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (a, Unit -> b \ ef) -> b \ ef, z: b, l: Nel[a]): b \ ef = match l {
        case Nel(x, xs) => f(x, _ -> List.foldRightWithCont(f, z, xs))
    }

    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def foldMap(f: a -> b \ ef, l: Nel[a]): b \ ef with Monoid[b] =
        foldLeft((acc, x) -> Monoid.combine(acc, f(x)), Monoid.empty(), l)

    ///
    /// Left-associative reduction of a structure.
    /// Applies `g` to the initial element of `l` and combines it
    /// with the remainder of `l` using `f` going from left to right.
    ///
    pub def reduceLeftTo(f: (b, a) -> b \ ef1, g: a -> b \ ef2, l: Nel[a]): b \ {ef1, ef2} = match l {
        case Nel(x, xs) => List.foldLeft(f, g(x), xs)
    }

    ///
    /// Applies `f` to all elements in `l` going from left to right until a single value `v` is obtained.
    ///
    /// That is, the result is of the form: `f(...f(f(x1, x2), x3)..., xn)`
    ///
    pub def reduceLeft(f: (a, a) -> a \ ef, l: Nel[a]): a \ ef = match l {
        case Nel(x, xs) => List.foldLeft(f, x, xs)
    }

    ///
    /// Returns the number of elements in `l` that satisfy the predicate `f`.
    ///
    @Time(time(f) * length(l)) @Space(space(f) * length(l))
    pub def count(f: a -> Bool \ ef, l: Nel[a]): Int32 \ ef = match l {
        case Nel(x, xs) => (if (f(x)) 1 else 0) + List.count(f, xs)
    }

    ///
    /// Returns the concatenation of the elements in `l`.
    ///
    @Time(length(l)) @Space(length(l))
    pub def flatten(l: Nel[Nel[a]]): Nel[a] = match l {
        case Nel(Nel(y, ys), xs) => Nel(y, ys ::: List.flatMap(toList, xs))
    }

    ///
    /// Returns `true` if and only if at least one element in `l` satisfies the predicate `f`.
    ///
    @Time(time(f) * length(l)) @Space(space(f))
    pub def exists(f: a -> Bool \ ef, l: Nel[a]): Bool \ ef = match l {
        case Nel(x, xs) => if (f(x)) true else List.exists(f, xs)
    }

    ///
    /// Returns `true` if and only if all elements in `l` satisfy the predicate `f`.
    ///
    @Time(time(f) * length(l)) @Space(space(f))
    pub def forall(f: a -> Bool \ ef, l: Nel[a]): Bool \ ef = match l {
        case Nel(x, xs) => if (f(x)) List.forall(f, xs) else false
    }

    ///
    /// Returns a non-empty list where the element at index `i` is `(a, b)` where
    /// `a` is the element at index `i` in `l1` and `b` is the element at index `i` in `l2`.
    ///
    /// If either `l1` or `l2` becomes depleted, then no further elements are added to the resulting list.
    ///
    @Time(Int32.min(length(l1), length(l2))) @Space(Int32.min(length(l1), length(l2)))
    pub def zip(l1: Nel[a], l2: Nel[b]): Nel[(a,b)] = match (l1, l2) {
        case (Nel(x, xs), Nel(y, ys)) => Nel((x, y), List.zip(xs, ys))
    }

    ///
    /// Returns a non-empty list where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `l1` and `b` is the element at index `i` in `l2`.
    ///
    /// If either `l1` or `l2` becomes depleted, then no further elements are added to the resulting list.
    ///
    @Time(time(f) * Int32.min(length(l1), length(l2))) @Space(space(f) * Int32.min(length(l1), length(l2)))
    pub def zipWith(f: (a, b) -> c \ ef, l1: Nel[a], l2: Nel[b]): Nel[c] \ ef = match (l1, l2) {
        case (Nel(x, xs), Nel(y, ys)) => Nel(f(x, y), List.zipWith(f, xs, ys))
    }

    ///
    /// Returns a pair of non-empty lists, the first containing all first components in `l`
    /// and the second containing all second components in `l`.
    ///
    @Time(length(l)) @Space(length(l))
    pub def unzip(l: Nel[(a, b)]): (Nel[a], Nel[b]) =
        let Nel((a, b), xs) = l;
        let (l1, l2) = List.unzip(xs);
        (Nel(a, l1), Nel(b, l2))

    ///
    /// Returns a new non-empty list where each element `e` is mapped to `(e, i)`
    /// where `i` is the index of `e`.
    ///
    pub def zipWithIndex(l: Nel[a]): Nel[(a, Int32)] =
        def loop(ll, k, i) = match ll {
            case (x :: xs) => loop(xs, ks -> (k((x, i) :: ks)), i + 1)
            case Nil       => k(Nil)
        };
        match l {
            case Nel(x, xs) => Nel((x, 0), loop(xs, identity, 1))
        }

    ///
    /// Returns `l` as a normal list.
    ///
    @Time(1) @Space(1)
    pub def toList(l: Nel[a]): List[a] = match l {
        case Nel(x, xs) => x :: xs
    }

    ///
    /// Applies `f` to every element of `l`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def foreach(f: a -> Unit \ ef, l: Nel[a]): Unit \ ef = match l {
        case Nel(x, xs) => f(x); List.foreach(f, xs)
    }

    ///
    /// Returns `l` without the longest prefix that satisfies the predicate `f`.
    ///
    pub def dropWhile(f: a -> Bool \ ef, l: Nel[a]): List[a] \ ef =
        let Nel(x, xs) = l;
        List.dropWhile(f, x :: xs)

    ///
    /// Returns the longest prefix of `l` that satisfies the predicate `f`.
    ///
    pub def takeWhile(f: a -> Bool \ ef, l: Nel[a]): List[a] \ ef =
        let Nel(x, xs) = l;
        List.takeWhile(f, x :: xs)

}


///
/// A type class for types that can be reduced to a summary value.
///
/// `Reducible` is like a non-empty `Foldable` and may only be implemented on non-empty data structures.
///
pub class Reducible[t: Type -> Type] {

    ///
    /// Left-associative reduction of a structure.
    /// Applies `g` to the initial element of `t` and combines it
    /// with the remainder of `t` using `f` going from left to right.
    ///
    pub def reduceLeftTo(f: (b, a) -> b \ ef1, g: a -> b \ ef2, t: t[a]): b \ { ef1, ef2 }

    ///
    /// Right-associative reduction of a structure.
    /// Applies `g` to the initial element of `t` and combines it
    /// with the remainder of `t` using `f` going from right to left.
    ///
    pub def reduceRightTo(f: (a, b) -> b \ ef1, g: a -> b \ ef2, t: t[a]): b \ { ef1, ef2 }

    ///
    /// Left-associative reduction on `t` using `f`.
    ///
    pub def reduceLeft(f: (a, a) -> a \ ef, t: t[a]): a \ ef =
        Reducible.reduceLeftTo(f, identity, t)

    ///
    /// Right-associative reduction on `t` using `f`.
    ///
    pub def reduceRight(f: (a, a) -> a \ ef, t: t[a]): a \ ef =
        Reducible.reduceRightTo(f, identity, t)

    ///
    /// Reduce `t` using the derived `SemiGroup` instance.
    ///
    pub def reduce(t: t[a]): a with SemiGroup[a] =
        Reducible.reduceLeft(SemiGroup.combine, t)

    ///
    /// Applies `f` to each element of `t` and combines them using the derived `SemiGroup` instance.
    ///
    pub def reduceMap(f: a -> b \ ef, t: t[a]): b \ ef with SemiGroup[b] =
        Reducible.reduceLeftTo((b, a) -> SemiGroup.combine(b, f(a)), f, t)

    ///
    /// Left-associative fold of a structure.
    /// Applies `f` to a start value `s` and all elements in `t` going from left to right.
    ///
    pub def foldLeft(f: (b, a) -> b \ ef, s: b, t: t[a]): b \ ef =
        Reducible.reduceLeftTo(f, a -> f(s, a), t)

    ///
    /// Right-associative fold of a structure.
    /// Applies `f` to a start value `s` and all elements in `t` going from right to left.
    ///
    pub def foldRight(f: (a, b) -> b \ ef, s: b, t: t[a]): b \ ef =
        Reducible.reduceRightTo(f, a -> f(a, s), t)

    ///
    /// Right-associative fold of a structure.
    /// Applies `f` to a start value `s` and all elements in `t` going from right to left.
    ///
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (a, Unit -> b \ ef) -> b \ ef, z: b, t: t[a]): b \ ef =
        let f1 = (a, b) -> f(a, upcast(_ -> b));
        Reducible.reduceRightTo(f1, a -> f(a, upcast(_ -> z)), t)

    ///
    /// Alias for `reduce`.
    ///
    /// Reduce `t` using the derived `SemiGroup` instance.
    ///
    pub def fold(t: t[a]): a with SemiGroup[a] =
        Reducible.reduceLeft(SemiGroup.combine, t)

    ///
    /// Returns the first element of `t`.
    ///
    pub def head(t: t[a]): a =
        Reducible.reduceLeft((acc, _) -> acc, t)

    ///
    /// Returns the last element of `t`.
    ///
    pub def last(t: t[a]): a =
        Reducible.reduceLeft((_, a) -> a, t)

    ///
    /// Returns `t` as a list without the last element.
    ///
    pub def init(t: t[a]): List[a] =
        Reducible.reduceRightTo((a, acc) -> a :: acc, _ -> Nil, t)

    ///
    /// Returns the tail of `t` as a list.
    ///
    pub def tail(t: t[a]): List[a] =
        Reducible.reduceLeftTo((k, a) -> ks -> k(a :: ks),  _ -> identity, t)(Nil) // NB: Builds a continuation function without the first
                                                                                   // element that is immediately invoked to avoid `foldRight`.
    ///
    /// Returns the reverse of `t` as a list.
    ///
    pub def reverse(t: t[a]): List[a] =
        Reducible.foldLeft((acc, a) -> a :: acc, Nil, t)

    ///
    /// Returns the number of elements in `t` that satisfy the predicate `f`.
    ///
    pub def count(f: a -> Bool \ ef, t: t[a]): Int32 \ ef =
        Reducible.foldLeft((acc, a) -> if (f(a)) 1 + acc else acc, 0, t)

    ///
    /// Returns the number of elements in `t`.
    ///
    pub def length(t: t[a]): Int32 =
        Reducible.foldLeft((acc, _) -> 1 + acc, 0, t)

    ///
    /// Returns the sum of all elements in `t`.
    ///
    pub def sum(t: t[Int32]): Int32 =
        Reducible.foldLeft((acc, a) -> a + acc, 0, t)

    ///
    /// Returns the sum of all elements in `t` according to the function `f`.
    ///
    pub def sumWith(f: a -> Int32 \ ef, t: t[a]): Int32 \ ef =
        Reducible.foldLeft((acc, a) -> f(a) + acc, 0, t)

    ///
    /// Returns the product of all elements in `t`.
    ///
    pub def product(t: t[Int32]): Int32 =
        Reducible.foldLeft((acc, a) -> a * acc, 1, t)

    ///
    /// Returns the product of all elements in `t` according to the function `f`.
    ///
    pub def productWith(f: a -> Int32 \ ef, t: t[a]): Int32 \ ef =
        Reducible.foldLeft((acc, a) -> f(a) * acc, 1, t)

    ///
    /// Returns `true` if and only if at least one element in `t` satisfies the predicate `f`.
    ///
    pub def exists(f: a -> Bool \ ef, t: t[a]): Bool \ ef =
        Reducible.foldLeft((acc, a) -> f(a) or acc, false, t)

    ///
    /// Returns `true` if and only if all elements in `t` satisfy the predicate `f`.
    ///
    pub def forall(f: a -> Bool \ ef, t: t[a]): Bool \ ef =
        Reducible.foldLeft((acc, a) -> f(a) and acc, true, t)

    ///
    /// Applies `f` to each element in `t`.
    ///
    pub def foreach(f: a -> Unit \ ef, t: t[a]): Unit \ ef =
        Reducible.foldLeft((_, a) -> f(a), (), t)

    ///
    /// Alias for `findLeft`.
    ///
    /// Optionally returns the first element of `t` that satisfies the predicate `f` when searching from left to right.
    ///
    pub def find(f: a -> Bool \ ef, t: t[a]): Option[a] \ ef =
        Reducible.findLeft(f, t)

    ///
    /// Optionally returns the first element of `t` that satisfies the predicate `f` when searching from left to right.
    ///
    pub def findLeft(f: a -> Bool \ ef, t: t[a]): Option[a] \ ef =
        Reducible.foldLeft((acc, a) -> Option.withDefault(default = if (f(a)) Some(a) else None, acc), None, t)

    ///
    /// Optionally returns the first element of `t` that satisfies the predicate `f` when searching from right to left.
    ///
    pub def findRight(f: a -> Bool \ ef, t: t[a]): Option[a] \ ef =
        Reducible.foldRight((a, acc) -> Option.withDefault(default = if (f(a)) Some(a) else None, acc), None, t)

    ///
    /// Returns `true` if and only if the element `a` is in `t`.
    ///
    pub def memberOf(a: a, t: t[a]): Bool with Eq[a] =
        Reducible.foldLeft((acc, x) -> a == x or acc, false, t)

    ///
    /// Finds the smallest element of `t` according to the `Order` on `a`.
    ///
    pub def minimum(t: t[a]): a with Order[a] =
        Reducible.reduceLeft(Order.min, t)

    ///
    /// Finds the smallest element of `t` according to the given comparator `cmp`.
    ///
    pub def minimumBy(cmp: (a, a) -> Comparison, t: t[a]): a =
        Reducible.reduceLeft(Order.minBy(cmp), t)

    ///
    /// Finds the largest element of `t` according to the `Order` on `a`.
    ///
    pub def maximum(t: t[a]): a with Order[a] =
        Reducible.reduceLeft(Order.max, t)

    ///
    /// Finds the largest element of `t` according to the given comparator `cmp`.
    ///
    pub def maximumBy(cmp: (a, a) -> Comparison, t: t[a]): a =
        Reducible.reduceLeft(Order.maxBy(cmp), t)

    ///
    /// Returns `t` as a list without the longest prefix that satisfies the predicate `f`.
    ///
    pub def dropWhile(f: a -> Bool \ ef, t: t[a]): List[a] \ ef =
        (Reducible.foldLeft((acc, a) -> {
            let (c, k) = acc;
            if (c and f(a))
                (true, k)
            else
                (false, ks -> k(a :: ks))
        }, (true, identity), t) |> snd)(Nil)

    ///
    /// Returns the longest prefix of `t` as a list that satisfies the predicate `f`.
    ///
    pub def takeWhile(f: a -> Bool \ ef, t: t[a]): List[a] \ ef =
        (Reducible.foldLeft((acc, a) -> {
            let (c, k) = acc;
            if (c and f(a))
                (true, ks -> k(a :: ks))
            else
                (false, k)
        }, (true, identity), t) |> snd)(Nil)

    ///
    /// Returns `t` as a list with `a` inserted between every two adjacent elements.
    ///
    pub def intersperse(a: a, t: t[a]): List[a] =
        Reducible.reduceRightTo((x, acc) -> x :: a :: acc, x -> x :: Nil, t)

    ///
    /// Returns `t` as a list.
    ///
    pub def toList(t: t[a]): List[a] =
        Reducible.foldRight((a, acc) -> a :: acc, Nil, t)

    ///
    /// Returns `t` as a non-empty list.
    ///
    pub def toNel(t: t[a]): Nel[a] =
        Reducible.reduceLeftTo((acc, a) -> Nel.append(acc, Nel(a, Nil)), a -> Nel(a, Nil), t)

}

///
/// The NonEmpty Chain type.
///
/// A chain is a list represented as an unbalanced binary tree.
/// It supports efficient append and "snoc" - appending elements at the tail
/// of the list.
///
/// Note - the constructors `NecOne` and `Nec` should not be used directly.
///
pub enum Nec[t] {
    case NecOne(t)
    case Nec(Nec[t], Nec[t])
}

instance Eq[Nec[a]] with Eq[a] {
    pub def eq(c1: Nec[a], c2: Nec[a]): Bool = Nec.equals(c1, c2)
}

instance Order[Nec[a]] with Order[a] {
    ///
    /// Compares `c1` and `c2` lexicographically.
    ///
    pub def compare(c1: Nec[a], c2: Nec[a]): Comparison =
        use Nec.ViewLeft.{OneLeft, SomeLeft};
        match (Nec.viewLeft(c1), Nec.viewLeft(c2)) {
            case (OneLeft(x), OneLeft(y))           => x <=> y
            case (OneLeft(x), SomeLeft(y, _))       => match (x <=> y) {
                case EqualTo => LessThan
                case cmp     => cmp
            }
            case (SomeLeft(x, _), OneLeft(y))       => match (x <=> y) {
                case EqualTo => GreaterThan
                case cmp     => cmp
            }
            case (SomeLeft(x, xs), SomeLeft(y, ys)) =>
                let cmp = x <=> y;
                if (cmp == EqualTo) xs <=> ys else cmp
        }
}

instance Hash[Nec[a]] with Hash[a] {
    pub def hash(c: Nec[a]): Int32 = 39119 + Hash.hash(Nec.toList(c))
}

instance SemiGroup[Nec[a]] {
    pub def combine(c1: Nec[a], c2: Nec[a]): Nec[a] = Nec.append(c1, c2)
}

instance Functor[Nec] {
    pub def map(f: a -> b \ ef, c: Nec[a]): Nec[b] \ ef = Nec.map(f, c)
}

instance Applicative[Nec] {
    pub def point(x: a) : Nec[a] = Nec.singleton(x)
    pub def ap(f: Nec[a -> b \ ef], x: Nec[a]) : Nec[b] \ ef = Nec.ap(f, x)
}

instance Monad[Nec] {
    pub def flatMap(f: a -> Nec[b] \ ef, x : Nec[a]) : Nec[b] \ ef = Nec.flatMap(f, x)
}

instance UnorderedFoldable[Nec] {
    pub def foldMap(f: a -> b \ ef, c: Nec[a]): b \ ef with CommutativeMonoid[b] = Nec.foldMap(f, c)
    override pub def isEmpty(_: Nec[a]): Bool = false
    override pub def exists(f: a -> Bool \ ef, c: Nec[a]): Bool \ ef = Nec.exists(f, c)
    override pub def forall(f: a -> Bool \ ef, c: Nec[a]): Bool \ ef = Nec.forall(f, c)
    override pub def memberOf(x: a, c: Nec[a]): Bool with Eq[a] = Nec.memberOf(x, c)
}

namespace Nec {

    use Functor.{<$>}
    use Applicative.{<*>}

    ///
    /// A datatype for pattern matching on a Nec (traversing left-to-right).
    ///
    pub enum ViewLeft[a] with Eq {
        case OneLeft(a)
        case SomeLeft(a, Nec[a])
    }

    ///
    /// A datatype for pattern matching on a Nec (traversing right-to-left).
    ///
    pub enum ViewRight[a] with Eq {
        case OneRight(a)
        case SomeRight(Nec[a], a)
    }

    ///
    /// Returns `true` if and only if `c1` and `c2` and equal.
    ///
    pub def equals(c1: Nec[a], c2: Nec[a]): Bool with Eq[a] = viewLeft(c1) == viewLeft(c2)

    //
    // Implementation Note: Necs are considered equal if their (ordered) list of elements are equal.
    //
    // Because they may have different shapes due to construction we use a view to
    // decide equality which imposes an order on the Nec.
    //

    ///
    /// Return the singleton Nec with element `x`.
    ///
    pub def singleton(x: a): Nec[a] = NecOne(x)

    ///
    /// Returns true if and only if `c` is a single element Nec.
    ///
    pub def isSingleton(c: Nec[a]): Bool = match c {
        case NecOne(_) => true
        case _         => false
    }

    ///
    /// Add element `x` to the left end of Nec `c`.
    ///
    pub def cons(x: a, c: Nec[a]): Nec[a] = Nec(NecOne(x), c)

    ///
    /// Add element `x` to the right end of Nec `c`.
    ///
    pub def snoc(c: Nec[a], x: a): Nec[a] = Nec(c, NecOne(x))

    ///
    /// Returns the first element of `c`.
    ///
    pub def head(c: Nec[a]): a = match viewLeft(c) {
        case OneLeft(x)     => x
        case SomeLeft(x, _) => x
    }

    ///
    /// Returns the last element of `c`.
    ///
    pub def last(c: Nec[a]): a = match viewRight(c) {
        case OneRight(x)     => x
        case SomeRight(_, x) => x
    }

    ///
    /// Returns the list of elements in `c` without the last element.
    ///
    pub def init(c: Nec[a]): List[a] = match viewRight(c) {
        case OneRight(_)      => Nil
        case SomeRight(rs, _) => toList(rs)
    }

    ///
    /// Returns all elements in `c` without the first element.
    ///
    pub def tail(c: Nec[a]): List[a] = match viewLeft(c) {
        case OneLeft(_)      => Nil
        case SomeLeft(_, rs) => toList(rs)
    }

    ///
    /// Returns the length of `c`.
    ///
    pub def length(c: Nec[a]): Int32 = foldRight((_, acc) -> acc + 1, 0, c)

    ///
    /// Returns a new Nec formed by appending the Necs `c1` and `c2`.
    ///
    pub def append(c1: Nec[a], c2: Nec[a]): Nec[a] = Nec(c1, c2)

    ///
    /// Deconstruct a Nec from left-to-right.
    ///
    /// Returns `SomeLeft(x, rs)` if the Nec has at least two elements, where `x` is the leftmost
    /// element of the Nec `c`, and `rs` is the rest of the Nec.
    ///
    /// Returns `OneLeft` if the Nec has a single element.
    ///
    pub def viewLeft(c: Nec[a]): ViewLeft[a] =
        def loop(c1: Nec[a], rs: Option[Nec[a]], k: ViewLeft[a] -> ViewLeft[a]) = match (c1, rs) {
            case (NecOne(x), None)      => k(OneLeft(x))
            case (NecOne(x), Some(rs1)) => k(SomeLeft(x, rs1))
            case (Nec(l, r), None)      => loop(l, Some(r),  k)
            case (Nec(l, r), Some(rs1)) => loop(l, Some(append(r, rs1)),  k)
        };
        loop(c, None, x -> x)


    ///
    /// Deconstruct a Nec from right-to-left.
    ///
    /// Returns `SomeRight(rs, x)` if the Nec has at least two elements, where `x` is the rightmost
    /// element of the Nec `c`, and `rs` is the front of the Nec.
    ///
    /// Returns `OneRight` if the Nec has a single element.
    ///
    pub def viewRight(c: Nec[a]): ViewRight[a] =
        def loop(c1: Nec[a], rs: Option[Nec[a]], k: ViewRight[a] -> ViewRight[a]) = match (c1, rs) {
            case (NecOne(x), None)      => k(OneRight(x))
            case (NecOne(x), Some(rs1)) => k(SomeRight(rs1, x))
            case (Nec(l, r), None)      => loop(r, Some(l),  k)
            case (Nec(l, r), Some(rs1)) => loop(r, Some(append(rs1, l)),  k)
        };
        loop(c, None, x -> x)

    ///
    /// Returns `true` if and only if `c` contains the element `a`.
    ///
    pub def memberOf(a: a, c: Nec[a]): Bool with Eq[a] =
        def loop(c1) = match viewLeft(c1) {
            case OneLeft(x)               => x == a
            case SomeLeft(x, _) if x == a => true
            case SomeLeft(_, c2)          => loop(c2)
        };
        loop(c)

    ///
    /// Finds the smallest element of `c` according to the `Order` on `a`.
    ///
    pub def minimum(c: Nec[a]): a with Order[a] =
        reduceLeft(Order.min, c)

    ///
    /// Finds the smallest element of `c` according to the given comparator `cmp`.
    ///
    pub def minimumBy(cmp: (a, a) -> Comparison, c: Nec[a]): a =
        reduceLeft(Order.minBy(cmp), c)

    ///
    /// Finds the largest element of `c` according to the `Order` on `a`.
    ///
    pub def maximum(c: Nec[a]): a with Order[a] =
        reduceLeft(Order.max, c)

    ///
    /// Finds the largest element of `c` according to the given comparator `cmp`.
    ///
    pub def maximumBy(cmp: (a, a) -> Comparison, c: Nec[a]): a =
        reduceLeft(Order.maxBy(cmp), c)

    ///
    /// Optionally returns the position of `a` in `c`.
    ///
    pub def indexOf(a: a, c: Nec[a]): Option[Int32] with Eq[a] =
        def loop(acc: Int32, c1: Nec[a]) = match viewLeft(c1) {
            case OneLeft(x)      => if (x == a) Some(acc) else None
            case SomeLeft(x, c2) => if (x == a) Some(acc) else loop(acc + 1, c2)
        };
        loop(0, c)

    ///
    /// Alias for `findLeft`.
    ///
    pub def find(f: a -> Bool \ ef, c: Nec[a]): Option[a] \ ef = findLeft(f, c)

    ///
    /// Optionally returns the first element of `c` that satisfies the predicate `f` when searching from left to right.
    ///
    pub def findLeft(f: a -> Bool \ ef, c: Nec[a]): Option[a] \ ef = match viewLeft(c) {
        case OneLeft(x)      => if (f(x)) Some(x) else None
        case SomeLeft(x, rs) => if (f(x)) Some(x) else findLeft(f, rs)
    }

    ///
    /// Optionally returns the first element of `c` that satisfies the predicate `f` when searching from right to left.
    ///
    pub def findRight(f: a -> Bool \ ef, c: Nec[a]): Option[a] \ ef = match viewRight(c) {
        case OneRight(x)      => if (f(x)) Some(x) else None
        case SomeRight(rs, x) => if (f(x)) Some(x) else findRight(f, rs)
    }

    ///
    /// Returns a list of all integers between `b` (inclusive) and `e` (exclusive) wrapped in `Some`.
    ///
    /// Returns `None` if `b >= e`.
    ///
    pub def range(b: Int32, e: Int32): Option[Nec[Int32]] =
        def loop(ix: Int32, k: Nec[Int32] -> Nec[Int32]) = match (e-1) {
            case e1 if ix == e1 => k(singleton(ix))
            case e1 if ix < e1  => loop(ix+1, ks -> k(cons(ix, ks)))
            case e1 if ix > e1  => unreachable!()
        };
        if (b < e)
            loop(b, ks -> ks) |> Some
        else
            None

    ///
    /// Returns the result of applying `f` to every element in `c`.
    ///
    /// That is, the result is of the form: `f(x1) :: f(x2) :: ...`.
    ///
    pub def map(f: a -> b \ ef, c: Nec[a]): Nec[b] \ ef =
        def loop(c1: Nec[a], k: Nec[b] -> Nec[b] \ ef) = match viewLeft(c1) {
            case OneLeft(x)      => {let a = f(x); k(NecOne(a))}
            case SomeLeft(x, rs) => loop(rs, ks -> {let a = f(x); k(cons(a, ks))})
        };
        loop(c, upcast(identity))

    ///
    /// Returns the result of applying `f` to every element in `c` along with that element's index.
    ///
    /// That is, the result is of the form: `f(x1, 0) :: f(x2, 1) :: ...`.
    ///
    pub def mapWithIndex(f: (Int32, a) -> b \ ef, c: Nec[a]): Nec[b] \ ef =
        def loop(c1: Nec[a], i: Int32, k: Nec[b] -> Nec[b] \ ef) = match viewLeft(c1) {
            case OneLeft(x)      => {let a = f(i, x); k(NecOne(a))}
            case SomeLeft(x, rs) => loop(rs, i+1, ks -> {let a = f(i, x); k(cons(a, ks))})
        };
        loop(c, 0, upcast(identity))

    ///
    /// Apply every function from `f` to every argument from `x` and return a Nec with all results.
    /// For `f = f1, f2, ...` and `x = x1, x2, ...` the results appear in the order
    /// `f1(x1), f1(x2), ..., f2(x1), f2(x2), ...`.
    ///
    pub def ap(f: Nec[a -> b \ ef], c: Nec[a]) : Nec[b] \ ef =
        def loop(f1: Nec[a -> b \ ef], k: Nec[b] -> Nec[b] \ ef) = match viewLeft(f1) {
            case OneLeft(f2)      => k(map(f2, c))
            case SomeLeft(f2, rs) => loop(rs, ks -> k(map(f2, c) `append` ks))
        };
        loop(f, upcast(identity))

    ///
    /// Returns the result of applying `f` to every element in `c` and concatenating the results.
    ///
    pub def flatMap(f: a -> Nec[b] \ ef, c: Nec[a]): Nec[b] \ ef =
        def loop(c1: Nec[a], k: Nec[b] -> Nec[b] \ ef) = match viewLeft(c1) {
            case OneLeft(x)      => {let a = f(x); k(a)}
            case SomeLeft(x, rs) => loop(rs, ks -> {let a = f(x); k(append(a, ks))})
        };
        loop(c, upcast(identity))

    ///
    /// Returns the reverse of `c`.
    ///
    pub def reverse(c: Nec[a]): Nec[a] =
        // Use an accumulator rather than CPS, as it will be built "naturally" in reverse order.
        def loop(c1: Nec[a], acc: Nec[a]) = match viewLeft(c1) {
            case OneLeft(x)      => cons(x, acc)
            case SomeLeft(x, rs) => loop(rs, cons(x, acc))
        };
        // Do one step before the loop so we have some content.
        match viewLeft(c) {
            case OneLeft(x)      => singleton(x)
            case SomeLeft(x, rs) => loop(rs, singleton(x))
        }

    ///
    /// Returns `l` with every occurrence of `from` replaced by `to`.
    ///
    pub def replace(from: {from = a}, to: {to = a}, l: Nec[a]): Nec[a] with Eq[a] =
        map(e -> if (e == from.from) to.to else e, l)

    ///
    /// Returns all permutations of `c` in lexicographical order by element indices in `c`.
    ///
    /// That is, `c` is the first permutation and `reverse(c)` is the last permutation.
    ///
    pub def permutations(c: Nec[a]): Nec[List[a]] = match viewLeft(c) {
        case OneLeft(x)      => singleton(x :: Nil)
        case SomeLeft(x, xs) => match fromList(List.permutations(x :: toList(xs))) {
            case Some(ans) => ans
            case None      => unreachable!()
        }
    }
    ///
    /// Returns all subsequences of `l` in lexicographical order by element indices in `l`.
    ///
    /// That is, `l` is the first subsequence and `Nil` is the last subsequence.
    ///
    pub def subsequences(c: Nec[a]): Nec[List[a]] = match viewLeft(c) {
        case OneLeft(x)      => cons(x :: Nil, singleton(Nil))
        case SomeLeft(x, xs) => match fromList(List.subsequences(x :: toList(xs))) {
            case Some(ans) => ans
            case None      => unreachable!()
        }
    }

    ///
    /// Helper for the `permutations` and `subsequences` functions.
    ///
    /// Uses a worker-wrapper idiom for the loop (passing the head and
    /// the rest of the list) so loop never produces an empty list.
    ///
    def fromList(l: List[a]): Option[Nec[a]] =
        def loop(x, xs, k) = match xs {
            case Nil       => k(singleton(x))
            case x1 :: rs  => loop(x1, rs, ks -> k(cons(x, ks)))
        };
        match l {
            case Nil => None
            case x :: xs => loop(x, xs, ks -> ks) |> Some
        }

    ///
    /// Returns `c` with `a` inserted between every two adjacent elements.
    ///
    pub def intersperse(sep: a, c: Nec[a]): Nec[a] =
        def loop(c1: Nec[a], k: Nec[a] -> Nec[a]) = match viewLeft(c1) {
            case OneLeft(x)      => k(cons(sep, singleton(x)))
            case SomeLeft(x, rs) => loop(rs, ks -> k(cons(sep, cons(x, ks))))
        };
        // Do one step before the loop so we have some content.
        match viewLeft(c) {
            case OneLeft(x)      => singleton(x)
            case SomeLeft(x, rs) => loop(rs, ks -> cons(x, ks))
        }

    ///
    /// Returns the number of elements in `c` that satisfy the predicate `f`.
    ///
    pub def count(f: a -> Bool \ ef, c: Nec[a]): Int32 \ ef =
        def loop(c1: Nec[a], acc: Int32) = match viewLeft(c1) {
            case OneLeft(x)      => if (f(x)) acc + 1 else acc
            case SomeLeft(x, rs) => loop(rs, if (f(x)) acc + 1 else acc)
        };
        loop(c, 0)

    ///
    /// Returns the concatenation of the elements in `c`.
    ///
    pub def flatten(c: Nec[Nec[a]]): Nec[a] = match viewLeft(c) {
        case OneLeft(xs)       => xs
        case SomeLeft(xs, xss) => foldLeft(append, xs, xss)
    }

    ///
    /// Returns `true` if and only if at least one element in `c` satisfies the predicate `f`.
    ///
    /// Returns `false` if `c` is empty.
    ///
    pub def exists(f: a -> Bool \ ef, c: Nec[a]): Bool \ ef = match viewLeft(c) {
        case OneLeft(x)      => f(x)
        case SomeLeft(x, rs) => if (f(x)) true else exists(f, rs)
    }

    ///
    /// Returns `true` if and only if all elements in `c` satisfy the predicate `f`.
    ///
    /// Returns `true` if `c` is empty.
    ///
    pub def forall(f: a -> Bool \ ef, c: Nec[a]): Bool \ ef = match viewLeft(c) {
        case OneLeft(x)      => f(x)
        case SomeLeft(x, rs) => if (not f(x)) false else forall(f, rs)
    }

    ///
    /// Returns a list of every element in `c` that satisfies the predicate `f`.
    ///
    pub def filter(f: a -> Bool \ ef, c: Nec[a]): List[a] \ ef =
        def loop(c1: Nec[a], k: List[a] -> List[a] \ ef) = match viewLeft(c1) {
            case OneLeft(x)      => if (f(x)) k(x :: Nil) else k(Nil)
            case SomeLeft(x, rs) => if (f(x)) loop(rs, ks -> k(x :: ks)) else loop(rs, k)
        };
        loop(c, upcast(identity))

    ///
    /// Applies `f` to a start value `s` and all elements in `c` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b \ ef, acc: b, c: Nec[a]): b \ ef = match viewLeft(c) {
        case OneLeft(x)      => f(acc, x)
        case SomeLeft(x, rs) => {
            let b = f(acc, x);
            foldLeft(f, b, rs)
        }
    }

    ///
    /// Applies `f` to a start value `s` and all elements in `c` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    pub def foldRight(f: (a, b) -> b \ ef, s: b, c: Nec[a]): b \ ef = match viewRight(c) {
        case OneRight(x)      => f(x, s)
        case SomeRight(rs, x) => {
            let b = f(x, s);
            foldRight(f, b, rs)
        }
    }

    ///
    /// Applies `f` to a start value `z` and all elements in `c` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, z))...)`.
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (a, Unit -> b \ ef) -> b \ ef, z: b, c: Nec[a]): b \ ef = match viewLeft(c) {
        case OneLeft(x)      => f(x, upcast(_ -> z))
        case SomeLeft(x, rs) => f(x, _ -> foldRightWithCont(f, z, rs))
    }

    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def foldMap(f: a -> b \ ef, c: Nec[a]): b \ ef with Monoid[b] =
        foldLeft((acc, x) -> Monoid.combine(acc, f(x)), Monoid.empty(), c)

    ///
    /// Returns the first non-None result of applying the partial function `f` to each element of `c`.
    ///
    /// Returns `None` if f(c) for every element of c is `None`.
    ///
    pub def findMap(f: a -> Option[b] \ ef, c: Nec[a]): Option[b] \ ef = match viewLeft(c) {
        case OneLeft(x)      => f(x)
        case SomeLeft(x, rs) => match f(x) {
            case Some(v) => Some(v)
            case None    => findMap(f, rs)
        }
    }

    ///
    /// Returns a Nec where the element at index `i` is `(a, b)` where
    /// `a` is the element at index `i` in `c1` and `b` is the element at index `i` in `c2`.
    ///
    /// If either `c1` or `c2` becomes depleted, then no further elements are added to the resulting Nec.
    ///
    pub def zip(c1: Nec[a], c2: Nec[b]): Nec[(a,b)] =
        def loop(nec1: Nec[a], nec2: Nec[b], k: Nec[(a,b)] -> Nec[(a,b)]) = match (viewLeft(nec1), viewLeft(nec2)) {
            case (SomeLeft(x, xs), SomeLeft(y, ys)) => loop(xs, ys, ks -> k(cons((x, y), ks)))
            case (OneLeft(x), OneLeft(y))           => k(NecOne((x, y)))
            case (SomeLeft(x, _), OneLeft(y))       => k(NecOne((x, y)))
            case (OneLeft(x), SomeLeft(y, _))       => k(NecOne((x, y)))
        };
        loop(c1, c2, k -> k)

    ///
    /// Returns a Nec where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `c1` and `b` is the element at index `i` in `c2`.
    ///
    /// If either `c1` or `c2` becomes depleted, then no further elements are added to the resulting Nec.
    ///
    pub def zipWith(f: (a, b) -> c \ ef, c1: Nec[a], c2: Nec[b]): Nec[c] \ ef =
        def loop(nec1: Nec[a], nec2: Nec[b], k: Nec[c] -> Nec[c] \ ef) = match (viewLeft(nec1), viewLeft(nec2)) {
            case (OneLeft(x), OneLeft(y))           => {let a = f(x, y); k(singleton(a))}
            case (OneLeft(x), SomeLeft(y, _))       => {let a = f(x, y); k(singleton(a))}
            case (SomeLeft(x, _), OneLeft(y))       => {let a = f(x, y); k(singleton(a))}
            case (SomeLeft(x, rs), SomeLeft(y, qs)) => {
                let a = f(x, y);
                loop(rs, qs, ks -> k(cons(a, ks)))
            }
        };
        loop(c1, c2, upcast(identity))

    ///
    /// Returns a pair of Necs, the first containing all first components in `c`
    /// and the second containing all second components in `c`.
    ///
    pub def unzip(c: Nec[(a, b)]): (Nec[a], Nec[b]) =
        def loop(c1: Nec[(a, b)], k: (Nec[a], Nec[b]) -> (Nec[a], Nec[b])) = match viewLeft(c1) {
            case OneLeft((a, b))      => k(singleton(a), singleton(b))
            case SomeLeft((a, b), rs) => loop(rs, (ks, ls) -> k(cons(a, ks), cons(b, ls)))
        };
        loop(c, (ks, ls) -> (ks, ls))

    ///
    /// Returns a Nec where each element `e` is mapped to `(e, i)` where `i`
    /// is the index of `e`.
    ///
    pub def zipWithIndex(c: Nec[a]): Nec[(a, Int32)] =
        def loop(c1, k, i) = match viewLeft(c1) {
            case OneLeft(x)      => k(NecOne(x, i))
            case SomeLeft(x, rs) => loop(rs, ks -> k(cons((x, i), ks)), i + 1)
        };
        loop(c, k -> k, 0)

    ///
    /// Applies `f` to every element of `c`.
    ///
    pub def foreach(f: a -> Unit \ ef, c: Nec[a]): Unit \ ef = match viewLeft(c) {
        case OneLeft(x)      => f(x)
        case SomeLeft(x, rs) => f(x); foreach(f, rs)
    }

    ///
    /// Returns `c` as a list.
    ///
    pub def toList(c: Nec[a]): List[a] =
        foldRight((x, acc) -> x :: acc, Nil, c)

    ///
    /// Helper function for `traverse` and `sequence`.
    ///
    /// Builds an "applicative Nec".
    ///
    def consA(mx: f[a], mxs: f[Nec[a]]): f[Nec[a]] with Applicative[f] =
        (((x, xs) -> cons(x, xs)) <$> mx) <*> mxs

    ///
    /// Returns the result of running all the actions in the Nec `c`.
    ///
    pub def sequence(c: Nec[m[a]]): m[Nec[a]] with Applicative[m] =
        def loop(l2, k) = match viewLeft(l2) {
            case OneLeft(x)      => k(NecOne <$> x)
            case SomeLeft(x, rs) => loop(rs, ks -> k(consA(x, ks)))
        };
        loop(c, ks -> ks)

    ///
    /// Applies `combine` to all elements in `c` until a single value is obtained.
    ///
    pub def reduce(c: Nec[a]): a with SemiGroup[a] = match viewLeft(c) {
        case OneLeft(x)      => x
        case SomeLeft(x, xs) => foldLeft((acc, y) -> SemiGroup.combine(acc, y), x, xs)
    }

    ///
    /// Applies `f` to all elements in `c` going from left to right until a single value `v` is obtained.
    ///
    /// That is, the result is of the form: `f(...f(f(x1, x2), x3)..., xn)`
    ///
    pub def reduceLeft(f: (a, a) -> a \ ef, c: Nec[a]): a \ ef = match viewLeft(c) {
        case OneLeft(x)      => x
        case SomeLeft(x, xs) => foldLeft(f, x, xs)
    }

    ///
    /// Applies `f` to all elements in `c` going from right to left until a single value `v` is obtained.
    ///
    /// That is, the result is of the form: `Some(f(x1, ...f(xn-2, f(xn-1, xn))...))`
    ///
    pub def reduceRight(f: (a, a) -> a \ ef, c: Nec[a]): a \ ef = match viewRight(c) {
        case OneRight(x)      => x
        case SomeRight(xs, x) => foldRight((a, acc) -> f(a, acc), x, xs)
    }

    ///
    /// Left-associative reduction of a structure.
    /// Applies `g` to the initial element of `c` and combines it
    /// with the remainder of `c` using `f` going from left to right.
    ///
    pub def reduceLeftTo(f: (b, a) -> b \ ef1, g: a -> b \ ef2, c: Nec[a]): b \ {ef1, ef2} = match viewLeft(c) {
        case OneLeft(x)      => g(x)
        case SomeLeft(x, xs) => foldLeft(f, g(x), xs)
    }

    ///
    /// Right-associative reduction of a structure.
    /// Applies `g` to the initial element of `c` and combines it
    /// with the remainder of `c` using `f` going from right to left.
    ///
    pub def reduceRightTo(f: (a, b) -> b \ ef1, g: a -> b \ ef2, c: Nec[a]): b \ {ef1, ef2} = match viewRight(c) {
        case OneRight(x)      => g(x)
        case SomeRight(xs, x) => foldRight((a, acc) -> f(a, acc), g(x), xs)
    }

    ///
    /// Returns `c` without the longest prefix that satisfies the predicate `f`.
    ///
    pub def dropWhileLeft(f: a -> Bool \ ef, c: Nec[a]): List[a] \ ef =
        def loop(c1) = match viewLeft(c1) {
            case OneLeft(x)      => if (f(x)) Nil else (x :: Nil)
            case SomeLeft(x, rs) => if (f(x)) loop(rs) else x :: toList(rs)
        };
        loop(c)

    ///
    /// Returns `c` without the longest sufffix that satisfies the predicate `f`.
    ///
    pub def dropWhileRight(f: a -> Bool \ ef, c: Nec[a]): List[a] \ ef =
        def loop(c1) = match viewRight(c1) {
            case OneRight(x)      => if (f(x)) Nil else (x :: Nil)
            case SomeRight(rs, x) => if (f(x)) loop(rs) else toList(rs `snoc` x)
        };
        loop(c)

    ///
    /// Returns the longest prefix of `c` that satisfies the predicate `f`.
    ///
    pub def takeWhileRight(f: a -> Bool \ ef, c: Nec[a]): List[a] \ ef =
        def loop(c1, ac) = match viewRight(c1) {
            case OneRight(x)      => if (f(x)) (x :: ac) else ac
            case SomeRight(rs, x) => if (f(x)) loop(rs, x :: ac) else ac
        };
        loop(c, Nil)

}


///
/// The Validation type.
///
pub enum Validation[t, e] with Eq, Order {
    case Success(t),
    case Failure(Nec[e])
}

instance Hash[Validation[t, e]] with Hash[t], Hash[e] {
    pub def hash(x: Validation[t, e]): Int32 = match x {
        case Success(v) => 5407 + 197 * Hash.hash(v)
        case Failure(v) => 5413 + 199 * Hash.hash(v)
    }
}

instance SemiGroup[Validation[t, e]] with SemiGroup[t] {
    pub def combine(x: Validation[t, e], y: Validation[t, e]): Validation[t, e] = match (x, y) {
        case (Success(x1), Success(y1))     => Success(SemiGroup.combine(x1, y1))
        case (Failure(es1), Failure(es2))   => Failure(Nec.append(es1, es2))
        case (Failure(_), _)                => x
        case (_, Failure(_))                => y
    }
}

instance Monoid[Validation[t, e]] with Monoid[t] {
    pub def empty(): Validation[t, e] = Success(Monoid.empty())
}

namespace Validation {

    ///
    /// Applies the function in `v1` to the value in `v2`.
    ///
    @Time(1) @Space(1)
    pub def ap(v1: Validation[t -> u \ ef, e], v2: Validation[t, e]): Validation[u, e] \ ef = match (v1, v2) {
        case (Success(f), Success(v)) => Success(f(v))
        case (Success(_), Failure(e)) => Failure(e)
        case (Failure(e), Success(_)) => Failure(e)
        case (Failure(x), Failure(y)) => Failure(Nec.append(x, y))
    }

    ///
    /// Chain two functions, returns the product of their results.
    ///
    pub def product(fa: Validation[t1, e], fb: Validation[t2, e]): Validation[(t1, t2), e] =
        ap(map((a, b) -> (a, b), fa), fb)

    ///
    /// Chain three functions, returns the product of their results.
    ///
    pub def product3(fa: Validation[t1, e], fb: Validation[t2, e], fc: Validation[t3, e]): Validation[(t1, t2, t3), e] =
        ap(ap(map((a, b, c) -> (a, b, c), fa), fb), fc)

    ///
    /// Chain four functions, returns the product of their results.
    ///
    pub def product4(fa: Validation[t1, e], fb: Validation[t2, e], fc: Validation[t3, e], fd: Validation[t4, e]): Validation[(t1, t2, t3, t4), e] =
        ap(ap(ap(map((a, b, c, d) -> (a, b, c, d), fa), fb), fc), fd)

    ///
    /// Chain five functions, returns the product of their results.
    ///
    pub def product5(fa: Validation[t1, e], fb: Validation[t2, e], fc: Validation[t3, e], fd: Validation[t4, e], fe: Validation[t5, e]): Validation[(t1, t2, t3, t4, t5), e] =
        ap(ap(ap(ap(map((a, b, c, d, e) -> (a, b, c, d, e), fa), fb), fc), fd), fe)

    ///
    /// Returns `t` if `v` is `Success(t).` Otherwise returns `d`.
    ///
    @Time(1) @Space(1)
    pub def getWithDefault(d: t, v: Validation[t, e]): t = match v {
        case Success(t) => t
        case Failure(_) => d
    }

    ///
    /// Returns `v` if it is `Success(v)`. Otherwise returns `default`.
    ///
    @Time(1) @Space(1)
    pub def withDefault(default: {default = Validation[t, e]}, v: Validation[t, e]): Validation[t, e] = match v {
        case Success(_) => v
        case Failure(_) => default.default
    }

    ///
    /// Returns `true` if and only if `v` is `Success(t)` and `f(t)` is true.
    ///
    /// Returns `false` if `v` is `Failure`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def exists(f: t -> Bool \ ef, v: Validation[t, e]): Bool \ ef = match v {
        case Success(t) => f(t)
        case Failure(_) => false
    }

    ///
    /// Returns `true` if `v` is `Success(t)` and `f(t)` is true or if `v` is `Failure`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def forall(f: t -> Bool \ ef, v: Validation[t, e]): Bool \ ef = match v {
        case Success(t) => f(t)
        case Failure(_) => true
    }

    ///
    /// Returns `Success(f(v))` if `o` is `Success(v)`. Otherwise returns `v`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def map(f: t -> u \ ef, v: Validation[t, e]): Validation[u, e] \ ef = match v {
        case Success(t) => Success(f(t))
        case Failure(e) => Failure(e)
    }

    ///
    /// Returns `Success(v1 :: v2 :: ... :: vn)` if each of `l_i` is `Success(v_i)`.
    ///
    /// Otherwise returns `Failure(e1 :: ... :: en)` with all of the failures concatenated.
    ///
    pub def sequence(l: List[Validation[t, e]]): Validation[List[t], e] =
        def loop(ll, k) = match ll {
            case Nil              => k(Nil)
            case Success(x) :: xs => loop(xs, ks -> k(x :: ks))
            case Failure(e) :: xs => collectFailuresWith(identity, xs, e)
        };
        loop(l, ks -> Success(ks))

    ///
    /// Returns `Success(v1 :: v2 :: ... v :: vn)` if each of `f(l_i)` is `Success(v_i)`.
    ///
    /// Otherwise returns `Failure(e1 :: ... :: en)` with all of the failures concatenated.
    ///
    pub def traverse(f: a -> Validation[b, e] \ ef, l: List[a]): Validation[List[b], e] \ ef =
        def loop(ll, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs => match f(x) {
                case Success(y) => loop(xs, ks -> k(y :: ks))
                case Failure(y) => collectFailuresWith(f, xs, y)
            }
        };
        loop(l, ks -> Success(ks))

    ///
    /// Returns `Success()` if each of `f(l_i)` is `Success(_)`.
    ///
    /// Otherwise returns `Failure(e1 :: ... :: en)` with all of the failures concatenated.
    ///
    /// This function is the "forgetful" version of `traverse`, use it when the you want the effect
    /// of applying `f` to each element but do not care about collecting the results.
    ///
    pub def traverseX(f: a -> Validation[b, e] \ ef, l: List[a]): Validation[Unit, e] \ ef = match l {
        case Nil     => Success()
        case x :: xs => match f(x) {
            case Success(_) => traverseX(f, xs)
            case Failure(e) => collectFailuresWith(f, xs, e)
        }
    }

    ///
    /// Helper function for `sequence`, `traverse` and `traverseX`. Collects a chain of failures.
    ///
    def collectFailuresWith(f: a -> Validation[b, e] \ ef, l: List[a], acc: Nec[e]): Validation[c, e] \ ef = match l {
        case Nil     => Failure(acc)
        case x :: xs => match f(x) {
            case Success(_) => collectFailuresWith(f, xs, acc)
            case Failure(e) => collectFailuresWith(f, xs, Nec.append(acc, e))
        }
    }

    ///
    /// Converts a Validation to an Option.
    ///
    /// Returns `Some(t)` if `v` is `Success(t)`.
    /// Returns `None` otherwise.
    ///
    @Time(1) @Space(1)
    pub def toOption(v: Validation[t, e]): Option[t] = match v {
        case Success(t) => Some(t)
        case Failure(_) => None
    }

    ///
    /// Converts a Validation to a Result.
    ///
    /// Returns `Ok(t)` if `v` is `Success(t)`.
    /// Returns `Err(e)` if `v` is `Failure(e)`.
    ///
    @Time(1) @Space(1)
    pub def toResult(v: Validation[t, e]): Result[t, Nec[e]] = match v {
        case Success(t) => Ok(t)
        case Failure(e) => Err(e)
    }

    ///
    /// Converts a Validation to a List.
    ///
    /// Returns `t :: Nil` if `v` is `Success(v)`.
    /// Returns `Nil` if `v` is `Failure(e)`.
    ///
    @Time(1) @Space(1)
    pub def toList(v: Validation[t, e]): List[t] = match v {
        case Success(t) => t :: Nil
        case Failure(_) => Nil
    }

    ///
    /// Applies the binary function `f` to the values in `v1` and `v2`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if either or both of `v1` or `v2` are `Failure(xs1)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift2(f: (t1, t2) -> u \ ef, v1: Validation[t1, e], v2: Validation[t2, e]): Validation[u, e] \ ef =
        ap(map(f, v1), v2)

    ///
    /// Applies the ternary function `f` to the values in `v1`, `v2` and `v3`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if any of `v1`, `v2` and `v3` are `Failure(xs1)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift3(f: (t1, t2, t3) -> u \ ef, v1: Validation[t1, e], v2: Validation[t2, e], v3: Validation[t3, e]): Validation[u, e] \ ef =
        ap(lift2(f, v1, v2), v3)

    ///
    /// Applies the 4-ary function `f` to the values in `v1`, `v2`, `v3` and `v4`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if any of `v1`, `v2`, `v3` and `v4` are `Failure(xs1)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift4(f: (t1, t2, t3, t4) -> u \ ef, v1: Validation[t1, e], v2: Validation[t2, e], v3: Validation[t3, e], v4: Validation[t4, e]): Validation[u, e] \ ef=
        ap(lift3(f, v1, v2, v3), v4)

    ///
    /// Applies the 5-ary function `f` to the values in `v1`, `v2`, ... `v5`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if any of `v1`, `v2`, ... `v5` are `Failure(xs1)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift5(f: (t1, t2, t3, t4, t5) -> u \ ef, v1: Validation[t1, e], v2: Validation[t2, e], v3: Validation[t3, e], v4: Validation[t4, e], v5: Validation[t5, e]): Validation[u, e] \ ef =
        ap(lift4(f, v1, v2, v3, v4), v5)

    ///
    /// Applies the 6-ary function `f` to the values in `v1`, `v2`, ... `v6`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if any of `v1`, `v2`, ... `v6` are `Failure(xs1)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift6(f: (t1, t2, t3, t4, t5, t6) -> u \ ef, v1: Validation[t1, e], v2: Validation[t2, e], v3: Validation[t3, e], v4: Validation[t4, e], v5: Validation[t5, e], v6: Validation[t6, e]): Validation[u, e] \ ef =
        ap(lift5(f, v1, v2, v3, v4, v5), v6)

    ///
    /// Applies the 7-ary function `f` to the values in `v1`, `v2`, ... `v7`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if any of `v1`, `v2`, ... `v7` are `Failure(xs1)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift7(f: (t1, t2, t3, t4, t5, t6, t7) -> u \ ef, v1: Validation[t1, e], v2: Validation[t2, e], v3: Validation[t3, e], v4: Validation[t4, e], v5: Validation[t5, e], v6: Validation[t6, e], v7: Validation[t7, e]): Validation[u, e] \ ef=
        ap(lift6(f, v1, v2, v3, v4, v5, v6), v7)

    ///
    /// Applies the 8-ary function `f` to the values in `v1`, `v2`, ... `v8`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if any of `v1`, `v2`, ... `v8` are `Failure(xs1)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift8(f: (t1, t2, t3, t4, t5, t6, t7, t8) -> u \ ef, v1: Validation[t1, e], v2: Validation[t2, e], v3: Validation[t3, e], v4: Validation[t4, e], v5: Validation[t5, e], v6: Validation[t6, e], v7: Validation[t7, e], v8: Validation[t8, e]): Validation[u, e] \ ef =
        ap(lift7(f, v1, v2, v3, v4, v5, v6, v7), v8)

    ///
    /// Applies the 9-ary function `f` to the values in `v1`, `v2`, ... `v9`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if any of `v1`, `v2`, ... `v9` are `Failure(xs1)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift9(f: (t1, t2, t3, t4, t5, t6, t7, t8, t9) -> u \ ef, v1: Validation[t1, e], v2: Validation[t2, e], v3: Validation[t3, e], v4: Validation[t4, e], v5: Validation[t5, e], v6: Validation[t6, e], v7: Validation[t7, e], v8: Validation[t8, e], v9: Validation[t9, e]): Validation[u, e] \ ef =
        ap(lift8(f, v1, v2, v3, v4, v5, v6, v7, v8), v9)

    ///
    /// Applies the 10-ary function `f` to the values in `v1`, `v2`, ... `v10`.
    ///
    /// Returns the concatenation of all the failures as `Failure(xs)` if any of `v1`, `v2`, ... `v10` are `Failure(xs1)`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift10(f: (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) -> u \ ef, v1: Validation[t1, e], v2: Validation[t2, e], v3: Validation[t3, e], v4: Validation[t4, e], v5: Validation[t5, e], v6: Validation[t6, e], v7: Validation[t7, e], v8: Validation[t8, e], v9: Validation[t9, e], v10: Validation[t10, e]): Validation[u, e] \ ef =
        ap(lift9(f, v1, v2, v3, v4, v5, v6, v7, v8, v9), v10)

}

///
/// The Chain type.
///
/// A chain is a list represented as an unbalanced binary tree.
/// It supports efficient append and "snoc" - appending elements at the tail
/// of the list.
///
/// Note - the constructors `Empty`, `One` and `Chain` should not be used directly.
///
pub enum Chain[t] {
    case Empty
    case One(t)
    case Chain(Chain[t], Chain[t])
}

instance Eq[Chain[a]] with Eq[a] {
    pub def eq(c1: Chain[a], c2: Chain[a]): Bool = Chain.equals(c1, c2)
}

instance SemiGroup[Chain[a]] {
    pub def combine(c1: Chain[a], c2: Chain[a]): Chain[a] = Chain.append(c1, c2)
}

instance Monoid[Chain[a]] {
    pub def empty(): Chain[a] = Chain.empty()
}

instance Functor[Chain] {
    pub def map(f: a -> b \ ef, c: Chain[a]): Chain[b] \ ef = Chain.map(f, c)
}

instance Applicative[Chain] {
    pub def point(x: a) : Chain[a] = Chain.singleton(x)
    pub def ap(f: Chain[a -> b \ ef], x: Chain[a]) : Chain[b] \ ef = Chain.ap(f, x)
}

instance Monad[Chain] {
    pub def flatMap(f: a -> Chain[b] \ ef, x : Chain[a]) : Chain[b] \ ef = Chain.flatMap(f, x)
}

instance MonadZero[Chain] {
    pub def empty(): Chain[a] = Chain.empty()
}

instance UnorderedFoldable[Chain] {
    pub def foldMap(f: a -> b \ ef, c: Chain[a]): b \ ef with CommutativeMonoid[b] = Chain.foldMap(f, c)
    override pub def isEmpty(c: Chain[a]): Bool = Chain.isEmpty(c)
    override pub def exists(f: a -> Bool \ ef, c: Chain[a]): Bool \ ef = Chain.exists(f, c)
    override pub def forall(f: a -> Bool \ ef, c: Chain[a]): Bool \ ef = Chain.forall(f, c)
    override pub def memberOf(x: a, c: Chain[a]): Bool with Eq[a] = Chain.memberOf(x, c)
}

instance Filterable[Chain] {
    pub def filterMap(f: a -> Option[b] \ ef, x: Chain[a]): Chain[b] \ ef = Chain.filterMap(f, x)
    pub override def filter(f: a -> Bool \ ef, x: Chain[a]): Chain[a] \ ef = Chain.filter(f, x)
}

namespace Chain {

    ///
    /// A datatype for pattern matching on a chain (traversing left-to-right).
    ///
    pub enum ViewLeft[t] with Eq {
        case NoneLeft
        case SomeLeft(t, Chain[t])
    }

    ///
    /// A datatype for pattern matching on a chain (traversing right-to-left).
    ///
    pub enum ViewRight[t] with Eq {
        case NoneRight
        case SomeRight(Chain[t], t)
    }

    ///
    /// Return the empty chain.
    ///
    pub def empty(): Chain[a] = Empty

    ///
    /// Return the singleton chain with element `x`.
    ///
    pub def singleton(x: a): Chain[a] = One(x)

    ///
    /// Apply every function from `f` to every argument from `x` and return a chain with all results.
    /// For `f = f1, f2, ...` and `x = x1, x2, ...` the results appear in the order
    /// `f1(x1), f1(x2), ..., f2(x1), f2(x2), ...`.
    ///
    pub def ap(f: Chain[a -> b \ ef], x: Chain[a]) : Chain[b] \ ef =
        def loop(g, acc) = match g {
            case NoneLeft        => acc
            case SomeLeft(h, xs) => loop(viewLeft(xs), acc `append` map(h, x))
        };
        loop(viewLeft(f), empty())

    ///
    /// Returns true if and only if `c` is the empty chain.
    ///
    pub def isEmpty(c: Chain[a]): Bool = match c {
        case Empty => true
        case _     => false
    }

    ///
    /// Add element `x` to the left end of chain `c`.
    ///
    pub def cons(x: a, c: Chain[a]): Chain[a] = match c {
        case Empty => One(x)
        case _     => Chain(One(x), c)
    }

    ///
    /// Add element `x` to the right end of chain `c`.
    ///
    pub def snoc(c: Chain[a], x: a): Chain[a] = match c {
        case Empty => One(x)
        case _     => Chain(c, One(x))
    }

    ///
    /// Returns `Some(x)` if `x` is the first element of `c`.
    ///
    /// Returns `None` if `c` is empty.
    ///
    pub def head(c: Chain[a]): Option[a] = match viewLeft(c) {
        case SomeLeft(x, _) => Some(x)
        case _              => None
    }

    ///
    /// Returns `Some(x)` if `x` is the last element of `c`.
    ///
    /// Returns `None` if `c` is empty.
    ///
    pub def last(c: Chain[a]): Option[a] = match viewRight(c) {
        case SomeRight(_, x) => Some(x)
        case _               => None
    }

    ///
    /// Returns the subchain of `c` without the last element.
    /// Returns `None` if the chain `c` is empty.
    ///
    pub def init(c: Chain[a]): Option[Chain[a]] = match viewRight(c) {
        case SomeRight(rs, _) => Some(rs)
        case _                => None
    }

    ///
    /// Returns the length of `c`.
    ///
    pub def length(c: Chain[a]): Int32 = foldRight((_, acc) -> acc + 1, 0, c)

    ///
    /// Returns a new chain formed by appending the chains `c1` and `c2`.
    ///
    pub def append(c1: Chain[a], c2: Chain[a]): Chain[a] = match (c1, c2) {
        case (Empty, c) => c
        case (c, Empty) => c
        case _          => Chain(c1, c2)
    }

    ///
    /// Deconstruct a Chain from left-to-right.
    ///
    /// Returns `ViewLeft(x, rs)` if the chain is non-empty, where `x` is the leftmost
    /// element of the chain `c`, and `rs` is the rest of the chain.
    ///
    /// Returns `NoneLeft` if the chain is empty.
    ///
    pub def viewLeft(c: Chain[a]): ViewLeft[a] =
        def loop(cc, acc, k) = match cc {
            case Empty       => k(NoneLeft)
            case One(x)      => k(SomeLeft(x, acc))
            case Chain(l, r) => loop(l, append(r, acc), k)
        };
        loop(c, Empty, identity)

    ///
    /// Deconstruct a Chain from right-to-left.
    ///
    /// Returns `ViewRight(rs, x)` if the chain is non-empty, where `x` is the rightmost
    /// element of the chain `c``, and `rs` is the front of the chain.
    ///
    /// Returns `NoneRight` if the chain is empty.
    ///
    pub def viewRight(c: Chain[a]): ViewRight[a] =
        def loop(cc, acc, k) = match cc {
            case Empty       => k(NoneRight)
            case One(x)      => k(SomeRight(acc, x))
            case Chain(l, r) => loop(r, append(acc, l),  k)
        };
        loop(c, Empty, identity)

    ///
    /// Returns `true` if and only if `c` contains the element `a`.
    ///
    pub def memberOf(a: a, c: Chain[a]): Bool with Eq[a] = match viewLeft(c) {
        case NoneLeft                 => false
        case SomeLeft(x, _) if x == a => true
        case SomeLeft(_, xs)          => memberOf(a, xs)
    }

    ///
    /// Optionally returns the position of `a` in `c`.
    ///
    pub def indexOf(a: a, c: Chain[a]): Option[Int32] with Eq[a] =
        def loop(v, acc) = match v {
            case NoneLeft        => None
            case SomeLeft(x, xs) => if (x == a) Some(acc) else loop(viewLeft(xs), acc + 1)
        };
        loop(viewLeft(c), 0)

    ///
    /// Alias for `findLeft`.
    ///
    /// The function `f` must be pure.
    ///
    pub def find(f: a -> Bool, c: Chain[a]): Option[a] = findLeft(f, c)

    ///
    /// Optionally returns the first element of `c` that satisfies the predicate `f` when searching from left to right.
    ///
    /// The function `f` must be pure.
    ///
    pub def findLeft(f: a -> Bool, c: Chain[a]): Option[a] = match viewLeft(c) {
        case NoneLeft        => None
        case SomeLeft(x, rs) => if (f(x)) Some(x) else findLeft(f, rs)
    }

    ///
    /// Optionally returns the first element of `c` that satisfies the predicate `f` when searching from right to left.
    ///
    /// The function `f` must be pure.
    ///
    pub def findRight(f: a -> Bool, c: Chain[a]): Option[a] = match viewRight(c) {
        case NoneRight        => None
        case SomeRight(rs, x) => if (f(x)) Some(x) else findRight(f, rs)
    }

    ///
    /// Returns a list of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns `Nil` if `b >= e`.
    ///
    pub def range(b: Int32, e: Int32): Chain[Int32] =
        def loop(i, acc) = if (i >= e) acc else loop(i + 1, snoc(acc, i));
        loop(b, Chain.empty())

    ///
    /// Returns a list with the element `a` repeated `n` times.
    ///
    /// Returns `Nil` if `n < 0`.
    ///
    pub def repeat(n: Int32, a: a): Chain[a] =
        def loop(i, acc) = {
            if (i <= 0)
                acc
            else
                loop(i - 1, cons(a, acc))
        };
        loop(n, Empty)

    ///
    /// Alias for `scanLeft`.
    ///
    pub def scan(f: (b, a) -> b \ ef, s: b, c: Chain[a]): Chain[b] \ ef = scanLeft(f, s, c)

    ///
    /// Accumulates the result of applying `f` to `c` going left to right.
    ///
    /// That is, the result is of the form: `s :: f(s, x1) :: f(f(s, x1), x2)  ...`.
    ///
    pub def scanLeft(f: (b, a) -> b \ ef, s: b, c: Chain[a]): Chain[b] \ ef =
        def loop(a, cc, acc) = match viewLeft(cc) {
            case NoneLeft        => acc
            case SomeLeft(x, xs) =>
                let aa = f(a, x);
                loop(aa, xs, snoc(acc, aa))
        };
        loop(s, c, singleton(s))

    ///
    /// Accumulates the result of applying `f` to `c` going right to left.
    ///
    /// That is, the result is of the form: `... f(xn-1, f(xn, s)) :: f(xn, s) :: s`.
    ///
    pub def scanRight(f: (a, b) -> b \ ef, s: b, c: Chain[a]): Chain[b] \ ef =
        def loop(a, cc, acc) = match viewRight(cc) {
            case NoneRight        => acc
            case SomeRight(xs, x) =>
                let aa = f(x, a);
                loop(aa, xs, cons(aa, acc))
        };
        loop(s, c, singleton(s))

    ///
    /// Returns the result of applying `f` to every element in `c`.
    ///
    /// That is, the result is of the form: `f(x1) :: f(x2) :: ...`.
    ///
    pub def map(f: a -> b \ ef, c: Chain[a]): Chain[b] \ ef =
        def loop(cc, acc) = match viewLeft(cc) {
            case NoneLeft        => acc
            case SomeLeft(a, rs) => loop(rs, snoc(acc, f(a)))
        };
        loop(c, Empty)

    ///
    /// Returns the result of applying `f` to every element in `c` along with that element's index.
    ///
    /// That is, the result is of the form: `f(x1, 0) :: f(x2, 1) :: ...`.
    ///
    pub def mapWithIndex(f: (Int32, a) -> b \ ef, c: Chain[a]): Chain[b] \ ef =
        def loop(cc, i, acc) = match viewLeft(cc) {
            case NoneLeft        => acc
            case SomeLeft(x, rs) =>
                let a = f(i, x);
                loop(rs, i + 1, snoc(acc, a))
        };
        loop(c, 0, Empty)

    ///
    /// Returns the result of applying `f` to every element in `c` and concatenating the results.
    ///
    pub def flatMap(f: a -> Chain[b] \ ef, c: Chain[a]): Chain[b] \ ef =
        def loop(cc, acc) = match viewLeft(cc) {
            case NoneLeft        => acc
            case SomeLeft(a, rs) => loop(rs, append(acc, f(a)))
        };
        loop(c, Empty)

    ///
    /// Returns the reverse of `c`.
    ///
    pub def reverse(c: Chain[a]): Chain[a] =
        def loop(cc, acc) = match viewLeft(cc) {
            case NoneLeft        => acc
            case SomeLeft(a, rs) => loop(rs, cons(a, acc))
        };
        loop(c, Empty)

    ///
    /// Returns `c` with `a` inserted between every two adjacent elements.
    ///
    pub def intersperse(a: a, c: Chain[a]): Chain[a] =
        def loop(cc, acc) = match viewLeft(cc) {
            case NoneLeft        => acc
            case SomeLeft(x, rs) => loop(rs, acc `snoc` a `snoc` x)
        };
        match viewLeft(c) {
            case NoneLeft        => Empty
            case SomeLeft(x, rs) => loop(rs, singleton(x))
        }

    ///
    /// Returns the number of elements in `c` that satisfy the predicate `f`.
    ///
    pub def count(f: a -> Bool \ ef, c: Chain[a]): Int32 \ ef =
        def loop(cc, acc) = match viewLeft(cc) {
            case NoneLeft        => acc
            case SomeLeft(a, rs) => if (f(a)) loop(rs, acc + 1) else loop(rs, acc)
        };
        loop(c, 0)

    ///
    /// Returns the concatenation of the elements in `c`.
    ///
    pub def flatten(c: Chain[Chain[a]]): Chain[a] =
        foldLeft(append, empty(), c)

    ///
    /// Returns `true` if and only if at least one element in `c` satisfies the predicate `f`.
    ///
    /// Returns `false` if `c` is empty.
    ///
    pub def exists(f: a -> Bool \ ef, c: Chain[a]): Bool \ ef = match viewLeft(c) {
        case NoneLeft               => false
        case SomeLeft(x, _) if f(x) => true
        case SomeLeft(_, xs)        => exists(f, xs)
    }

    ///
    /// Returns `true` if and only if all elements in `c` satisfy the predicate `f`.
    ///
    /// Returns `true` if `c` is empty.
    ///
    pub def forall(f: a -> Bool \ ef, c: Chain[a]): Bool \ ef = match viewLeft(c) {
        case NoneLeft                   => true
        case SomeLeft(x, _) if not f(x) => false
        case SomeLeft(_, xs)            => forall(f, xs)
    }

    ///
    /// Returns a list of every element in `c` that satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def filter(f: a -> Bool \ ef, c: Chain[a]): Chain[a] \ ef =
        def loop(cc, acc) = match viewLeft(cc) {
            case NoneLeft        => acc
            case SomeLeft(a, rs) => if (f(a)) loop(rs, snoc(acc, a)) else loop(rs, acc)
        };
        loop(c, Empty)

    ///
    /// Applies `f` to a start value `s` and all elements in `c` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b \ ef, s: b, c: Chain[a]): b \ ef = match viewLeft(c) {
        case NoneLeft        => s
        case SomeLeft(x, xs) => foldLeft(f, f(s, x), xs)
    }

    ///
    /// Applies `f` to a start value `s` and all elements in `c` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    pub def foldRight(f: (a, b) -> b \ ef, s: b, c: Chain[a]): b \ ef = match viewRight(c) {
        case NoneRight        => s
        case SomeRight(xs, x) => foldRight(f, f(x, s), xs)
    }

    ///
    /// Applies `f` to a start value `z` and all elements in `c` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, z))...)`.
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (a, Unit -> b \ ef) -> b \ ef, z: b, c: Chain[a]): b \ ef = match viewLeft(c) {
        case NoneLeft        => z
        case SomeLeft(x, xs) => f(x, _ -> foldRightWithCont(f, z, xs))
    }

    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def foldMap(f: a -> b \ ef, c: Chain[a]): b \ ef with Monoid[b] =
        foldLeft((acc, x) -> Monoid.combine(acc, f(x)), Monoid.empty(), c)

    ///
    /// Returns `c` without the first `n` elements.
    ///
    /// Returns `Nil` if `n > length(c)`.
    /// Returns `c` if `n < 0`.
    ///
    pub def dropLeft(n: Int32, c: Chain[a]): Chain[a] =
        if (n <= 0)
            c
        else
            match viewLeft(c) {
                case NoneLeft        => Empty
                case SomeLeft(_, xs) => dropLeft(n - 1, xs)
            }

    ///
    /// Returns `c` without the last `n` elements.
    ///
    /// Returns `Nil` if `n > length(c)`.
    /// Returns `c` if `n < 0`.
    ///
    pub def dropRight(n: Int32, c: Chain[a]): Chain[a] =
        if (n <= 0)
            c
        else
            match viewRight(c) {
                case NoneRight        => Empty
                case SomeRight(xs, _) => dropRight(n - 1, xs)
            }

    ///
    /// Returns `c` without the longest prefix that satisfies the predicate `f`.
    ///
    pub def dropWhileLeft(f: a -> Bool \ ef, c: Chain[a]): Chain[a] \ ef = match viewLeft(c) {
        case SomeLeft(x, xs) if (f(x)) => dropWhileLeft(f, xs)
        case _                         => c
    }

    ///
    /// Returns `c` without the longest suffix that satisfies the predicate `f`.
    ///
    pub def dropWhileRight(f: a -> Bool \ ef, c: Chain[a]): Chain[a] \ ef = match viewRight(c) {
        case SomeRight(xs, x) if (f(x)) => dropWhileRight(f, xs)
        case _                          => c
    }

    ///
    /// Returns the first `n` elements of `c`.
    ///
    /// Returns `c` if `n > length(c)`.
    /// Returns `Nil` if `n < 0`.
    ///
    pub def takeLeft(n: Int32, c: Chain[a]): Chain[a] =
        def loop(i, cc, acc) = match viewLeft(cc) {
            case NoneLeft        => acc
            case _ if i < 1      => acc
            case SomeLeft(a, rs) => loop(i - 1, rs, snoc(acc, a))
        };
        if (n < 0)
            Empty
        else
            loop(n, c, Empty)

    ///
    /// Returns the last `n` elements of `c`.
    ///
    /// Returns `c` if `n > length(c)`.
    /// Returns `Nil` if `n < 0`.
    ///
    pub def takeRight(n: Int32, c: Chain[a]): Chain[a] =
        def loop(i, cc, acc) = match viewRight(cc) {
            case NoneRight        => acc
            case _ if i < 1       => acc
            case SomeRight(rs, a) => loop(i - 1, rs, cons(a, acc))
        };
        if (n < 0)
            Empty
        else
            loop(n, c, Empty)

    ///
    /// Returns the longest prefix of `c` that satisfies the predicate `f`.
    ///
    pub def takeWhileLeft(f: a -> Bool \ ef, c: Chain[a]): Chain[a] \ ef =
        def loop(cc, acc) = match viewLeft(cc) {
            case NoneLeft        => acc
            case SomeLeft(a, rs) => if (f(a)) loop(rs, snoc(acc, a)) else acc
        };
        loop(c, Empty)

    ///
    /// Returns the longest suffix of `c` that satisfies the predicate `f`.
    ///
    pub def takeWhileRight(f: a -> Bool \ ef, c: Chain[a]): Chain[a] \ ef =
        def loop(cc, acc) = match viewRight(cc) {
            case SomeRight(rs, a) if (f(a)) => loop(rs, cons(a, acc))
            case _                          => acc
        };
        loop(c, Empty)

    ///
    /// Collects the results of applying the partial function `f` to every element in `c`.
    ///
    pub def filterMap(f: a -> Option[b] \ ef, c: Chain[a]): Chain[b] \ ef =
        let step = (acc, x) -> match f(x) {
            case None    => acc
            case Some(v) => snoc(acc, v)
        };
        foldLeft(step, empty(), c)

    ///
    /// Returns the first non-None result of applying the partial function `f` to each element of `c`.
    ///
    /// Returns `None` if every element of `c` is `None`.
    ///
    pub def findMap(f: a -> Option[b] \ ef, c: Chain[a]): Option[b] \ ef = match viewLeft(c) {
        case NoneLeft        => None
        case SomeLeft(x, xs) => match f(x) {
            case None    => findMap(f, xs)
            case Some(v) => Some(v)
        }
    }

    ///
    /// Returns a chain where the element at index `i` is `(a, b)` where
    /// `a` is the element at index `i` in `c1` and `b` is the element at index `i` in `c2`.
    ///
    /// If either `c1` or `c2` becomes depleted, then no further elements are added to the resulting chain.
    ///
    pub def zip(c1: Chain[a], c2: Chain[b]): Chain[(a,b)] =
        def loop(cc1, cc2, acc) = match (viewLeft(cc1), viewLeft(cc2)) {
            case (SomeLeft(a, rs), SomeLeft(b, qs)) => loop(rs, qs, snoc(acc, (a, b)))
            case _                                  => acc
        };
        loop(c1, c2, empty())

    ///
    /// Returns a chain where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `c1` and `b` is the element at index `i` in `c2`.
    ///
    /// If either `c1` or `c2` becomes depleted, then no further elements are added to the resulting chain.
    ///
    pub def zipWith(f: (a, b) -> c \ ef, c1: Chain[a], c2: Chain[b]): Chain[c] \ ef =
        def loop(cc1, cc2, acc) = match (viewLeft(cc1), viewLeft(cc2)) {
            case (SomeLeft(a, rs), SomeLeft(b, qs)) => loop(rs, qs, snoc(acc, f(a, b)))
            case _                                  => acc
        };
        loop(c1, c2, empty())

    ///
    /// Returns a pair of chains, the first containing all first components in `c`
    /// and the second containing all second components in `c`.
    ///
    pub def unzip(c: Chain[(a, b)]): (Chain[a], Chain[b]) =
        def loop(cc, acc1, acc2) = match viewLeft(cc) {
            case NoneLeft             => (acc1, acc2)
            case SomeLeft((a, b), rs) => loop(rs, snoc(acc1, a), snoc(acc2, b))
        };
        loop(c, Chain.empty(), Chain.empty())

    ///
    /// `mapAccumLeft` is a stateful version of `map`. The accumulating paramter `s` is updated at each
    /// step in a left-to-right traversal.
    ///
    pub def mapAccumLeft(f: (s, a) -> (s, b) \ ef, start: s, c: Chain[a]): (s, Chain[b]) \ ef =
        def loop(s1, cc, k) = match viewLeft(cc) {
            case NoneLeft        => k((s1, empty()))
            case SomeLeft(a, rs) => {
                let (s2, b) = f(s1, a);
                loop(s2, rs, match (s3, ks) -> k((s3, cons(b, ks))))
            }
        };
        loop(start, c, identity)

    ///
    /// `mapAccumRight` is a stateful version of `map`. The accumulating parameter `s` is updated at each
    /// step in a right-to-left traversal.
    ///
    pub def mapAccumRight(f: (s, a) -> (s, b) \ ef, start: s, c: Chain[a]): (s, Chain[b]) \ ef =
        def loop(s1, cc, k) = match viewRight(cc) {
            case NoneRight        => k((s1, empty()))
            case SomeRight(rs, a) => {
                let (s2, b) = f(s1, a);
                loop(s2, rs, match (s3, ks) -> k((s3, snoc(ks, b))))
           }
        };
        loop(start, c, identity)

    ///
    /// Applies `f` to every element of `c`.
    ///
    pub def foreach(f: a -> Unit \ ef, c: Chain[a]): Unit \ ef = match viewLeft(c) {
        case NoneLeft => ()
        case SomeLeft(x, xs) => f(x); foreach(f, xs)
    }

    ///
    /// Returns `c` as a list.
    ///
    pub def toList(c: Chain[a]): List[a] =
        foldRight((x, acc) -> x :: acc, Nil, c)

    ///
    /// Returns the chain `c` as a Nel.
    ///
    pub def toNel(c: Chain[a]): Option[Nel[a]] =
        match viewLeft(c) {
            case NoneLeft        => None
            case SomeLeft(x, rs) => Nel(x, Chain.toList(rs)) |> Some
        }

    ///
    /// Returns the chain `c` as a Nec.
    ///
    pub def toNec(c: Chain[a]): Option[Nec[a]] =
        match viewLeft(c) {
            case NoneLeft        => None
            case SomeLeft(x, rs) => foldLeft(Nec.snoc, Nec.singleton(x), rs) |> Some
        }

    ///
    /// Returns `true` if and only if `c1` and `c2` and equal.
    ///
    pub def equals(c1: Chain[a], c2: Chain[a]): Bool with Eq[a] =
        // Note: Chains are considered equal if their (ordered) list of elements are equal.
        // Because they may have different shapes due to construction we use a view to
        // decide equality which imposes an order on the Chain.
        match (viewLeft(c1), viewLeft(c2)) {
            case (NoneLeft, NoneLeft)                           => true
            case (SomeLeft(x, xs), SomeLeft(y, ys)) if (x == y) => equals(xs, ys)
            case _                                              => false
        }

    ///
    /// Helper function for `traverse` and `sequence`.
    ///
    /// Builds an "applicative chain" from an applicative chain of the front of the chain
    /// "snocing" a last element of one applictive action.
    ///
    def snocA(mxs: f[Chain[a]], mx: f[a]): f[Chain[a]] with Applicative[f] =
        (((xs, x) -> snoc(xs, x)) `Functor.map` mxs) `Applicative.ap` mx

    ///
    /// Returns the result of running all the actions in the chain `c`.
    ///
    pub def sequence(c: Chain[m[a]]): m[Chain[a]] with Applicative[m] =
        def loop(v, acc) = match v {
            case NoneLeft        => acc
            case SomeLeft(x, rs) => loop(viewLeft(rs), snocA(acc, x))
        };
        loop(viewLeft(c), Applicative.point(empty()))

    ///
    /// Returns a chain where each element `e` is mapped to `(e, i)` where `i`
    /// is the index of `e`.
    ///
    pub def zipWithIndex(c: Chain[a]): Chain[(a, Int32)] =
        def loop(cc, acc, i) = match viewLeft(cc) {
            case NoneLeft        => acc
            case SomeLeft(a, rs) => loop(rs, snoc(acc, (a, i)), i + 1)
        };
        loop(c, Empty, 0)

}

pub enum DelayList[a] {
    case ENil,
    case ECons(a, DelayList[a]),
    case LCons(a, Lazy[DelayList[a]]),
    case LList(Lazy[DelayList[a]])
}

instance Eq[DelayList[a]] with Eq[a] {
    pub def eq(l1: DelayList[a], l2: DelayList[a]): Bool = match (l1, l2) {
        case (ENil, ENil)                 => true
        case (ECons(x, xs), ECons(y, ys)) => if (x != y) false else xs == ys
        case (ECons(x, xs), LCons(y, ys)) => if (x != y) false else xs == force ys
        case (LCons(x, xs), ECons(y, ys)) => if (x != y) false else (force xs) == ys
        case (LCons(x, xs), LCons(y, ys)) => if (x != y) false else (force xs) == force ys
        case (LList(xs), LList(ys))       => (force xs) == force ys
        case (l, LList(ys))               => l == force ys
        case (LList(xs), l)               => (force xs) == l
        case _                            => false
    }
}

instance Order[DelayList[a]] with Order[a] {

    ///
    /// Compares `l1` and `l2` lexicographically.
    ///
    pub def compare(l1: DelayList[a], l2: DelayList[a]): Comparison = match (l1, l2) {
        case (ENil        ,         ENil) => EqualTo
        case (_           ,         ENil) => GreaterThan
        case (ENil        ,            _) => LessThan
        case (LList(xs)   ,    LList(ys)) => (force xs) <=> (force ys)
        case (_           ,    LList(ys)) =>         l1 <=> (force ys)
        case (LList(xs)   ,            _) => (force xs) <=>        l2
        case (ECons(x, xs), ECons(y, ys)) =>
            let cmp = x <=> y;
            if (cmp == EqualTo) xs <=> ys else cmp
        case (ECons(x, xs), LCons(y, ys)) =>
            let cmp = x <=> y;
            if (cmp == EqualTo) xs <=> (force ys) else cmp
        case (LCons(x, xs), ECons(y, ys)) =>
            let cmp = x <=> y;
            if (cmp == EqualTo) (force xs) <=> ys  else cmp
        case (LCons(x, xs), LCons(y, ys)) =>
            let cmp = x <=> y;
            if (cmp == EqualTo) (force xs) <=> (force ys) else cmp
    }
}

instance UnorderedFoldable[DelayList] {
    pub def foldMap(f: a -> b \ ef, l: DelayList[a]): b \ ef with CommutativeMonoid[b] = DelayList.foldMap(f, l)
    override pub def isEmpty(l: DelayList[a]): Bool = DelayList.isEmpty(l)
    override pub def exists(f: a -> Bool \ ef, l: DelayList[a]): Bool \ ef = DelayList.exists(f, l)
    override pub def forall(f: a -> Bool \ ef, l: DelayList[a]): Bool \ ef = DelayList.forall(f, l)
    override pub def memberOf(x: a, l: DelayList[a]): Bool with Eq[a] = DelayList.memberOf(x, l)
}

instance Functor[DelayList] {
    pub def map(f: a -> b \ ef, l: DelayList[a]): DelayList[b] \ ef = DelayList.map(f, l)
}

instance Applicative[DelayList] {
    pub def point(x: a): DelayList[a] = DelayList.point(x)
    pub def ap(f: DelayList[a -> b \ ef], l: DelayList[a]): DelayList[b] \ ef = DelayList.ap(f, l)
}

instance Monad[DelayList] {
    pub def flatMap(f: a -> DelayList[b] \ ef, l: DelayList[a]): DelayList[b] \ ef = DelayList.flatMap(f, l)
}

instance MonadZero[DelayList] {
    pub def empty(): DelayList[a] = DelayList.empty()
}

instance Filterable[DelayList] {
    pub def filterMap(f: a -> Option[b] \ ef, x: DelayList[a]): DelayList[b] \ ef = DelayList.filterMap(f, x)
    pub override def filter(f: a -> Bool \ ef, x: DelayList[a]): DelayList[a] \ ef = DelayList.filter(f, x)
}

instance SemiGroup[DelayList[a]] {
    pub def combine(l1: DelayList[a], l2: DelayList[a]): DelayList[a] = DelayList.append(l1, l2)
}

instance Monoid[DelayList[a]] {
    pub def empty(): DelayList[a] = ENil
}

namespace DelayList {

    ///
    /// Returns an empty DelayList.
    ///
    @Experimental
    pub def empty(): DelayList[a] = ENil

    ///
    /// Returns true if and only if `l` is the empty DelayList, i.e. `ENil`.
    ///
    /// Does not force the tail of `l`.
    ///
    @Experimental
    pub def isEmpty(l: DelayList[a]): Bool = match l {
        case ENil      => true
        case LList(xs) => isEmpty(force xs)
        case _         => false
    }

    ///
    /// Returns `Some(x)` if `x` is the first element of `l`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    /// Does not force the tail of `l`.
    ///
    @Experimental
    pub def head(l: DelayList[a]): Option[a] = match l {
        case ENil        => None
        case ECons(x, _) => Some(x)
        case LCons(x, _) => Some(x)
        case LList(xs)   => head(force xs)
    }

    ///
    /// Returns `Some(x)` if `x` is the last element of `l`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def last(l: DelayList[a]): Option[a] = match l {
        case ENil         => None
        case ECons(x, xs) => if (isEmpty(      xs)) Some(x) else last(      xs)
        case LCons(x, xs) => if (isEmpty(force xs)) Some(x) else last(force xs)
        case LList(xs)    => last(force xs)
    }

    ///
    /// Returns `l` without the first element.
    ///
    /// Does not force the tail of `l`.
    ///
    @Experimental @Lazy
    pub def tail(l: DelayList[a]): DelayList[a] = match l {
        case ENil         => ENil
        case ECons(_, xs) => xs
        case LCons(_, xs) => LList(xs)
        case LList(xs)    => LList(lazy tail(force xs))
    }

    ///
    /// Returns the length of `l`.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def length(l: DelayList[a]): Int32 =
        def loop(ll, acc) = match ll {
            case ENil         => acc
            case ECons(_, xs) => loop(      xs, acc + 1)
            case LCons(_, xs) => loop(force xs, acc + 1)
            case LList(xs)    => loop(force xs, acc)
        };
        loop(l, 0)

    ///
    /// Returns `l2` appended to `l1`.
    ///
    /// Does not force the tail of `l1`.
    ///
    @Experimental @Lazy
    pub def append(l1: DelayList[a], l2: DelayList[a]): DelayList[a] = match l1 {
        case ENil         => l2
        case ECons(x, xs) => LCons(x, lazy append(      xs, l2))
        case LCons(x, xs) => LCons(x, lazy append(force xs, l2))
        case LList(xs)    => LList(   lazy append(force xs, l2))
    }

    ///
    /// Returns the number of elements in `l` that satisfy the predicate `f`.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def count(f: a -> Bool \ ef, l: DelayList[a]): Int32 \ ef =
        foldLeft((i, x) -> if (f(x)) i + 1 else i, 0, l)

    ///
    /// Returns the concatenation of the elements in `l`.
    ///
    /// Does not force the tail of `l`.
    ///
    @Experimental @Lazy
    pub def flatten(l: DelayList[DelayList[a]]): DelayList[a] = match l {
        case ENil         => ENil
        case ECons(x, xs) => append(x, LList(lazy flatten(      xs)))
        case LCons(x, xs) => append(x, LList(lazy flatten(force xs)))
        case LList(xs)    => LList(lazy flatten(force xs))
    }

    ///
    /// Returns `true` if and only if at least one element in `l` satisfies the predicate `f`.
    ///
    /// Returns `false` if `l` is empty.
    ///
    /// Forces elements of `l` until the predicate `f` is satisfied.
    ///
    @Experimental
    pub def exists(f: a -> Bool \ ef, l: DelayList[a]): Bool \ ef = match l {
        case ENil         => false
        case ECons(x, xs) => if (f(x)) true else exists(f,       xs)
        case LCons(x, xs) => if (f(x)) true else exists(f, force xs)
        case LList(xs)    => exists(f, force xs)
    }

    ///
    /// Returns `true` if and only if all elements in `l` satisfy the predicate `f`.
    ///
    /// Returns `true` if `l` is empty.
    ///
    /// Forces elements in `l` until the first element that does not satisfy the predicate `f` (inclusive).
    ///
    @Experimental
    pub def forall(f: a -> Bool \ ef, l: DelayList[a]): Bool \ ef = match l {
        case ENil         => true
        case ECons(x, xs) => if (f(x)) forall(f,       xs) else false
        case LCons(x, xs) => if (f(x)) forall(f, force xs) else false
        case LList(xs)    =>           forall(f, force xs)
    }

    ///
    /// Returns `true` if and only if `l` contains the element `x`.
    ///
    /// Forces elements until `x` is found.
    ///
    @Experimental
    pub def memberOf(x: a, l: DelayList[a]): Bool with Eq[a] = match l {
        case ENil          => false
        case ECons(x1, xs) => if (x1 == x) true else memberOf(x,       xs)
        case LCons(x1, xs) => if (x1 == x) true else memberOf(x, force xs)
        case LList(xs)     =>                        memberOf(x, force xs)
    }

    ///
    /// Optionally finds the smallest element of `l` according to the `Order` on `a`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def minimum(l: DelayList[a]): Option[a] with Order[a] =
        reduceLeft(Order.min, l)

    ///
    /// Optionally finds the smallest element of `l` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def minimumBy(cmp: (a, a) -> Comparison, l: DelayList[a]): Option[a] =
        reduceLeft(Order.minBy(cmp), l)

    ///
    /// Optionally finds the largest element of `l` according to the `Order` on `a`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def maximum(l: DelayList[a]): Option[a] with Order[a] =
        reduceLeft(Order.max, l)

    ///
    /// Optionally finds the largest element of `l` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def maximumBy(cmp: (a, a) -> Comparison, l: DelayList[a]): Option[a] =
        reduceLeft(Order.maxBy(cmp), l)

    ///
    /// Returns a `DelayList` of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns an empty `DelayList` if `b >= e`.
    ///
    @Experimental @Lazy
    pub def range(b: Int32, e: Int32): DelayList[Int32] =
        def loop(i) = {
            if (i >= e)
                ENil
            else
                LCons(i, lazy loop(i + 1))
        };
        LList(lazy loop(b))

    ///
    /// Returns an infinite DelayList of repeating `x`s.
    ///
    @Experimental @Lazy
    pub def repeat(x: a): DelayList[a] =
        LCons(x, lazy repeat(x))

    ///
    /// Returns an infinite sequence of integers starting from and including `n`.
    ///
    @Experimental @Lazy
    pub def from(n: Int32): DelayList[Int32] =
        def loop(i) = LCons(i, lazy loop(i + 1));
        LList(lazy loop(n))

    ///
    /// Returns the result of applying `f` to every element in `l`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily (i.e. the tail is not forced).
    /// - If `f` is impure then it is applied eagerly (i.e. the entire list `l` is forced).
    ///
    @Experimental @LazyWhenPure
    pub def map(f: a -> b \ ef, l: DelayList[a]): DelayList[b] \ ef =
        reifyEff(f) {
            case Pure(g) => mapL(g, l)
            case _       => mapE(f, l)
        }

    ///
    /// Returns the result of applying `f` to every element in `l`.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    @Lazy
    def mapL(f: a -> b, l: DelayList[a]): DelayList[b] = match l {
        case ENil         => ENil
        case ECons(x, xs) => LCons(f(x), lazy mapL(f,       xs))
        case LCons(x, xs) => LCons(f(x), lazy mapL(f, force xs))
        case LList(xs)    => LList(      lazy mapL(f, force xs))
    }

    ///
    /// Returns the result of applying `f` to every element in `l`.
    ///
    /// Applies `f` eagerly (i.e. the entire list `l` is forced).
    ///
    def mapE(f: a -> b \ ef, l: DelayList[a]): DelayList[b] \ ef =
        def loop(ll, k) = match ll {
            case ENil => k(ENil)
            case ECons(x, xs) =>
                let x1 = f(x);
                loop(xs, ks -> k(ECons(x1, ks)))
            case LCons(x, xs) =>
                let x1 = f(x);
                loop(force xs, ks -> k(ECons(x1, ks)))
            case LList(xs) =>
                loop(force xs, k)
        };
        loop(l, identity)

    ///
    /// Returns the result of applying `f` to every element in `l` along with that element's index.
    ///
    /// That is, the result is of the form: `f(x1, 0) :: f(x2, 1) :: ...`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily (i.e. the tail is not forced).
    /// - If `f` is impure then it is applied eagerly (i.e. the entire list `l` is forced).
    ///
    @Experimental @LazyWhenPure
    pub def mapWithIndex(f: (Int32, a) -> b \ ef, l: DelayList[a]): DelayList[b] \ ef =
        let f1 = (args) -> f(fst(args), snd(args));
        reifyEff(f1) {
            case Pure(g) => mapWithIndexL((i, x) -> g((i, x)), l)
            case _       => mapWithIndexE(f, l)
        }

    ///
    /// Returns the result of applying `f` to every element in `l` along with the element's index.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    @Lazy
    def mapWithIndexL(f: (Int32, a) -> b, l: DelayList[a]): DelayList[b] =
        def loop(ll, i) = match ll {
            case ENil         => ENil
            case ECons(x, xs) => LCons(f(i, x), lazy loop(      xs, i + 1))
            case LCons(x, xs) => LCons(f(i, x), lazy loop(force xs, i + 1))
            case LList(xs)    => LList(         lazy loop(force xs, i    ))
        };
        LList(lazy loop(l, 0))

    ///
    /// Returns the result of applying `f` to every element in `l` along with the element's index.
    ///
    /// Applies `f` eagerly (i.e. the entire list `l` is forced).
    ///
    def mapWithIndexE(f: (Int32, a) -> b \ ef, l: DelayList[a]): DelayList[b] \ ef =
        def loop(ll, i, k) = match ll {
            case ENil => k(ENil)
            case ECons(x, xs) =>
                let x1 = f(i, x);
                loop(xs, i + 1, ks -> k(ECons(x1, ks)))
            case LCons(x, xs) =>
                let x1 = f(i, x);
                loop(force xs, i + 1, ks -> k(ECons(x1, ks)))
            case LList(xs) => loop(force xs, i, k)
        };
        loop(l, 0, identity)

    ///
    /// Returns the result of applying `f` to every element in `l` and concatenating the results.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily (i.e. the tail is not forced).
    /// - If `f` is impure then it is applied eagerly (i.e. the entire list `l` is forced).
    ///
    @Experimental @LazyWhenPure
    pub def flatMap(f: a -> DelayList[b] \ ef, l: DelayList[a]): DelayList[b] \ ef =
        reifyEff(f) {
            case Pure(g) => flatMapL(g, l)
            case _       => flatMapE(f, l)
        }

    ///
    /// Returns the result of applying `f` to every element in `l` and concatenating the results.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    @Lazy
    def flatMapL(f: a -> DelayList[b], l: DelayList[a]): DelayList[b] = match l {
        case ENil         => ENil
        case ECons(x, xs) => append(f(x), LList(lazy flatMapL(f,       xs)))
        case LCons(x, xs) => append(f(x), LList(lazy flatMapL(f, force xs)))
        case LList(xs)    =>              LList(lazy flatMapL(f, force xs))
    }

    ///
    /// Returns the result of applying `f` to every element in `l` and concatenating the results.
    ///
    /// Applies `f` eagerly (i.e. the entire list `l` is forced).
    ///
    def flatMapE(f: a -> DelayList[b] \ ef, l: DelayList[a]): DelayList[b] \ ef =
        def loop(ll, k) = match ll {
            case ENil => k(ENil)
            case ECons(x, xs) =>
                let xs1 = f(x);
                loop(xs, ks -> k(append(xs1, ks)))
            case LCons(x, xs) =>
                let xs1 = f(x);
                loop(force xs, ks -> k(append(xs1, ks)))
            case LList(xs) => loop(force xs, k)
        };
        loop(l, identity)

    ///
    /// Return the singleton list with element `x`.
    ///
    @Experimental
    pub def point(x: a): DelayList[a] = ECons(x, ENil)

    ///
    /// Apply every function from `f` to every argument from `l` and return a list with all results.
    /// For `f = f1, f2, ...` and `l = x1, x2, ...` the results appear in the order
    /// `f1(x1), f1(x2), ..., f2(x1), f2(x2), ...`.
    ///
    /// Whether the i-th function in `f` (`fi`) is applied eagerly or lazily depends on its purity:
    ///
    /// - If `fi` is pure then it is applied lazily (i.e. the tail of `l` is not forced).
    /// - If `fi` is impure then it is applied eagerly (i.e. the entire list `l` is forced).
    ///
    /// Note that this implies that ALL functions in `f` must be pure to avoid forcing `l`.
    ///
    @Experimental @LazyWhenPure
    pub def ap(f: DelayList[a -> b \ ef], l: DelayList[a]): DelayList[b] \ ef =
        flatMap(g -> map(g, l), f)

    ///
    /// Reverses the list `l`.
    ///
    /// Does not force the tail of `l`.
    ///
    @Experimental @Lazy
    pub def reverse(l: DelayList[a]): DelayList[a] =
        def loop(ll, acc) = match ll {
            case ENil         => acc
            case ECons(x, xs) => loop(      xs, ECons(x, acc))
            case LCons(x, xs) => loop(force xs, ECons(x, acc))
            case LList(xs)    => loop(force xs,          acc)
        };
        LList(lazy loop(l, ENil))

    ///
    /// Returns `l` with every occurrence of `from` replaced by `to`.
    ///
    /// Does not force the tail of `l`.
    ///
    @Experimental @Lazy
    pub def replace(from: {from = a}, to: {to = a}, l: DelayList[a]): DelayList[a] with Eq[a] =
        map(e -> if (from.from == e) to.to else e, l)

    ///
    /// Applies `f` to a start value `s` and all elements in `l` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def foldLeft(f: (b, a) -> b \ ef, s: b, l: DelayList[a]): b \ ef = match l {
        case ENil         => s
        case ECons(x, xs) => foldLeft(f, f(s, x),       xs)
        case LCons(x, xs) => foldLeft(f, f(s, x), force xs)
        case LList(xs)    => foldLeft(f,   s,     force xs)
    }

    ///
    /// Applies `f` to a start value `z` and all elements in `l` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, z))...)`.
    ///
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    /// Calling the continuation forces the list `l`.
    ///
    @Experimental
    pub def foldRightWithCont(f: (a, Unit -> b \ ef) -> b \ ef, z: b, l: DelayList[a]): b \ ef =
        def loop(ll) = match ll {
            case ENil         => z
            case ECons(x, xs) => f(x, _ -> loop(xs))
            case LCons(x, xs) => f(x, _ -> loop(force xs))
            case LList(xs)    =>           loop(force xs)
        };
        loop(l)

    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def foldMap(f: a -> b \ ef, l: DelayList[a]): b \ ef with Monoid[b] =
        foldLeft((acc, x) -> Monoid.combine(acc, f(x)), Monoid.empty(), l)

    ///
    /// Applies `f` to every element of `l`.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def foreach(f: a -> Unit \ ef, l: DelayList[a]): Unit \ ef = match l {
        case ENil         => ()
        case ECons(x, xs) => f(x); foreach(f,       xs)
        case LCons(x, xs) => f(x); foreach(f, force xs)
        case LList(xs)    =>       foreach(f, force xs)
    }

    ///
    /// Applies `f` to all elements in `l` going from left to right until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(x1, x2), x3)..., xn))`
    ///
    /// Returns `None` if `l` is empty.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def reduceLeft(f: (a, a) -> a \ ef, l: DelayList[a]): Option[a] \ ef = match l {
        case ENil         => None
        case ECons(x, xs) => Some(foldLeft(f, x,       xs))
        case LCons(x, xs) => Some(foldLeft(f, x, force xs))
        case LList(xs)    => reduceLeft(f, force xs)
    }

    ///
    /// Returns a `DelayList` with every element in `l` that satisfies the predicate `f`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily (i.e. the tail is not forced).
    /// - If `f` is impure then it is applied eagerly (i.e. the entire list `l` is forced).
    ///
    @Experimental @LazyWhenPure
    pub def filter(f: a -> Bool \ ef, l: DelayList[a]): DelayList[a] \ ef =
        reifyEff(f) {
            case Pure(g) => filterL(g, l)
            case _       => filterE(f, l)
        }

    ///
    /// Returns a `DelayList` with every element in `l` that satisfies the predicate `f`.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    @Lazy
    def filterL(f: a -> Bool, l: DelayList[a]): DelayList[a] = match l {
        case ENil         => ENil
        case ECons(x, xs) => if (f(x)) LCons(x, lazy filterL(f,       xs)) else LList(lazy filterL(f,       xs))
        case LCons(x, xs) => if (f(x)) LCons(x, lazy filterL(f, force xs)) else LList(lazy filterL(f, force xs))
        case LList(xs)    =>              LList(lazy filterL(f, force xs))
    }

    ///
    /// Returns a `DelayList` with every element in `l` that satisfies the predicate `f`.
    ///
    /// Applies `f` eagerly (i.e. the entire list `l` is forced).
    ///
    def filterE(f: a -> Bool \ ef, l: DelayList[a]): DelayList[a] \ ef =
        def loop(ll, k) = match ll {
            case ENil         => k(ENil)
            case ECons(x, xs) => if (f(x)) loop(      xs, ks -> k(ECons(x, ks))) else loop(      xs, k)
            case LCons(x, xs) => if (f(x)) loop(force xs, ks -> k(ECons(x, ks))) else loop(force xs, k)
            case LList(xs)    => loop(force xs, k)
        };
        loop(l, identity)

    ///
    /// Collects the results of applying the partial function `f` to every element in `l`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily (i.e. the tail is not forced).
    /// - If `f` is impure then it is applied eagerly (i.e. the entire list `l` is forced).
    ///
    @Experimental @LazyWhenPure
    pub def filterMap(f: a -> Option[b] \ ef, l: DelayList[a]): DelayList[b] \ ef =
        reifyEff(f) {
            case Pure(g) => filterMapL(g, l)
            case _       => filterMapE(f, l)
        }

    ///
    /// Helper function for `filterMap`.
    ///
    /// Collects the results of applying the partial function `f` to every element in `l`.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    @Lazy
    def filterMapL(f: a -> Option[b], l: DelayList[a]): DelayList[b] =
        def loop(ll) = match ll {
            case ENil         => ENil
            case ECons(x, xs) =>
                match f(x) {
                    case None    => loop(xs)
                    case Some(v) => LCons(v, lazy loop(xs))
                }
            case LCons(x, xs) => // Same as above except `xs` is forced.
                match f(x) {
                    case None    => loop(force xs)
                    case Some(v) => LCons(v, lazy loop(force xs))
                }
            case LList(xs) => LList(lazy loop(force xs))
        };
        LList(lazy loop(l))

    ///
    /// Helper function for `filterMap`.
    ///
    /// Collects the results of applying the partial function `f` to every element in `l`.
    ///
    /// Applies `f` eagerly (i.e. the entire list `l` is forced).
    ///
    def filterMapE(f: a -> Option[b] \ ef, l: DelayList[a]): DelayList[b] \ ef =
        def loop(ll, k) = match ll {
            case ENil         => k(ENil)
            case ECons(x, xs) => match f(x) {
                case None    => loop(xs, k)
                case Some(v) => loop(xs, ks -> k(ECons(v, ks)))
            }
            case LCons(x, xs) => match f(x) { // Same as above except `xs` is forced.
                case None    => loop(force xs, k)
                case Some(v) => loop(force xs, ks -> k(ECons(v, ks)))
            }
            case LList(xs) => loop(force xs, k)
        };
        loop(l, identity)

    ///
    /// Optionally returns the first element of `l` that satisfies the predicate `f` when searching from left to right.
    ///
    /// Forces elements of `l` until the predicate `f` is satisfied.
    ///
    @Experimental
    pub def findLeft(f: a -> Bool \ ef, l: DelayList[a]): Option[a] \ ef = match l {
        case ENil         => None
        case ECons(x, xs) => if (f(x)) Some(x) else findLeft(f,       xs)
        case LCons(x, xs) => if (f(x)) Some(x) else findLeft(f, force xs)
        case LList(xs)    =>                        findLeft(f, force xs)
    }

    ///
    /// Returns the first non-None result of applying the partial function `f` to each element of `l`.
    ///
    /// Returns `None` if every element `f(x)` of `l` is `None`.
    ///
    /// Forces elements of `l` until `f(x)` returns `Some(v)`.
    ///
    @Experimental
    pub def findMap(f: a -> Option[b] \ ef, l: DelayList[a]): Option[b] \ ef = match l {
        case ENil         => None
        case ECons(x, xs) => match f(x) {
            case None    => findMap(f, xs)
            case Some(v) => Some(v)
        }
        case LCons(x, xs) => match f(x) { // Same as above except `xs` is forced.
            case None    => findMap(f, force xs)
            case Some(v) => Some(v)
        }
        case LList(xs) => findMap(f, force xs)
    }

    ///
    /// Returns `l` with `x` inserted between every two adjacent elements.
    ///
    /// Does not force the tail of `l`.
    ///
    @Experimental @Lazy
    pub def intersperse(x: a, l: DelayList[a]): DelayList[a] = match l {
        case ENil           => ENil
        case ECons(x1, xs)  => if (isEmpty(      xs)) l else LCons(x1, lazy LCons(x, lazy intersperse(x,       xs)))
        case LCons(x1, xs)  => if (isEmpty(force xs)) l else LCons(x1, lazy LCons(x, lazy intersperse(x, force xs)))
        case LList(xs)      => LList(lazy intersperse(x, force xs))
    }

    ///
    /// Returns the concatenation of the elements in `l2` with the elements
    /// of `l1` inserted between every two adjacent elements of `l2`.
    ///
    /// That is, returns `l2.1 :: l1.1 ... l1.n :: l2.2 :: ... :: l2.n-1 :: l1.1 :: ... :: l1.n :: l2.n :: ENil`.
    ///
    /// Does not force the tail of `l2`.
    ///
    @Experimental @Lazy
    pub def intercalate(l1: DelayList[a], l2: DelayList[DelayList[a]]): DelayList[a] = match l2 {
        case ENil           => ENil
        case ECons(x, xs)   => if (isEmpty(      xs)) x else append(append(x, l1), intercalate(l1,       xs))
        case LCons(x, xs)   => if (isEmpty(force xs)) x else append(append(x, l1), intercalate(l1, force xs))
        case LList(xs)      =>                                          LList(lazy intercalate(l1, force xs))
    }

    ///
    /// Returns a pair of lists `(l1, l2)` where:
    /// - `l1` contains all elements of `l` that satisfy the predicate `f`.
    /// - `l2` contains all elements of `l` that DO NOT satisfy the predicate `f`.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def partition(f: a -> Bool \ ef, l: DelayList[a]): (DelayList[a], DelayList[a]) \ ef =
        def loop(ll, k) = match ll {
            case ENil         => k((ENil, ENil))
            case ECons(x, xs) =>
                if (f(x))
                    loop(xs, match (ks, ls) -> k((ECons(x, ks), ls)))
                else
                    loop(xs, match (ks, ls) -> k((ks, ECons(x, ls))))
            case LCons(x, xs) => // Same as above except `xs` is forced.
                if (f(x))
                    loop(force xs, match (ks, ls) -> k((ECons(x, ks), ls)))
                else
                    loop(force xs, match (ks, ls) -> k((ks, ECons(x, ls))))
            case LList(xs) => loop(force xs, k)
        };
        loop(l, identity)

    ///
    /// Returns a pair of lists `(l1, l2)` where:
    /// - `l1` is the longest prefix of `l` that satisfies the predicate `f`.
    /// - `l2` is the remainder of `l`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily (i.e. the tail is not forced).
    /// - If `f` is impure then it is applied eagerly (i.e. the entire list `l` is forced).
    ///
    @Experimental @LazyWhenPure
    pub def span(f: a -> Bool \ ef, l: DelayList[a]): (DelayList[a], DelayList[a]) \ ef =
        reifyEff(f) {
            case Pure(g) => spanL(g, l)
            case _       => spanE(f, l)
        }

    ///
    /// Helper function for `span`.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    @Lazy
    def spanL(f: a -> Bool, l: DelayList[a]): (DelayList[a], DelayList[a]) = match l {
        case ENil         => (ENil, ENil)
        case ECons(x, xs) =>
            if (f(x))
                let t = lazy spanL(f, xs);
                (LCons(x, lazy fst(force t)), LList(lazy snd(force t)))
            else
                (ENil, l)
        case LCons(x, xs) => // Same as above except `xs` is forced.
            if (f(x))
                let t = lazy spanL(f, force xs);
                (LCons(x, lazy fst(force t)), LList(lazy snd(force t)))
            else
                (ENil, l)
        case LList(xs) => spanL(f, force xs)
    }

    ///
    /// Helper function for `span`.
    ///
    /// Applies `f` eagerly (i.e. the entire list `l` is forced).
    ///
    def spanE(f: a -> Bool \ ef, l: DelayList[a]): (DelayList[a], DelayList[a]) \ ef =
        def loop(ll, k) = match ll {
            case ENil         => k((ENil, ENil))
            case ECons(x, xs) =>
                if (f(x))
                    loop(xs, match (ks, ls) -> k((ECons(x, ks), ls)))
                else
                    k((ENil, l))
            case LCons(x, xs) => // Same as above except `xs` is forced.
                if (f(x))
                    loop(force xs, match (ks, ls) -> k((ECons(x, ks), ls)))
                else
                    k((ENil, l))
            case LList(xs) => loop(force xs, k)
        };
        loop(l, identity)

    ///
    /// Returns `l` without the first `n` elements.
    ///
    /// Returns `ENil` if `n > length(l)`.
    /// Returns `l` if `n < 1`.
    ///
    /// Does not force the tail of `l`.
    ///
    @Experimental @Lazy
    pub def drop(n: Int32, l: DelayList[a]): DelayList[a] =
        def loop(i, ll) = { // Inner function used here to allow for early termination
            if (i < 1)
                ll
            else
                match ll {
                    case ENil         => ll
                    case ECons(_, xs) => loop(i - 1,       xs)
                    case LCons(_, xs) => loop(i - 1, force xs)
                    case LList(xs)    => loop(i,     force xs)
                }
        };
        LList(lazy loop(n, l))

    ///
    /// Returns `l` without the longest prefix that satisfies the predicate `f`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily (i.e. the tail is not forced).
    /// - If `f` is impure then it is applied eagerly (i.e. the tail is forced until the first element that satisfies `f`).
    ///
    @Experimental @LazyWhenPure
    pub def dropWhile(f: a -> Bool \ ef, l: DelayList[a]): DelayList[a] \ ef =
        reifyEff(f) {
            case Pure(g) => dropWhileL(g, l)
            case _       => dropWhileE(f, l)
        }

    ///
    /// Helper function for `dropWhile`.
    ///
    /// Returns `l` without the longest prefix that satisfies the predicate `f`.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    @Lazy
    def dropWhileL(f: a -> Bool, l: DelayList[a]): DelayList[a] =
        def loop(ll) = match ll { // Inner function used here to allow for early termination
            case ENil         => ENil
            case ECons(x, xs) => if (f(x)) loop(      xs) else ll
            case LCons(x, xs) => if (f(x)) loop(force xs) else ll
            case LList(xs)    => loop(force xs)
        };
        LList(lazy loop(l))

    ///
    /// Helper function for `dropWhile`.
    ///
    /// Returns `l` without the longest prefix that satisfies the predicate `f`.
    ///
    /// Applies `f` eagerly (i.e. the tail is forced until the first element that satisfies `f`).
    ///
    def dropWhileE(f: a -> Bool \ ef, l: DelayList[a]): DelayList[a] \ ef = match l {
        case ENil         => ENil
        case ECons(x, xs) => if (f(x)) dropWhileE(f,       xs) else l
        case LCons(x, xs) => if (f(x)) dropWhileE(f, force xs) else l
        case LList(xs)    =>           dropWhileE(f, force xs)
    }

    ///
    /// Returns the first `n` elements of `l`.
    ///
    /// Does not force the tail of `l`.
    ///
    @Experimental @Lazy
    pub def take(n: Int32, l: DelayList[a]): DelayList[a] =
        def loop(i, ll) = { // Inner function used here to allow for early termination
            if (i <= 0)
                ENil
            else
                match ll {
                    case ENil         => ENil
                    case ECons(x, xs) => LCons(x, lazy loop(i - 1,       xs))
                    case LCons(x, xs) => LCons(x, lazy loop(i - 1, force xs))
                    case LList(xs)    => loop(i, force xs)
                }
        };
        LList(lazy loop(n, l))

    ///
    /// Returns the longest prefix of `l` that satisfies the predicate `f`.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily (i.e. the tail is not forced).
    /// - If `f` is impure then it is applied eagerly (i.e. the tail is forced until the first element that satisfies `f`).
    ///
    @Experimental @LazyWhenPure
    pub def takeWhile(f: a -> Bool \ ef, l: DelayList[a]): DelayList[a] \ ef =
        reifyEff(f) {
            case Pure(g) => takeWhileL(g, l)
            case _       => takeWhileE(f, l)
        }

    ///
    /// Helper function for `takeWhile`.
    ///
    /// Returns the longest prefix of `l` that satisfies the predicate `f`.
    ///
    /// Applies `f` lazily (i.e. the tail is not forced).
    ///
    @Lazy
    def takeWhileL(f: a -> Bool, l: DelayList[a]): DelayList[a] =
        def loop(ll) = match ll { // Inner function used here to allow for early termination
            case ENil         => ENil
            case ECons(x, xs) => if (f(x)) LCons(x, lazy loop(      xs)) else ENil
            case LCons(x, xs) => if (f(x)) LCons(x, lazy loop(force xs)) else ENil
            case LList(xs)    => loop(force xs)
        };
        LList(lazy loop(l))

    ///
    /// Helper function for `takeWhile`.
    ///
    /// Returns the longest prefix of `l` that satisfies the predicate `f`.
    ///
    /// Applies `f` eagerly (i.e. the tail is forced until the first element that satisfies `f`).
    ///
    def takeWhileE(f: a -> Bool \ ef, l: DelayList[a]): DelayList[a] \ ef =
        def loop(ll, k) = match ll {
            case ENil         => k(ENil)
            case ECons(x, xs) => if (f(x)) loop(      xs, ks -> k(ECons(x, ks))) else k(ENil)
            case LCons(x, xs) => if (f(x)) loop(force xs, ks -> k(ECons(x, ks))) else k(ENil)
            case LList(xs)    => loop(force xs, k)
        };
        loop(l, identity)

    ///
    /// Returns a list where the element at index `i` is `(a, b)` where
    /// `a` is the element at index `i` in `l1` and `b` is the element at index `i` in `l2`.
    ///
    /// If either `l1` or `l2` is depleted, then no further elements are added to the resulting list.
    ///
    /// Does not force the tail of either `l1` or `l2`.
    ///
    @Experimental @Lazy
    pub def zip(l1: DelayList[a], l2: DelayList[b]): DelayList[(a, b)] =
        def loop(ll1, ll2) = match (ll1, ll2) { // Inner function used here to allow for early termination
            case (ENil, _) => ENil
            case (_, ENil) => ENil
            case (ECons(x, xs), ECons(y, ys)) => LCons((x, y), lazy loop(      xs,       ys))
            case (ECons(x, xs), LCons(y, ys)) => LCons((x, y), lazy loop(      xs, force ys))
            case (LCons(x, xs), ECons(y, ys)) => LCons((x, y), lazy loop(force xs,       ys))
            case (LCons(x, xs), LCons(y, ys)) => LCons((x, y), lazy loop(force xs, force ys))
            case (LList(xs), LList(ys))       => LList(        lazy loop(force xs, force ys))
            case (xs, LList(ys))              => LList(        lazy loop(      xs, force ys))
            case (LList(xs), ys)              => LList(        lazy loop(force xs,       ys))
        };
        match (l1, l2) {
            case (ENil, _) => ENil
            case (_, ENil) => ENil
            case _         => LList(lazy loop(l1, l2))
        }

    ///
    /// Returns a list where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `l1` and `b` is the element at index `i` in `l2`.
    ///
    /// If either `l1` or `l2` is depleted, then no further elements are added to the resulting list.
    ///
    /// Whether `f` is applied eagerly or lazily depends on its purity:
    ///
    /// - If `f` is pure then it is applied lazily (i.e. the tails are not forced).
    /// - If `f` is impure then it is applied eagerly (i.e. both lists `l1` and `l2` are forced).
    ///
    @Experimental @LazyWhenPure
    pub def zipWith(f: (a, b) -> c \ ef, l1: DelayList[a], l2: DelayList[b]): DelayList[c] \ ef =
        map(x -> f(fst(x), snd(x)), zip(l1, l2))

    ///
    /// Returns a `DelayList` where each element `e` is mapped to `(e, i)` where `i`
    /// is the index of `e`.
    ///
    /// Does not force the tail of `l`.
    ///
    pub def zipWithIndex(l: DelayList[a]): DelayList[(a, Int32)] =
        def loop(ll, i) = match ll {
            case ENil         => ENil
            case ECons(x, xs) => LCons(((x, i)), lazy loop(      xs, i + 1))
            case LCons(x, xs) => LCons(((x, i)), lazy loop(force xs, i + 1))
            case LList(xs)    => LList(          lazy loop(force xs, i))
        };
        loop(l, 0)

    ///
    /// Returns `l` as a `List`.
    ///
    /// Forces the entire list `l`.
    ///
    @Experimental
    pub def toList(l: DelayList[a]): List[a] =
        def loop(ll, k) = match ll {
            case ENil         => k(Nil)
            case ECons(x, xs) => loop(      xs, ks -> k(x :: ks))
            case LCons(x, xs) => loop(force xs, ks -> k(x :: ks))
            case LList(xs)    => loop(force xs, k)
        };
        loop(l, identity)

    ///
    /// Helper function for `traverse` and `sequence`.
    ///
    /// Builds an "applicative DelayList" from a head of one applicative action and an
    /// applicative DelayList of the tail.
    ///
    @Experimental
    def consA(mx: f[a], ml: f[DelayList[a]]): f[DelayList[a]] with Applicative[f] =
        (((x, xs) -> ECons(x, xs)) `Functor.map` mx) `Applicative.ap` ml

    ///
    /// Returns the result of running all the actions in the DelayList `l`.
    ///
    @Experimental
    pub def sequence(l: DelayList[m[a]]): m[DelayList[a]] with Applicative[m] =
        def loop(ll, k) = match ll {
            case ENil          => k(Applicative.point(ENil))
            case ECons(mx, xs) => loop(      xs, ks -> k(consA(mx, ks)))
            case LCons(mx, xs) => loop(force xs, ks -> k(consA(mx, ks)))
            case LList(xs)     => loop(force xs, k)
        };
        loop(l, identity)

}


///
/// The Option type.
///
/// An option represents an optional value.
/// The constructor `None` represents an absent value,
/// whereas the constructor `Some(v)` represents the value `v`.
///
pub enum Option[t] with Eq, Order, ToString {
    case None,
    case Some(t)
}

instance LowerBound[Option[t]] {
    pub def minValue(_: Unit): Option[t] = None
}

instance PartialOrder[Option[t]] with PartialOrder[t] {
    pub def lessEqual(x: Option[t], y: Option[t]): Bool = match (x,y) {
       case (None, _)           => true
       case (Some(a), Some(b))  => PartialOrder.lessEqual(a,b)
       case _                   => false
   }
}

instance JoinLattice[Option[t]] with JoinLattice[t] {
    pub def leastUpperBound(x: Option[t], y: Option[t]): Option[t] = match (x,y) {
       case (None, _)           => y
       case (_, None)           => x
       case (Some(a), Some(b))  => Some(JoinLattice.leastUpperBound(a,b))
   }
}

instance MeetLattice[Option[t]] with MeetLattice[t] {
    pub def greatestLowerBound(x: Option[t], y: Option[t]): Option[t] = match (x,y) {
       case (None, _)           => None
       case (_, None)           => None
       case (Some(a), Some(b))  => Some(MeetLattice.greatestLowerBound(a, b))
   }
}

instance Hash[Option[a]] with Hash[a] {
    pub def hash(o: Option[a]): Int32 = match o {
        case None    => 1046527
        case Some(y) => 919 * Hash.hash(y)
    }
}

instance Functor[Option] {
    pub def map(f: a -> b \ ef, o: Option[a]): Option[b] \ ef = Option.map(f, o)
}

instance Applicative[Option] {
    pub def point(x: a) : Option[a] = Option.point(x)

    pub def ap(f: Option[a -> b \ ef], x: Option[a]) : Option[b] \ ef = Option.ap(f, x)

    // Same as the default implementation using `map` and `ap` but with less indirection.
    override pub def liftA2(f: t1 -> t2 -> r \ ef, x1: Option[t1], x2: Option[t2]): Option[r] \ ef = Option.lift2(f, x1, x2)
    override pub def liftA3(f: t1 -> t2 -> t3 -> r \ ef, x1: Option[t1], x2: Option[t2], x3: Option[t3]): Option[r] \ ef = Option.lift3(f, x1, x2, x3)
    override pub def liftA4(f: t1 -> t2 -> t3 -> t4 -> r \ ef, x1: Option[t1], x2: Option[t2], x3: Option[t3], x4: Option[t4]): Option[r] \ ef = Option.lift4(f, x1, x2, x3, x4)
    override pub def liftA5(f: t1 -> t2 -> t3 -> t4 -> t5 -> r \ ef, x1: Option[t1], x2: Option[t2], x3: Option[t3], x4: Option[t4], x5: Option[t5]): Option[r] \ ef = Option.lift5(f, x1, x2, x3, x4, x5)
}

instance Monad[Option] {
    pub def flatMap(f: a -> Option[b] \ ef, x : Option[a]) : Option[b] \ ef = Option.flatMap(f, x)
}

instance MonadZero[Option] {
    pub def empty(): Option[a] = None
}

instance MonadZip[Option] {
    pub def zipWith(f: (a, b) -> c \ ef, ma: Option[a], mb: Option[b]): Option[c] \ ef = Applicative.liftA2(f, ma, mb)
    pub override def zip(ma: Option[a], mb: Option[b]): Option[(a, b)] = Applicative.liftA2((x, y) -> (x, y), ma, mb)

    pub def zipWithA(f: (a, b) -> f[c] \ ef, x: Option[a], y: Option[b]): f[Option[c]] \ ef with Applicative[f] =
        use Functor.{<$>};
        match (x, y) {
            case (Some(x1), Some(y1)) => Some <$> f(x1, y1)
            case (_, _)               => Applicative.point(None)
        }
}

instance UnorderedFoldable[Option] {
    pub def foldMap(f: a -> b \ ef, o: Option[a]): b \ ef with CommutativeMonoid[b] = Option.foldMap(f, o)
}

instance Filterable[Option] {
    pub def filterMap(f: a -> Option[b] \ ef, o: Option[a]): Option[b] \ ef = Option.flatMap(f, o)
    pub override def filter(f: a -> Bool \ ef, o: Option[a]): Option[a] \ ef = Option.filter(f, o)
}

instance SemiGroup[Option[a]] with SemiGroup[a] {
    pub def combine(x: Option[a], y: Option[a]): Option[a] = match (x, y) {
        case (a, None)              => a
        case (None, b)              => b
        case (Some(x1), Some(y1))   => Some(SemiGroup.combine(x1, y1))
    }
}

instance CommutativeSemiGroup[Option[a]] with CommutativeSemiGroup[a]

instance Monoid[Option[a]] with Monoid[a] {
    pub def empty(): Option[a] = None
}

instance CommutativeMonoid[Option[a]] with CommutativeMonoid[a]

namespace Option {

    ///
    /// Returns `true` iff `o` is `None`.
    ///
    @Time(1) @Space(1)
    pub def isEmpty(o: Option[a]): Bool = match o {
        case None    => true
        case Some(_) => false
    }

    ///
    /// Returns `v` if `o` is `Some(v).` Otherwise returns `d`.
    ///
    @Time(1) @Space(1)
    pub def getWithDefault(d: a, o: Option[a]): a = match o {
        case None    => d
        case Some(v) => v
    }

    ///
    /// Returns `o` if it is `Some(v)`. Otherwise returns `default`.
    ///
    @Time(1) @Space(1)
    pub def withDefault(default: {default = Option[a]}, o: Option[a]): Option[a] = match o {
        case None    => default.default
        case Some(_) => o
    }

    ///
    /// Returns `Some(to)` if `o` is `Some(from)`. Otherwise returns `o`.
    ///
    @Time(1) @Space(1)
    pub def replace(from: {from = a}, to: {to = a}, o: Option[a]): Option[a] with Eq[a] = match o {
        case None    => o
        case Some(v) => Some(if (v == from.from) to.to else v)
    }

    ///
    /// Returns `true` if `o` is `Some(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `false`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def exists(f: a -> Bool \ ef, o: Option[a]): Bool \ ef = match o {
        case None    => false
        case Some(v) => f(v)
    }

    ///
    /// Returns `true` if `o` is `Some(v)` and the predicate `f(v)` evaluates to `true` or if `o` is `None`.
    ///
    /// Otherwise returns `false`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def forall(f: a -> Bool \ ef, o: Option[a]): Bool \ ef = match o {
        case None    => true
        case Some(v) => f(v)
    }

    ///
    /// Returns `o` if `o` is `Some(v)` and the predicate `f(v)` is true. Otherwise returns `None`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def filter(f: a -> Bool \ ef, o: Option[a]): Option[a] \ ef = match o {
        case None    => None
        case Some(v) => if (f(v)) o else None
    }

    ///
    /// Returns `Some(f(v))` if `o` is `Some(v)`. Otherwise returns `None`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def map(f: a -> b \ ef, o: Option[a]): Option[b] \ ef = match o {
        case None    => None
        case Some(v) => Some(f(v))
    }

    ///
    /// Returns `Some(x)`.
    ///
    pub def point(x: a) : Option[a] = Some(x)

    ///
    /// If both arguments are `Some`, return a `Some` containing the result of applying the function inside
    /// `f` to the value inside `x`. Otherwise return `None`.
    ///
    pub def ap(f: Option[a -> b \ ef], x: Option[a]) : Option[b] \ ef =
        match f {
            case None    => None
            case Some(g) => match x {
                case None    => None
                case Some(y) => Some(g(y))
            }
        }

    ///
    /// Returns `f(v)` if `o` is `Some(v)`. Otherwise returns `None`.
    ///
    pub def flatMap(f: a -> Option[b] \ ef, o: Option[a]): Option[b] \ ef = match o {
        case None    => None
        case Some(v) => f(v)
    }

    ///
    /// Returns `1` if `o` is `Some(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `0`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def count(f: a -> Bool \ ef, o: Option[a]): Int32 \ ef = match o {
        case None    => 0
        case Some(v) => if (f(v)) 1 else 0
    }


    ///
    /// Returns `o` if `o` is `Some(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `None`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f)) @Space(space(f))
    pub def find(f: a -> Bool, o: Option[a]): Option[a] = match o {
        case None    => None
        case Some(v) => if (f(v)) o else None
    }

    ///
    /// Returns `v` if `o` is `Some(v)`. Otherwise returns `None`.
    ///
    @Time(1) @Space(1)
    pub def flatten(o: Option[Option[a]]): Option[a] = match o {
        case None    => None
        case Some(v) => v
    }

    ///
    /// Returns `f(z, v)` if `o` is `Some(v)`. Otherwise returns `z`.
    ///
    pub def foldLeft(f: (b, a) -> b \ ef, z: b, o: Option[a]): b \ ef = match o {
        case None    => z
        case Some(v) => f(z, v)
    }

    ///
    /// Returns `f(v, z)` if `o` is `Some(v)`. Otherwise returns `z`.
    ///
    pub def foldRight(f: (a, b) -> b \ ef, o: Option[a], z: b): b \ ef = match o {
        case None    => z
        case Some(v) => f(v, z)
    }

    ///
    /// Returns `f(v, z)` if `o` is `Some(v)`. Otherwise returns `z`.
    ///
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (a, Unit -> b \ ef) -> b \ ef, o: Option[a], z: b): b \ ef = match o {
        case None    => z
        case Some(v) => f(v, upcast(_ -> z))
    }

    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def foldMap(f: a -> b \ ef, o: Option[a]): b \ ef with Monoid[b] =
        foldLeft((acc, x) -> Monoid.combine(acc, f(x)), Monoid.empty(), o)

    ///
    /// Returns `Some(v1 :: v2 :: ... :: vn)` if each of `xs_i` is `Some(v_i)`. Otherwise returns `None`.
    ///
    @Time(List.length(l)) @Space(List.length(l))
    pub def sequence(l: List[Option[a]]): Option[List[a]] =
        def loop(ll, k) = match ll {
            case Nil            => k(Nil)
            case None :: _      => None
            case Some(x) :: xs  => loop(xs, ks -> k(x :: ks))
        };
        loop(l, ks -> Some(ks))

    ///
    /// Returns `Some(v1 :: v2 :: ... v :: vn)` if each of `f(l_i)` is `Some(v_i)`. Otherwise returns `None`.
    ///
    pub def traverse(f: a -> Option[b] \ ef, l: List[a]): Option[List[b]] \ ef =
        def loop(ll, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs => match f(x) {
                case None    => None
                case Some(y) => loop(xs, ys -> k(y :: ys))
            }
        };
        loop(l, ks -> Some(ks))

    ///
    /// Returns `Some()` if each of `f(l_i)` is `Some(_)`. Otherwise returns `None`.
    ///
    /// This function is the "forgetful" version of `traverse`, use it when the you want the effect
    /// of applying `f` to each element but do not care about collecting the results.
    ///
    pub def traverseX(f: a -> Option[b] \ ef, l: List[a]): Option[Unit] \ ef = match l {
        case Nil     => Some()
        case x :: xs => match f(x) {
            case None    => None
            case Some(_) => traverseX(f, xs)
        }
    }

    ///
    /// Returns the result of applying `f` to a start value `s` and the elements in `l`
    /// going from left to right.
    ///
    /// If at any step applying `f` fails (i.e. it produces a `None` value) the traversal
    /// of `l` is short-circuited and `None` is returned.
    ///
    /// If `f` is successfully applied to all the elements in `l` the result is of the form:
    /// `Some(f(...f(f(s, x1), x2)..., xn))`.
    ///
    pub def foldLeftM(f: (b, a) -> Option[b] \ ef, s: b, l: List[a]): Option[b] \ ef = match l {
        case Nil     => Some(s)
        case x :: xs => match f(s, x) {
            case Some(s1) => foldLeftM(f, s1, xs)
            case None     => None
        }
    }

    ///
    /// Returns a one-element list of the value `v` if `o` is `Some(v)`. Otherwise returns the empty list.
    ///
    @Time(1) @Space(1)
    pub def toList(o: Option[a]): List[a] = match o {
        case None    => Nil
        case Some(v) => v :: Nil
    }

    ///
    /// Returns the Option value `Ok(v)` if `o` is `Some(v)`. Otherwise returns `Err(e)`.
    ///
    @Time(1) @Space(1)
    pub def toOk(e: e, o: Option[t]): Result[t, e] = match o {
        case None    => Err(e)
        case Some(a) => Ok(a)
    }

    ///
    /// Returns the Option value `Err(e)` if `o` is `Some(e)`. Otherwise returns `Ok(d)`.
    ///
    @Time(1) @Space(1)
    pub def toErr(d: t, o: Option[e]): Result[t, e] = match o {
        case None    => Ok(d)
        case Some(e) => Err(e)
    }

    ///
    /// Returns the Validation value `Success(v)` if `o` is `Some(v)`. Otherwise lifts `e` into Validation's `Failure`.
    ///
    @Time(1) @Space(1)
    pub def toSuccess(e: e, o: Option[t]): Validation[t, e] = match o {
        case None    => Failure(Nec.singleton(e))
        case Some(a) => Success(a)
    }

    ///
    /// Returns `e` into Validation's `Failure` if `o` is `Some(e)`. Otherwise returns `Success(d)`.
    ///
    @Time(1) @Space(1)
    pub def toFailure(d: t, o: Option[e]): Validation[t, e] = match o {
        case None    => Success(d)
        case Some(e) => Failure(Nec.singleton(e))
    }

    ///
    /// Returns `Some((v1, v2))` if `o1` is `Some(v1)` and `o2` is `Some(v2)`. Otherwise returns `None`.
    ///
    @Time(1) @Space(1)
    pub def zip(o1: Option[a], o2: Option[b]): Option[(a, b)] = match (o1, o2) {
        case (None, _)            => None
        case (_, None)            => None
        case (Some(v1), Some(v2)) => Some((v1, v2))
    }

    ///
    /// Returns `(Some(v1), Some(v2))` if `o` is `Some((v1, v2))`. Otherwise returns `(None, None)`.
    ///
    @Time(1) @Space(1)
    pub def unzip(o: Option[(a, b)]): (Option[a], Option[b]) = match o {
        case None           => (None, None)
        case Some((v1, v2)) => (Some(v1), Some(v2))
    }

    ///
    /// Applies `f` to `v` if `o` is `Some(v)`. Otherwise does nothing.
    ///
    @Time(time(f)) @Space(space(f))
    pub def foreach(f: a -> Unit \ ef, o: Option[a]): Unit \ ef = match o {
        case None    => ()
        case Some(v) => f(v)
    }

    ///
    /// Applies the binary function `f` to the values in `o1` and `o2`.
    ///
    /// Returns `None` if either `o1` or `o2` are `None`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift2(f: (t1, t2) -> u \ ef, o1: Option[t1], o2: Option[t2]): Option[u] \ ef =
        ap(map(f, o1), o2)

    ///
    /// Applies the ternary function `f` to the values in `o1`, `o2` and `o3`.
    ///
    /// Returns `None` if any of `o1`, `o2` and `o3` are `None`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift3(f: (t1, t2, t3) -> u \ ef, o1: Option[t1], o2: Option[t2], o3: Option[t3]): Option[u] \ ef =
        ap(lift2(f, o1, o2), o3)

    ///
    /// Applies the 4-ary function `f` to the values in `o1`, `o2`, `o3` and `o4`.
    ///
    /// Returns `None` if any of `o1`, `o2`, `o3` and `o4` are `None`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift4(f: (t1, t2, t3, t4) -> u \ ef, o1: Option[t1], o2: Option[t2], o3: Option[t3], o4: Option[t4]): Option[u] \ ef =
        ap(lift3(f, o1, o2, o3), o4)

    ///
    /// Applies the 5-ary function `f` to the values in `o1`, `o2`, ... `o5`.
    ///
    /// Returns `None` if any of `o1`, `o2`, ... `o5` are `None`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift5(f: (t1, t2, t3, t4, t5) -> u \ ef, o1: Option[t1], o2: Option[t2], o3: Option[t3], o4: Option[t4], o5: Option[t5]): Option[u] \ ef =
        ap(lift4(f, o1, o2, o3, o4), o5)

    ///
    /// Applies the 6-ary function `f` to the values in `o1`, `o2`, ... `o6`.
    ///
    /// Returns `None` if any of `o1`, `o2`, ... `o6` are `None`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift6(f: (t1, t2, t3, t4, t5, t6) -> u \ ef, o1: Option[t1], o2: Option[t2], o3: Option[t3], o4: Option[t4], o5: Option[t5], o6: Option[t6]): Option[u] \ ef =
        ap(lift5(f, o1, o2, o3, o4, o5), o6)

    ///
    /// Applies the 7-ary function `f` to the values in `o1`, `o2`, ... `o7`.
    ///
    /// Returns `None` if any of `o1`, `o2`, ... `o7` are `None`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift7(f: (t1, t2, t3, t4, t5, t6, t7) -> u \ ef, o1: Option[t1], o2: Option[t2], o3: Option[t3], o4: Option[t4], o5: Option[t5], o6: Option[t6], o7: Option[t7]): Option[u] \ ef=
        ap(lift6(f, o1, o2, o3, o4, o5, o6), o7)

    ///
    /// Applies the 8-ary function `f` to the values in `o1`, `o2`, ... `o8`.
    ///
    /// Returns `None` if any of `o1`, `o2`, ... `o8` are `None`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift8(f: (t1, t2, t3, t4, t5, t6, t7, t8) -> u \ ef, o1: Option[t1], o2: Option[t2], o3: Option[t3], o4: Option[t4], o5: Option[t5], o6: Option[t6], o7: Option[t7], o8: Option[t8]): Option[u] \ ef =
        ap(lift7(f, o1, o2, o3, o4, o5, o6, o7), o8)

    ///
    /// Applies the 9-ary function `f` to the values in `o1`, `o2`, ... `o9`.
    ///
    /// Returns `None` if any of `o1`, `o2`, ... `o9` are `None`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift9(f: (t1, t2, t3, t4, t5, t6, t7, t8, t9) -> u \ ef, o1: Option[t1], o2: Option[t2], o3: Option[t3], o4: Option[t4], o5: Option[t5], o6: Option[t6], o7: Option[t7], o8: Option[t8], o9: Option[t9]): Option[u] \ ef =
        ap(lift8(f, o1, o2, o3, o4, o5, o6, o7, o8), o9)

    ///
    /// Applies the 10-ary function `f` to the values in `o1`, `o2`, ... `o10`.
    ///
    /// Returns `None` if any of `o1`, `o2`, ... `o10` are `None`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def lift10(f: (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) -> u \ ef, o1: Option[t1], o2: Option[t2], o3: Option[t3], o4: Option[t4], o5: Option[t5], o6: Option[t6], o7: Option[t7], o8: Option[t8], o9: Option[t9], o10: Option[t10]): Option[u] \ ef =
        ap(lift9(f, o1, o2, o3, o4, o5, o6, o7, o8, o9), o10)

}


///
/// A type class for data structures that can be folded.
///
pub class Foldable[t : Type -> Type] {

    ///
    /// Left-associative fold of a structure.
    /// Applies `f` to a start value `s` and all elements in `t` going from left to right.
    ///
    pub def foldLeft(f: (b, a) -> b \ ef, s: b, t: t[a]): b \ ef

    ///
    /// Right-associative fold of a structure.
    /// Applies `f` to a start value `s` and all elements in `t` going from right to left.
    ///
    pub def foldRight(f: (a, b) -> b \ ef, s: b, t: t[a]): b \ ef

    ///
    /// Right-associative fold of a structure.
    /// Applies `f` to a start value `s` and all elements in `t` going from right to left.
    ///
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (a, Unit -> b \ ef) -> b \ ef, s: b, t: t[a]): b \ ef

    ///
    /// Returns the number of elements in `t` that satisfy the predicate `f`.
    ///
    pub def count(f: a -> Bool \ ef, t: t[a]): Int32 \ ef =
        Foldable.foldLeft((acc, x) -> if (f(x)) acc + 1 else acc, 0, t)

    ///
    /// Returns the sum of all elements in `t`.
    ///
    pub def sum(t: t[Int32]): Int32 =
        Foldable.foldLeft((acc, x) -> acc + x, 0, t)

    ///
    /// Returns the sum of all elements in `t` according to the function `f`.
    ///
    pub def sumWith(f: a -> Int32 \ ef, t: t[a]): Int32 \ ef =
        Foldable.foldLeft((acc, x) -> acc + f(x), 0, t)

    ///
    /// Returns the product of all elements in `t`.
    ///
    pub def product(t: t[Int32]): Int32 =
        if (Foldable.isEmpty(t))
            1
        else
            Foldable.foldLeft((acc, x) -> acc * x, 1, t)

    ///
    /// Returns the product of all elements in `t` according to the function `f`.
    ///
    pub def productWith(f: a -> Int32 \ ef, t: t[a]): Int32 \ ef =
        if (Foldable.isEmpty(t))
            1
        else
            Foldable.foldLeft((acc, x) -> acc * f(x), 1, t)

    ///
    /// Returns `true` if and only if at least one element in `t` satisfies the predicate `f`.
    ///
    /// Returns `false` if `t` is empty.
    ///
    pub def exists(f: a -> Bool \ ef, t: t[a]): Bool \ ef =
        Foldable.foldRightWithCont((x, k) -> f(x) or k(), false, t)

    ///
    /// Returns `true` if and only if all elements in `t` satisfy the predicate `f`.
    ///
    /// Returns `true` if `t` is empty.
    ///
    pub def forall(f: a -> Bool \ ef, t: t[a]): Bool \ ef =
        Foldable.foldRightWithCont((x, k) -> f(x) and k(), true, t)

    ///
    /// Optionally returns the first element of `t`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def head(t: t[a]): Option[a] =
        Foldable.foldRightWithCont((x, _) -> Some(x), None, t)

    ///
    /// Optionally returns the last element of `t`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def last(t: t[a]): Option[a] =
        Foldable.foldRightWithCont((x, k) -> Option.withDefault(default = Some(x), k()), None, t)

    ///
    /// Optionally returns the first element of `t` that satisfies the predicate `f` when searching from left to right.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def findLeft(f: a -> Bool \ ef, t: t[a]): Option[a] \ ef =
        Foldable.foldRightWithCont((x, k) -> if (f(x)) Some(x) else k(), None, t)

    ///
    /// Optionally returns the first element of `t` that satisfies the predicate `f` when searching from right to left.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def findRight(f: a -> Bool, t: t[a]): Option[a] =
        Foldable.foldRight((x, acc) -> Option.withDefault(default = if (f(x)) Some(x) else None, acc), None, t)

    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def foldMap(f: a -> b \ ef, t: t[a]): b \ ef with Monoid[b] =
        Foldable.foldLeft((acc, x) -> Monoid.combine(acc, f(x)), Monoid.empty(), t)

    ///
    /// Returns true if and only if `t` is empty.
    ///
    pub def isEmpty(t: t[a]): Bool =
      Foldable.foldRightWithCont((_, _) -> false, true, t)

    ///
    /// Returns `true` if and only if the element `x` is in `t`.
    ///
    pub def memberOf(x: a, t: t[a]): Bool with Eq[a] =
        Foldable.foldRightWithCont((y, k) -> x == y or k(), false, t)

    ///
    /// Optionally finds the smallest element of `t` according to the `Order` on `a`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def minimum(t: t[a]): Option[a] with Order[a] =
        Foldable.reduceLeft(Order.min, t)

    ///
    /// Optionally finds the smallest element of `t` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def minimumBy(cmp: (a, a) -> Comparison, t: t[a]): Option[a] =
        Foldable.reduceLeft(Order.minBy(cmp), t)

    ///
    /// Optionally finds the largest element of `t` according to the `Order` on `a`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def maximum(t: t[a]): Option[a] with Order[a] =
        Foldable.reduceLeft(Order.max, t)

    ///
    /// Optionally finds the largest element of `t` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def maximumBy(cmp: (a, a) -> Comparison, t: t[a]): Option[a] =
        Foldable.reduceLeft(Order.maxBy(cmp), t)

    ///
    /// Optionally applies `f` to all elements in `t` going from left to right until a single value is obtained.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def reduceLeft(f: (a, a) -> a \ ef, t: t[a]): Option[a] \ ef =
        Foldable.foldLeft((acc, x) -> Some(Option.foldLeft((z, v) -> f(v, z), x, acc)), None, t)

    ///
    /// Optionally applies `f` to all elements in `t` going from right to left until a single value is obtained.
    ///
    /// Returns `None` if `t` is empty.
    ///
    pub def reduceRight(f: (a, a) -> a \ ef, t: t[a]): Option[a] \ ef =
        Foldable.foldRight((x, acc) -> Some(Option.foldLeft((v, z) -> f(v, z), x, acc)), None, t)

    ///
    /// Returns `t` as a chain.
    ///
    pub def toChain(t: t[a]): Chain[a] =
        Foldable.foldRight((x, acc) -> Chain.cons(x, acc), Chain.empty(), t)

    ///
    /// Returns `t` as a `DelayList`.
    ///
    pub def toDelayList(t: t[a]): DelayList[a] =
        Foldable.foldRight((x, acc) -> ECons(x, acc), ENil, t)

    ///
    /// Returns `t` as an immutable list.
    ///
    pub def toList(t: t[a]): List[a] =
        Foldable.foldRight((x, acc) -> x :: acc, Nil, t)

    ///
    /// Optionally returns `t` as a non empty chain.
    ///
    pub def toNec(t: t[a]): Option[Nec[a]] =
        Foldable.toChain(t) |> Chain.toNec

    ///
    /// Optionally returns `t` as a non empty list.
    ///
    pub def toNel(t: t[a]): Option[Nel[a]] =
        Foldable.toList(t) |> List.toNel

    ///
    /// Returns `t` without the longest prefix that satisfies the predicate `f`.
    ///
    /// Returns an immutable list.
    ///
    pub def dropWhile(f: a -> Bool, t: t[a]): List[a] =
        Foldable.foldLeft((acc, x) -> {
                let (c, tail) = acc;
                if (c and f(x)) (true, tail)
                else (false, x :: tail)
            }, (true, Nil), t) |> snd |> List.reverse

    ///
    /// Returns the longest prefix of `t` that satisfies the predicate `f`.
    ///
    /// Returns an immutable list.
    ///
    pub def takeWhile(f: a -> Bool, t: t[a]): List[a] =
        Foldable.foldRight((x, acc) -> if (f(x)) x :: acc else Nil, Nil, t)

    ///
    /// Returns an immutable list of all the elements in `t` that satisfy the predicate `f`.
    ///
    pub def filter(f: a -> Bool, t: t[a]): List[a] =
        Foldable.foldRight((x, acc) -> if (f(x)) x :: acc else acc, Nil, t)

    ///
    /// Applies `f` to each element in `t`.
    ///
    pub def foreach(f: a -> Unit \ ef, t: t[a]): Unit \ ef =
        Foldable.foldLeft((_, x) -> f(x), (), t)

}

namespace Foldable {

    use Applicative.{*>}

    ///
    /// Returns the result of applying `combine` to all the elements in `t`, using `empty` as the initial value.
    ///
    pub def fold[t: Type -> Type, a: Type](t: t[a]): a with Foldable[t], Monoid[a] =
        Foldable.foldLeft((acc, x) -> Monoid.combine(acc, x), Monoid.empty(), t)

}


///
/// The List type.
///
/// A list is either the empty list represented by `Nil`, or
/// an element `v` followed by a list `vs` represented by `v :: vs`.
///
pub enum List[t] {
    case Nil,
    case Cons(t, List[t])
}

instance Hash[List[a]] with Hash[a] {
    pub def hash(l: List[a]): Int32 =
        List.foldLeft((acc, x) -> acc `Hash.combine` Hash.hash(x), Hash.magic(), l)
}

instance Eq[List[a]] with Eq[a] {
    pub def eq(l1: List[a], l2: List[a]): Bool = match (l1, l2) {
        case (Nil, Nil) => true
        case (x :: rs, y :: qs) => if (x != y) false else rs == qs
        case _ => false
    }
}

instance Order[List[a]] with Order[a] {

    ///
    /// Compares `l1` and `l2` lexicographically.
    ///
    pub def compare(l1: List[a], l2: List[a]): Comparison = match (l1, l2) {
        case (_ :: _, Nil) => GreaterThan
        case (Nil, Nil) => EqualTo
        case (Nil, _ :: _) => LessThan
        case (z :: zs, w :: ws) =>
            let cmp = z <=> w;
            if (cmp == EqualTo) zs <=> ws else cmp
    }

}

instance Functor[List] {
    pub def map(f: a -> b \ ef, l: List[a]): List[b] \ ef = List.map(f, l)
}

instance Applicative[List] {
    pub def point(a: a) : List[a] = List.point(a)
    pub def ap(f: List[a -> b \ ef], x: List[a]) : List[b] \ ef = List.ap(f, x)
}

instance Monad[List] {
    pub def flatMap(f: a -> List[b] \ ef, x : List[a]) : List[b] \ ef = List.flatMap(f, x)
}

instance MonadZero[List] {
    pub def empty(): List[a] = Nil
}

instance UnorderedFoldable[List] {
    pub def foldMap(f: a -> b \ ef, l: List[a]): b \ ef with CommutativeMonoid[b] = List.foldMap(f, l)
    override pub def isEmpty(l: List[a]): Bool = List.isEmpty(l)
    override pub def exists(f: a -> Bool \ ef, l: List[a]): Bool \ ef = List.exists(f, l)
    override pub def forall(f: a -> Bool \ ef, l: List[a]): Bool \ ef = List.forall(f, l)
    override pub def memberOf(x: a, l: List[a]): Bool with Eq[a] = List.memberOf(x, l)
}

instance SemiGroup[List[a]] {
    pub def combine(x: List[a], y: List[a]): List[a] = x ::: y
}

instance Monoid[List[a]] {
    pub def empty(): List[a] = Nil
}

namespace List {

    ///
    /// Returns true if and only if `l` is the empty list, i.e. `Nil`.
    ///
    @Time(1) @Space(1)
    pub def isEmpty(l: List[a]): Bool = match l {
        case Nil => true
        case _   => false
    }

    ///
    /// Returns `Some(x)` if `x` is the first element of `l`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    @Time(1) @Space(1)
    pub def head(l: List[a]): Option[a] = match l {
        case Nil    => None
        case x :: _ => Some(x)
    }

    ///
    /// Returns `Some(x)` if `x` is the last element of `l`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    @Time(length(l)) @Space(1)
    pub def last(l: List[a]): Option[a] = match l {
        case Nil      => None
        case x :: Nil => Some(x)
        case _ :: rs  => last(rs)
    }

    ///
    /// Returns the length of `l`.
    ///
    @Time(length(l)) @Space(1)
    pub def length(l: List[a]): Int32 =
        def loop(ll, acc) = match ll {
            case Nil     => acc
            case _ :: xs => loop(xs, acc + 1)
        };
        loop(l, 0)



    ///
    /// Returns `l2` appended to `l1`.
    ///
    /// The infix operator `:::` is an alias for `append` (`l1 ::: l2 = append(l1, l2)`).
    ///
    @Time(length(l1)) @Space(length(l1))
    pub def append(l1: List[a], l2: List[a]): List[a] =
        foldRight((x, acc) -> x :: acc, l2, l1)

    ///
    /// Returns `true` if and only if `l` contains the element `x`.
    ///
    @Time(length(l)) @Space(1)
    pub def memberOf(a: a, l: List[a]): Bool with Eq[a] = match l {
        case Nil     => false
        case x :: xs => if (a == x) true else memberOf(a, xs)
    }

    ///
    /// Optionally finds the smallest element of `l` according to the `Order` on `a`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def minimum(l: List[a]): Option[a] with Order[a] =
        reduceLeft(Order.min, l)

    ///
    /// Optionally finds the smallest element of `l` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def minimumBy(cmp: (a, a) -> Comparison, l: List[a]): Option[a] =
        reduceLeft(Order.minBy(cmp), l)

    ///
    /// Optionally finds the largest element of `l` according to the `Order` on `a`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def maximum(l: List[a]): Option[a] with Order[a] =
        reduceLeft(Order.max, l)

    ///
    /// Optionally finds the largest element of `l` according to the given comparator `cmp`.
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def maximumBy(cmp: (a, a) -> Comparison, l: List[a]): Option[a] =
        reduceLeft(Order.maxBy(cmp), l)

    ///
    /// Optionally returns the position of `x` in `l`.
    ///
    pub def indexOf(a: a, l: List[a]): Option[Int32] with Eq[a] =
        def loop(ll, acc) = match ll {
            case Nil     => None
            case x :: xs => if (a == x) Some(acc) else loop(xs, acc + 1)
        };
        loop(l, 0)

    ///
    /// Alias for `findLeft`.
    ///
    @Time(time(f) * length(l)) @Space(space(f))
    pub def find(f: a -> Bool \ ef, l: List[a]): Option[a] \ ef = findLeft(f, l)

    ///
    /// Optionally returns the first element of `l` that satisfies the predicate `f` when searching from left to right.
    ///
    @Time(time(f) * length(l)) @Space(space(f))
    pub def findLeft(f: a -> Bool \ ef, l: List[a]): Option[a] \ ef = match l {
        case Nil     => None
        case x :: xs => if (f(x)) Some(x) else findLeft(f, xs)
    }

    ///
    /// Returns a list of all integers between `b` (inclusive) and `e` (exclusive).
    ///
    /// Returns `Nil` if `b >= e`.
    ///
    @Time(e - b) @Space(e - b)
    pub def range(b: Int32, e: Int32): List[Int32] =
        def loop(i, acc) =
            if (i < b)
                acc
            else
                loop(i - 1, i :: acc);
        loop(e - 1, Nil)

    ///
    /// Returns a list with the element `x` repeated `n` times.
    ///
    /// Returns `Nil` if `n < 0`.
    ///
    @Time(n) @Space(n)
    pub def repeat(n: Int32, a: a): List[a] =
        def loop(i, acc) =
            if (i >= n)
                acc
            else
                loop(i + 1, a :: acc);
        loop(0, Nil)

    ///
    /// Alias for `scanLeft`.
    ///
    pub def scan(f: (b, a) -> b \ ef, s: b, l: List[a]): List[b] \ ef = scanLeft(f, s, l)

    ///
    /// Accumulates the result of applying `f` to `l` going left to right.
    ///
    /// That is, the result is of the form: `s :: f(s, x1) :: f(f(s, x1), x2)  ...`.
    ///
    pub def scanLeft(f: (b, a) -> b \ ef, s: b, l: List[a]): List[b] \ ef =
        def loop(ll, k, acc) = match ll {
            case Nil     => k(Nil)
            case x :: xs =>
                let y = f(acc, x);
                loop(xs, ks -> k(y :: ks), y)
        };
        loop(l, ks -> s :: ks, s)

    ///
    /// Returns the result of applying `f` to every element in `l`.
    ///
    /// That is, the result is of the form: `f(x1) :: f(x2) :: ...`.
    ///
    pub def map(f: a -> b \ ef, l: List[a]): List[b] \ ef =
        def loop(ll, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs => {
                let y = f(x);
                loop(xs, ys -> k(y :: ys))
            }
        };
        loop(l, identity)

    ///
    /// Return the singleton list with element `x`.
    ///
    pub def point(a: a) : List[a] = a :: Nil

    ///
    /// Apply every function from `f` to every argument from `x` and return a list with all results.
    /// For `f = f1, f2, ...` and `x = x1, x2, ...` the results appear in the order
    /// `f1(x1), f1(x2), ..., f2(x1), f2(x2), ...`.
    ///
    pub def ap(f: List[a -> b \ ef], x: List[a]) : List[b] \ ef =
        map(g -> map(g, x), f) |> flatten

    ///
    /// Lift a binary function to work on lists of its original arguments, returning a list
    /// of applying all combinations of arguments.
    /// For argument lists `l1 = x1, x2, ...` and `l2 = y1, y2, ...` the results appear in the order
    /// `f(x1,y1), f(x1,y2), ..., f(x2,y1), f(x2,y2), ...`.
    ///
    pub def lift2(f: t1 -> t2 -> r \ ef, l1: List[t1], l2: List[t2]): List[r] \ ef = Applicative.liftA2(f, l1, l2)

    ///
    /// Lift a ternary function to work on lists of its original arguments, returning a list
    /// of applying all combinations of arguments.
    /// For argument lists `l1 = x1, x2, ...`, `l2 = y1, y2, ...` and `l3 = z1, z2, ...` the results appear
    /// in the following order:
    ///
    /// ```
    /// f(x1,y1,z1), f(x1,y1,z2), ..., f(x1,y2,z1), f(x1,y2,z2), ...,
    /// f(x2,y1,z1), f(x2,y1,z2), ..., f(x2,y2,z1), f(x2,y2,z2), ...`
    /// ...
    /// ```
    ///
    pub def lift3(f: t1 -> t2 -> t3 -> r \ ef, l1: List[t1], l2: List[t2], l3: List[t3]): List[r] \ ef = Applicative.liftA3(f, l1, l2, l3)

    /// Lift a 4-ary function to work on lists of its original arguments, returning a list
    /// of applying all combinations of arguments. The results appear in the order extending the pattern from `lift3`.
    pub def lift4(f: t1 -> t2 -> t3 -> t4 -> r \ ef, l1: List[t1], l2: List[t2], l3: List[t3], l4: List[t4]): List[r] \ ef = Applicative.liftA4(f, l1, l2, l3, l4)

    /// Lift a 5-ary function to work on lists of its original arguments, returning a list
    /// of applying all combinations of arguments. The results appear in the order extending the pattern from `lift3`.
    pub def lift5(f: t1 -> t2 -> t3 -> t4 -> t5 -> r \ ef, l1: List[t1], l2: List[t2], l3: List[t3], l4: List[t4], l5: List[t5]): List[r] \ ef = Applicative.liftA5(f, l1, l2, l3, l4, l5)

    ///
    /// Returns the result of applying `f` to every element in `l` along with that element's index.
    ///
    /// That is, the result is of the form: `f(x1, 0) :: f(x2, 1) :: ...`.
    ///
    @Time(time(f) * length(l)) @Space(space(f) * length(l))
    pub def mapWithIndex(f: (Int32, a) -> b \ ef, l: List[a]): List[b] \ ef =
        def loop(ll, i, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs =>
                let y = f(i, x);
                loop(xs, i + 1, ys -> k(y :: ys))
        };
        loop(l, 0, identity)

    ///
    /// Returns the result of applying `f` to every element in `l` and concatenating the results.
    ///
    @Time(time(f) * length(l)) @Space(time(f) * length(l))
    pub def flatMap(f: a -> List[b] \ ef, l: List[a]): List[b] \ ef =
        def loop(ll, acc) = match ll {
            case Nil     => acc
            case x :: xs => loop(xs, acc ::: f(x))
        };
        loop(l, Nil)

    ///
    /// Returns the reverse of `l`.
    ///
    @Time(length(l)) @Space(length(l))
    pub def reverse(l: List[a]): List[a] =
        def loop(ll, acc) = match ll {
            case Nil     => acc
            case x :: xs => loop(xs, x :: acc)
        };
        loop(l, Nil)

    ///
    /// Returns `l` with its elements rotated `n` positions to the left.
    ///
    /// That is, returns a new list where the first `n mod length(l)` elements in `l`
    /// are the last `n mod length(l)` elements of the new list.
    ///
    @Time(length(l)) @Space(length(l))
    pub def rotateLeft(n: Int32, l: List[a]): List[a] =
        let len = length(l);
        if (len == 0)
            l
        else {
            let rem = n rem len;
            let rotate = if (rem < 0) rem + len else rem;
            drop(rotate, l) ::: take(rotate, l)
        }

    ///
    /// Returns `l` with its elements rotated `n` positions to the right.
    ///
    /// That is, returns a new list where the last `n mod length(l)` elements in `l`
    /// are the first `n mod length(l)` elements of the new list.
    ///
    @Time(length(l)) @Space(length(l))
    pub def rotateRight(n: Int32, l: List[a]): List[a] = rotateLeft(-n, l)

    ///
    /// Returns `l` with the element at index `i` replaced by `x`.
    ///
    /// Returns `l` if `i < 0` or `i > length(l)-1`.
    ///
    @Time(i) @Space(i)
    pub def update(i: Int32, a: a, l: List[a]): List[a] =
        def loop(ll, j, k) = match (j, ll) {
            case (_, Nil)     => k(Nil)
            case (0, _ :: xs) => k(a :: xs)
            case (_, x :: xs) => loop(xs, j - 1, ks -> k(x :: ks))
        };
        loop(l, i, identity)

    ///
    /// Returns `l` with every occurrence of `from` replaced by `to`.
    ///
    @Time(length(l)) @Space(length(l))
    pub def replace(from: {from = a}, to: {to = a}, l: List[a]): List[a] with Eq[a] = map(e -> if (e == from.from) to.to else e, l)

    ///
    /// Returns `l2` with the `n` elements starting at index `i` replaced with the elements of `l1`.
    ///
    /// If any of the indices `i, i+1, i+2, ... , i+n-1` are out of range in `l2` then no patching is done at these indices.
    /// If `l1` becomes depleted then no further patching is done.
    /// If patching occurs at index `i+j` in `l2`, then the element at index `j` in `l1` is used.
    ///
    @Time(n) @Space(length(l2))
    pub def patch(i: Int32, n: Int32, l1: List[a], l2: List[a]): List[a] =
        def loop(ll1, ll2, c, k) = match (ll1, ll2) {
            case (x :: xs, y :: ys) => {
                if (c >= i and c < i + n)
                    loop(xs, ys, c + 1, ks -> k(x :: ks))
                else
                    loop(l1, ys, c + 1, ks -> k(y :: ks))
            }
            case _ => k(ll2)
        };
        loop(drop(-i, l1), l2, 0, identity)

    ///
    /// Returns all permutations of `l` in lexicographical order by element indices in `l`.
    ///
    /// That is, `l` is the first permutation and `reverse(l)` is the last permutation.
    ///
    @Time(Int32.factorial(length(l))) @Space(Int32.factorial(length(l)))
    pub def permutations(l: List[a]): List[List[a]] = match l {
        case Nil => Nil :: Nil
        case _   => permutationHelper(0, l)
    }

    ///
    /// Helper function for `permutations`.
    /// Returns all permutations of `l` starting with an element at or after index `i`.
    ///
    def permutationHelper(i: Int32, l: List[a]): List[List[a]] =
        if (i == length(l))
            Nil
        else
            applyHelper(at(i, l), permutations(removeIndex(i, l))) ::: permutationHelper(i + 1, l)

    ///
    /// Helper function for `permutations`.
    ///
    def at(i: Int32, l: List[a]): a = match (i, l) {
        case (0, x :: _)  => x
        case (p, _ :: xs) => at(p - 1, xs)
        case _ => unreachable!()
    }

    ///
    /// Helper function for `permutations`.
    ///
    def removeIndex(i: Int32, l: List[a]): List[a] = match (i, l) {
        case (_, Nil) => l
        case (0, _ :: xs) => xs
        case (p, x :: xs) => x :: removeIndex(p - 1, xs)
    }

    ///
    /// Returns all subsequences of `l` in lexicographical order by element indices in `l`.
    ///
    /// That is, `l` is the first subsequence and `Nil` is the last subsequence.
    ///
    @Time(length(l) * length(l)) @Space(length(l) * length(l))
    pub def subsequences(l: List[a]): List[List[a]] = match l {
        case Nil     => Nil :: Nil
        case x :: xs =>
            let r = subsequences(xs);
            applyHelper(x, r) ::: r
    }

    ///
    /// Helper function for `permutations` and `subsequences`.
    /// Returns `l` with `x` added to the beginning of each element in `l`.
    ///
    def applyHelper(a: a, l: List[List[a]]): List[List[a]] =
        def loop(ll, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs => loop(xs, ks -> k((a :: x) :: ks))
        };
        loop(l, identity)

    ///
    /// Returns `l` with `x` inserted between every two adjacent elements.
    ///
    @Time(length(l)) @Space(length(l))
    pub def intersperse(a: a, l: List[a]): List[a] =
        def loop(ll, k) = match ll {
            case x1 :: x2 :: xs => loop(x2 :: xs, ks -> k(x1 :: a :: ks))
            case _              => k(ll)
        };
        loop(l, identity)

    ///
    /// Returns the concatenation of the elements in `l2` with the elements of `l1` inserted between every two adjacent elements.
    ///
    /// That is, returns `y1 :: x1 ... xn :: y2 :: ... yn-1 :: x1 :: ... :: xn :: yn :: Nil`.
    ///
    @Time(length(l2) * length(l1)) @Space(length(l2) * length(l1))
    pub def intercalate(l1: List[a], l2: List[List[a]]): List[a] =
        def loop(ll, k) = match ll {
            case Nil            => k(Nil)
            case y :: Nil       => k(y)
            case y1 :: y2 :: ys => loop(y2 :: ys, ks -> k(y1 ::: l1 ::: ks))
        };
        loop(l2, identity)

    ///
    /// Returns the transpose of `l`.
    ///
    /// Returns `l` if the dimensions of the elements of `l` are mismatched.
    ///
    @Time(length(l) * length(l)) @Space(length(l) * length(l))
    pub def transpose(l: List[List[a]]): List[List[a]] = match l {
        case Nil    => Nil
        case x :: _ =>
            let len = length(x);
            if (not uniformHelper(l, len) or len == 0) l else transposeHelper(l, len)
    }

    ///
    /// Helper function for `transpose`.
    ///
    def uniformHelper(l: List[List[a]], len: Int32): Bool = match l {
        case Nil     => true
        case x :: xs => if (length(x) == len) uniformHelper(xs, len) else false
    }

    ///
    /// Helper function for `transpose`.
    ///
    def transposeHelper(l: List[List[a]], len: Int32): List[List[a]] = match l {
        case Nil     => repeat(len, Nil)
        case x :: xs => applyListHelper(x, transposeHelper(xs, len))
    }

    ///
    /// Helper function for `transpose`.
    ///
    def applyListHelper(l1: List[a], l2: List[List[a]]): List[List[a]] = match (l1, l2) {
        case (Nil, Nil)         => Nil
        case (x :: xs, y :: ys) => (x :: y) :: applyListHelper(xs, ys)
        case _                  => unreachable!()
    }

    ///
    /// Returns `true` if and only if `l1` is a prefix of `l2`.
    ///
    @Time(length(l1)) @Space(1)
    pub def isPrefixOf(l1: List[a], l2: List[a]): Bool with Eq[a] = match (l1, l2) {
        case (Nil, _)           => true
        case (_, Nil)           => false
        case (x :: xs, y :: ys) => if (x == y) isPrefixOf(xs, ys) else false
    }

    ///
    /// Returns `true` if and only if `l1` is an infix of `l2`.
    ///
    @Time(length(l1)) @Space(1)
    pub def isInfixOf(l1: List[a], l2: List[a]): Bool with Eq[a] = match (l1, l2) {
        case (Nil, _)     => true
        case (_, Nil)     => false
        case (_, _ :: ys) => if (isPrefixOf(l1, l2)) true else isInfixOf(l1, ys)
    }

    ///
    /// Returns `true` if and only if `l1` is a suffix of `l2`.
    ///
    @Time(length(l1)) @Space(Int32.max(length(l1), length(l2)))
    pub def isSuffixOf(l1: List[a], l2: List[a]): Bool with Eq[a] = isPrefixOf(reverse(l1), reverse(l2))

    ///
    /// Applies `f` to a start value `s` and all elements in `l` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b \ ef, s: b, l: List[a]): b \ ef = match l {
        case Nil     => s
        case x :: xs => foldLeft(f, f(s, x), xs)
    }

    ///
    /// Applies `f` to a start value `s` and all elements in `l` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, s))...)`.
    ///
    pub def foldRight(f: (a, b) -> b \ ef, s: b, l: List[a]): b \ ef =
        def loop(ll, k) = match ll {
            case Nil     => k(s)
            case x :: xs => loop(xs, ks -> k(f(x, ks)))
        };
        loop(l, upcast(identity)) // NB: Cast required because the identity continuation is pure,
                                  // but the intermediate continuations are effect polymorphic

    ///
    /// Applies `f` to a start value `z` and all elements in `l` going from right to left.
    ///
    /// That is, the result is of the form: `f(x1, ...f(xn-1, f(xn, z))...)`.
    /// A `foldRightWithCont` allows early termination by not calling the continuation.
    ///
    pub def foldRightWithCont(f: (a, Unit -> b \ ef) -> b \ ef, z: b, l: List[a]): b \ ef = match l {
        case Nil     => z
        case x :: xs => f(x, _ -> foldRightWithCont(f, z, xs))
    }

    ///
    /// Applies `f` to all elements in `l` going from left to right until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(x1, x2), x3)..., xn))`
    ///
    /// Returns `None` if `l` is empty.
    ///
    pub def reduceLeft(f: (a, a) -> a \ ef, l: List[a]): Option[a] \ ef = match l {
        case Nil     => None
        case x :: xs => Some(foldLeft(f, x, xs))
    }

    ///
    /// Returns the number of elements in `l` that satisfy the predicate `f`.
    ///
    @Time(time(f) * length(l)) @Space(space(f) * length(l))
    pub def count(f: a -> Bool \ ef, l: List[a]): Int32 \ ef =
        foldLeft((acc, x) -> if (f(x)) acc + 1 else acc, 0, l)

    ///
    /// Returns the concatenation of the elements in `l`.
    ///
    @Time(length(l)) @Space(length(l))
    pub def flatten(l: List[List[a]]): List[a] =
        def loop(ll, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs => loop(xs, ks -> k(x ::: ks))
        };
        loop(l, identity)

    ///
    /// Returns `true` if and only if at least one element in `l` satisfies the predicate `f`.
    ///
    /// Returns `false` if `l` is empty.
    ///
    @Time(time(f) * length(l)) @Space(space(f))
    pub def exists(f: a -> Bool \ ef, l: List[a]): Bool \ ef = match l {
        case Nil     => false
        case x :: xs => if (f(x)) true else exists(f, xs)
    }

    ///
    /// Returns `true` if and only if all elements in `l` satisfy the predicate `f`.
    ///
    /// Returns `true` if `l` is empty.
    ///
    @Time(time(f) * length(l)) @Space(space(f))
    pub def forall(f: a -> Bool \ ef, l: List[a]): Bool \ ef = match l {
        case Nil     => true
        case x :: xs => if (f(x)) forall(f, xs) else false
    }

    ///
    /// Returns the sublist of `l` without the last element.
    /// Returns `None` if the list `l` is `Nil`.
    ///
    @Time(length(l)) @Space(length(l))
    pub def init(l: List[a]): Option[List[a]] =
        def loop(ll, k) = match ll {
            case Nil      => None
            case _ :: Nil => Some(k(Nil))
            case x :: xs  => loop(xs, ks -> k(x :: ks))
        };
        loop(l, identity)

    ///
    /// Returns the sublist of `l` from index `b` (inclusive) to index `e` (exclusive).
    ///
    /// That is, an element at index `i` in `l` is part of the returned sublist if and only if `i >= b` and `i < e`.
    /// Note: Indices that are out of bounds in `l` are not considered (i.e. slice(b, e, l) = slice(max(0,b), min(length(l),e), l)).
    ///
    @Time(e - b) @Space(e - b)
    pub def slice(b: Int32, e: Int32, l: List[a]): List[a] =
        def loop(ll, i, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs =>
                if (i < b)
                    loop(xs, i + 1, k)
                else if (i >= e)
                    k(Nil)
                else
                    loop(xs, i + 1, ks -> k(x :: ks))
        };
        if (b < e) loop(l, 0, identity) else Nil

    ///
    /// Returns a pair of lists `(l1, l2)`.
    ///
    /// `l1` contains all elements of `l` that satisfy the predicate `f`.
    /// `l2` contains all elements of `l` that do not satisfy the predicate `f`.
    ///
    @Time(time(f) * length(l)) @Space(space(f) * length(l))
    pub def partition(f: a -> Bool \ ef, l: List[a]): (List[a], List[a]) \ ef =
        def loop(ll, k) = match ll {
            case Nil     => k((Nil, Nil))
            case x :: xs =>
                if (f(x))
                    loop(xs, match (ks, ls) -> k((x :: ks, ls)))
                else
                    loop(xs, match (ks, ls) -> k((ks, x :: ls)))
        };
        loop(l, identity)

    ///
    /// Returns a pair of lists `(l1, l2)`.
    ///
    /// `l1` is the longest prefix of `l` that satisfies the predicate `f`.
    /// `l2` is the remainder of `l`.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(l)) @Space(space(f) * length(l))
    pub def span(f: a -> Bool, l: List[a]): (List[a], List[a]) =
        def loop(ll, k) = match ll {
            case Nil     => k((Nil, Nil))
            case x :: xs =>
                if (f(x))
                    loop(xs, match (ks, ls) -> k((x :: ks, ls)))
                else
                    k((Nil, ll))
        };
        loop(l, identity)

    ///
    /// Returns `l` without the first `n` elements.
    ///
    /// Returns `Nil` if `n > length(l)`.
    /// Returns `l` if `n < 0`.
    ///
    @Time(n) @Space(length(l) - n)
    pub def drop(n: Int32, l: List[a]): List[a] = match l {
        case _ if n <= 0 => l
        case Nil         => Nil
        case _ :: xs     => drop(n - 1, xs)
    }

    ///
    /// Returns `l` without the longest prefix that satisfies the predicate `f`.
    ///
    @Time(time(f) * length(l)) @Space(space(f))
    pub def dropWhile(f: a -> Bool \ ef, l: List[a]): List[a] \ ef = match l {
        case Nil     => Nil
        case x :: xs => if (f(x)) dropWhile(f, xs) else l
    }

    ///
    /// Returns the first `n` elements of `l`.
    ///
    /// Returns `l` if `n > length(l)`.
    /// Returns `Nil` if `n < 0`.
    ///
    @Time(n) @Space(n)
    pub def take(n: Int32, l: List[a]): List[a] =
        def loop(ll, i, k) =
            if (i <= 0)
                k(Nil)
            else
                match ll {
                    case Nil     => k(Nil)
                    case x :: xs => loop(xs, i - 1, ks -> k(x :: ks))
                };
        loop(l, n, identity)

    ///
    /// Returns the longest prefix of `l` that satisfies the predicate `f`.
    ///
    @Time(time(f) * length(l)) @Space(space(f) * length(l))
    pub def takeWhile(f: a -> Bool \ ef, l: List[a]): List[a] \ ef =
        def loop(ll, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs => if (f(x)) loop(xs, ks -> k(x :: ks)) else k(Nil)
        };
        loop(l, identity)

    ///
    /// Split the list `xs` at the position `n` returning the left and right parts.
    /// Position `n` is included in the right part.
    ///
    /// Example: `splitAt(2, 1::2::3::4::Nil)` returns `(1::2::Nil, 3::4::Nil)`
    ///
    /// Returns `(xs, Nil)` if `n > length(xs)`.
    /// Returns `(Nil, xs)` if `n < 0`.
    ///
    pub def splitAt(n: Int32, xs: List[a]): (List[a], List[a]) =
        (List.take(n, xs), List.drop(n, xs))

    ///
    /// Partitions `l` into sublists such that for any two elements `x` and `y` in a sublist, `f(x, y)` is true.
    ///
    /// A sublist is created by iterating through the remaining elements of `l` from left to right and adding an
    /// element to the sublist if and only if doing so creates no conflicts with the elements already in the sublist.
    ///
    /// The function `f` must be pure.
    ///
    @Time(time(f) * length(l)) @Space(space(f) * length(l))
    pub def groupBy(f: (a, a) -> Bool, l: List[a]): List[List[a]] =
        def loop(ll, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs =>
                let (r1, r2) = extractHelper(f, xs, x :: Nil);
                loop(r2, ks -> k(r1 :: ks))
        };
        loop(l, identity)

    ///
    /// Helper function for `groupBy`.
    ///
    /// The list `l2` is needed by `agreeHelper` so it must be materialized (rather than a continuation).
    /// We have no choice but to reverse it.
    ///
    def extractHelper(f: (a, a) -> Bool, l1: List[a], l2: List[a]): (List[a], List[a]) =
        def loop(ll, ll2, k) = match ll {
            case Nil     => (reverse(ll2), k(Nil))
            case x :: xs => if (agreeHelper(f, x, ll2)) loop(xs, x :: ll2, k) else loop(xs, ll2, ks -> k(x :: ks))
        };
        loop(l1, l2, identity)

    ///
    /// Helper function for `groupBy`.
    ///
    def agreeHelper(f: (a, a) -> Bool, a: a, l: List[a]): Bool = match l {
        case Nil     => true
        case x :: xs => if (f(x, a) and f(a, x)) agreeHelper(f, a, xs) else false
    }

    ///
    /// Returns a list where the element at index `i` is `(a, b)` where
    /// `a` is the element at index `i` in `l1` and `b` is the element at index `i` in `l2`.
    ///
    /// If either `l1` or `l2` becomes depleted, then no further elements are added to the resulting list.
    ///
    @Time(Int32.min(length(l1), length(l2))) @Space(Int32.min(length(l1), length(l2)))
    pub def zip(l1: List[a], l2: List[b]): List[(a, b)] =
        def loop(ll1, ll2, k) = match (ll1, ll2) {
            case (x :: xs, y :: ys) => loop(xs, ys, ks -> k((x, y) :: ks))
            case _                  => k(Nil)
        };
        loop(l1, l2, identity)

    ///
    /// Returns a list where the element at index `i` is `f(a, b)` where
    /// `a` is the element at index `i` in `l1` and `b` is the element at index `i` in `l2`.
    ///
    /// If either `l1` or `l2` becomes depleted, then no further elements are added to the resulting list.
    ///
    @Time(time(f) * Int32.min(length(l1), length(l2))) @Space(space(f) * Int32.min(length(l1), length(l2)))
    pub def zipWith(f: (a, b) -> c \ ef, l1: List[a], l2: List[b]): List[c] \ ef =
        def loop(ll1, ll2, k) = match (ll1, ll2) {
            case (x :: xs, y :: ys) =>
                let z = f(x, y);
                loop(xs, ys, ks -> k(z :: ks))
            case _ => k(Nil)
        };
        loop(l1, l2, identity)

    ///
    /// Returns a list where each element `e` is mapped to `(e, i)` where `i`
    /// is the index of `e`.
    ///
    @Time(length(l)) @Space(length(l))
    pub def zipWithIndex(l: List[a]): List[(a, Int32)] =
        def loop(ll, i, k) = match ll {
            case Nil       => k(Nil)
            case (x :: xs) => loop(xs, i + 1, ks -> k((x, i) :: ks))
        };
        loop(l, 0, identity)

    ///
    /// Returns a pair of lists, the first containing all first components in `l`
    /// and the second containing all second components in `l`.
    ///
    @Time(length(l)) @Space(length(l))
    pub def unzip(l: List[(a, b)]): (List[a], List[b]) =
        def loop(ll, k) = match ll {
            case Nil            => k((Nil, Nil))
            case (x1, x2) :: xs => loop(xs, match (ks, ls) -> k((x1 :: ks, x2 :: ls)))
        };
        loop(l, identity)

    ///
    /// Returns a list where the element at index `i` is `(a, b, c)` where
    /// `a` is the element at index `i` in `l1`, `b` is the element at index `i` in `l2`
    /// and `c` is the element at index `i` in `l3`.
    ///
    /// If any one of `l1`, `l2` or `l3` become depleted, then no further elements are added to the resulting list.
    ///
    pub def zip3(l1: List[a], l2: List[b], l3: List[c]): List[(a, b, c)] =
        zipWith3((x, y, z) -> (x, y, z), l1, l2, l3)

    ///
    /// Returns a list where the element at index `i` is `f(a, b, c)` where
    /// `a` is the element at index `i` in `l1`, `b` is the element at index `i` in `l2`
    /// and `c` is the element at index `i` in `l3`.
    ///
    /// If any one of `l1`, `l2` or `l3` become depleted, then no further elements are added to the resulting list.
    ///
    pub def zipWith3(f: (a, b, c) -> d \ ef, l1: List[a], l2: List[b], l3: List[c]): List[d] \ ef =
        def loop(ll1, ll2, ll3, k) = match (ll1, ll2, ll3) {
            case (x :: xs, y :: ys, z :: zs) =>
                let r = f(x, y, z);
                loop(xs, ys, zs, ks -> k(r :: ks))
            case _ => k(Nil)
        };
        loop(l1, l2, l3, identity)

    ///
    /// Returns a triple of lists, the first containing all first components in `l`
    /// the second containing all second components in `l` and the third containing all
    /// third components in `l`.
    ///
    pub def unzip3(l: List[(a, b, c)]): (List[a], List[b], List[c]) =
        def loop(ll, k) = match ll {
            case Nil             => k((Nil, Nil, Nil))
            case (x, y, z) :: xs => loop(xs, match (ks, ls, ms) -> k((x :: ks, y :: ls, z :: ms)))
        };
        loop(l, identity)

    ///
    /// Alias for `foldLeft2`.
    ///
    @Time(time(f) * Int32.min(length(l1), length(l2))) @Space(space(f) * Int32.min(length(l1), length(l2)))
    pub def fold2(f: (c, a, b) -> c \ ef, c: c, l1: List[a], l2: List[b]): c \ ef = foldLeft2(f, c, l1, l2)

    ///
    /// Accumulates the result of applying `f` pairwise to the elements of `l1` and `l2`
    /// starting with the initial value `c` and going from left to right.
    ///
    @Time(time(f) * Int32.min(length(l1), length(l2))) @Space(space(f) * Int32.min(length(l1), length(l2)))
    pub def foldLeft2(f: (c, a, b) -> c \ ef, c: c, l1: List[a], l2: List[b]): c \ ef = match (l1, l2) {
        case (x :: xs, y :: ys) => foldLeft2(f, f(c, x, y), xs, ys)
        case _                  => c
    }

    ///
    /// Returns the result of mapping each element and combining the results.
    ///
    pub def foldMap(f: a -> b \ ef, l: List[a]): b \ ef with Monoid[b] =
        foldLeft((acc, x) -> Monoid.combine(acc, f(x)), Monoid.empty(), l)

    ///
    /// Collects the results of applying the partial function `f` to every element in `l`.
    ///
    @Time(time(f) * length(l)) @Space(space(f) * length(l))
    pub def filterMap(f: a -> Option[b] \ ef, l: List[a]): List[b] \ ef =
        def loop(ll, k) = match ll {
            case Nil     => k(Nil)
            case x :: xs => match f(x) {
                case None    => loop(xs, k)
                case Some(v) => loop(xs, ks -> k(v :: ks))
            }
        };
        loop(l, identity)

    ///
    /// Returns the first non-None result of applying the partial function `f` to each element of `l`.
    ///
    /// Returns `None` if every element of `l` is `None`.
    ///
    @Time(time(f) * length(l)) @Space(space(f))
    pub def findMap(f: a -> Option[b] \ ef, l: List[a]): Option[b] \ ef = match l {
        case Nil     => None
        case x :: xs => match f(x) {
            case None    => findMap(f, xs)
            case Some(v) => Some(v)
        }
    }

    ///
    /// Returns the elements of `l` as a `DelayList`.
    ///
    pub def toDelayList(l: List[a]): DelayList[a] = match l {
        case Nil     => ENil
        case x :: xs => LCons(x, lazy toDelayList(xs))
    }

    ///
    /// Returns the list `l` as a chain.
    ///
    pub def toChain(l: List[a]): Chain[a] =
        List.foldLeft(Chain.snoc, Chain.empty(), l)

    ///
    /// Applies `f` to every element of `l`.
    ///
    @Time(time(f)) @Space(space(f))
    pub def foreach(f: a -> Unit \ ef, l: List[a]): Unit \ ef = match l {
        case Nil     => ()
        case x :: xs => f(x); foreach(f, xs)
    }

    ///
    /// Returns the list `l` as `Option[Nel[a]]`.
    ///
    /// If `l` is empty return `None`, otherwise return the Nel wrapped in `Some`.
    ///
    pub def toNel(l: List[a]): Option[Nel[a]] = match l {
        case Nil     => None
        case x :: xs => Some(Nel(x, xs))
    }

    ///
    /// Returns the list `l` as `Option[Nec[a]]`.
    ///
    /// If `l` is empty return `None`, otherwise return the Nec wrapped in `Some`.
    ///
    pub def toNec(l: List[a]): Option[Nec[a]] = match l {
        case Nil     => None
        case x :: xs => Some(foldLeft(Nec.snoc, Nec.singleton(x), xs))
    }

    ///
    /// Build a list by applying `f` to the seed value `st`.
    ///
    /// `f` should return `Some(a,st1)` to signal a new list element `a` and a new seed value `st1`.
    ///
    /// `f` should return `None` to signal the end of building the list.
    ///
    pub def unfold(f: s -> Option[(a, s)] \ ef, st: s): List[a] \ ef =
        def loop(sst, k) = match f(sst) {
            case None         => k(Nil)
            case Some(a, st1) => loop(st1, ks -> k(a :: ks))
        };
        loop(st, identity)

    ///
    /// Build a list by applying the function `next` to `()`. `next` is expected to encapsulate
    /// a stateful resource such as a file handle that can be iterated.
    ///
    /// `next` should return `Some(a)` to signal a new list element `a`.
    ///
    /// `next` should return `None` to signal the end of building the list.
    ///
    pub def unfoldWithIter(next: Unit -> Option[a] \ ef): List[a] \ ef =
        def loop(k) = match next() {
            case None    => k(Nil)
            case Some(x) => loop(ks -> k(x :: ks))
        };
        loop(identity)

    ///
    /// Build a list by applying the function `next` to `()`. `next` is expected to encapsulate
    /// a stateful resource such as a file handle that can be iterated.
    ///
    /// `next` should return `Ok(Some(a)` to signal a new list element `Ok(a)`.
    ///
    /// `next` should return `Ok(None)` to signal the end of building the list.
    ///
    /// `next` should return `Err(e)` to signal that an error occurred. The function returns `Err(e)`.
    ///
    pub def unfoldWithOkIter(next: Unit -> Result[Option[a], e] \ ef): Result[List[a], e] \ ef =
        def loop(k) = match next() {
            case Ok(None)    => k(Ok(Nil))
            case Err(e)      => k(Err(e))
            case Ok(Some(x)) => loop(Result.flatMap(ks -> k(Ok(x :: ks))))
        };
        loop(identity)

    ///
    /// Returns the list `l` with duplicates removed. The first occurence of
    /// an element is kept and except for the removal of subsequent duplicates
    /// the order of `l` is preserved.
    ///
    /// `distinct` uses the Flix's builtin equality test. Use `distinctWith` if you
    /// need a custom equality test.
    ///
    pub def distinct(l: List[a]): List[a] with Eq[a] =
        def loop(ll1, ll2, k) = match ll1 {
            case Nil     => k(Nil)
            case x :: xs =>
                if (memberOf(x, ll2))
                    loop(xs, ll2, k)
                else
                    loop(xs, x :: ll2, ks -> k(x :: ks))
        };
        loop(l, Nil, identity)

    ///
    /// Returns the list `l` with duplicates removed using the supplied function
    /// `f` for comparison. The first occurrence of an element is kept and except
    /// for the removal of subsequent duplicates the order of `l` is preserved.
    ///
    pub def distinctWith(f: (a, a) -> Bool, l: List[a]): List[a] =
        def loop(ll1, ll2, k) = match ll1 {
            case Nil     => k(Nil)
            case x :: xs =>
                if (exists(f(x), ll2))
                    loop(xs, ll2, k)
                else
                    loop(xs, x :: ll2, ks -> k(x :: ks))
        };
        loop(l, Nil, identity)

    ///
    /// Helper function for `sequence` and `traverse`.
    ///
    /// Builds an "applicative list" from a head of one applicative action and an
    /// applicative list of the tail.
    ///
    def consA(mx: f[a], ml: f[List[a]]): f[List[a]] with Applicative[f] =
        use Functor.{<$>};
        use Applicative.{<*>};
        (((x, xs) -> x :: xs) <$> mx) <*> ml

    ///
    /// Returns the result of running all the actions in the list `l` going from left
    /// to right.
    ///
    pub def sequence(l: List[m[a]]): m[List[a]] with Applicative[m] =
        def loop(ll, k) = match ll {
            case Nil      => k(Applicative.point(Nil))
            case mx :: rs => loop(rs, ks -> k(consA(mx, ks)))
        };
        loop(l, identity)

    ///
    /// Merges the two lists `l1` and `l2`. Assuming they are both sorted.
    /// If two elements compare `EqualTo`, then the element of `l1` is first in the result.
    ///
    pub def merge(l1: List[a], l2: List[a]): List[a] with Order[a] =
        def loop(acc1, acc2, k) = match (acc1, acc2) {
            case (x :: xs, y :: ys) => {
                let cmp = x <=> y;
                if (cmp == LessThan or cmp == EqualTo) loop(xs, acc2, ks -> k(x :: ks))
                else                                   loop(acc1, ys, ks -> k(y :: ks))
            }
            case (Nil    , y :: ys) => k(y :: ys)
            case (x :: xs, Nil    ) => k(x :: xs)
            case (Nil    , Nil    ) => k(Nil)
        };
        loop(l1, l2, identity)

}
