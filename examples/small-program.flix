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
/// The List type.
///
/// A list is either the empty list represented by `Nil`, or
/// an element `v` followed by a list `vs` represented by `v :: vs`.
///
pub enum List[t] {
    case Nil,
    case Cons(t, List[t])
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
    /// Returns `true` if and only if `l` contains the element `x`.
    ///
    @Time(length(l)) @Space(1)
    pub def memberOf(a: a, l: List[a]): Bool with Eq[a] = match l {
        case Nil     => false
        case x :: xs => if (a == x) true else memberOf(a, xs)
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
    /// Return the singleton list with element `x`.
    ///
    pub def point(a: a) : List[a] = a :: Nil

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
    /// Applies `f` to a start value `s` and all elements in `l` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, x1), x2)..., xn)`.
    ///
    pub def foldLeft(f: (b, a) -> b \ ef, s: b, l: List[a]): b \ ef = match l {
        case Nil     => s
        case x :: xs => foldLeft(f, f(s, x), xs)
    }

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
}

