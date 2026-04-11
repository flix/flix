instance Eq[BigDecimal] {
    pub def eq(x: BigDecimal, y: BigDecimal): Bool = %%BIGDEC_CMP%%(x, y) == 0
    redef neq(x: BigDecimal, y: BigDecimal): Bool = %%BIGDEC_CMP%%(x, y) != 0
}

instance Order[BigDecimal] {

    redef less(x: BigDecimal, y: BigDecimal): Bool = %%BIGDEC_CMP%%(x, y) < 0

    redef lessEqual(x: BigDecimal, y: BigDecimal): Bool = %%BIGDEC_CMP%%(x, y) <= 0

    redef greater(x: BigDecimal, y: BigDecimal): Bool = %%BIGDEC_CMP%%(x, y) > 0

    redef greaterEqual(x: BigDecimal, y: BigDecimal): Bool = %%BIGDEC_CMP%%(x, y) >= 0

    pub def compare(x: BigDecimal, y: BigDecimal): Comparison =
        match %%BIGDEC_CMP%%(x, y) {
            case 0          => Comparison.EqualTo
            case z if z < 0 => Comparison.LessThan
            case _          => Comparison.GreaterThan
        }

}

instance Hash[BigDecimal] {
    pub def hash(x: BigDecimal): Int32 = %%BIGDEC_HASH%%(x)
}

instance ToString[BigDecimal] {
    pub def toString(x: BigDecimal): String = %%BIGDEC_TO_STRING%%(x)
}

instance FromString[BigDecimal] {
    pub def fromString(x: String): Option[BigDecimal] = BigDecimal.fromString(x)
}

instance Add[BigDecimal] {
    pub def add(x: BigDecimal, y: BigDecimal): BigDecimal = %%BIGDEC_ADD%%(x, y)
}

instance Sub[BigDecimal] {
    pub def sub(x: BigDecimal, y: BigDecimal): BigDecimal = %%BIGDEC_SUB%%(x, y)
}

instance Mul[BigDecimal] {
    pub def mul(x: BigDecimal, y: BigDecimal): BigDecimal = %%BIGDEC_MUL%%(x, y)
}

instance Div[BigDecimal] {
    pub def div(x: BigDecimal, y: BigDecimal): BigDecimal =
        if (y == 0.0ff)
            0.0ff
        else
            %%BIGDEC_DIV%%(x, y)
}

instance Neg[BigDecimal] {
    pub def neg(x: BigDecimal): BigDecimal = %%BIGDEC_NEG%%(x)
}

pub mod BigDecimal {

    ///
    /// Returns the smaller of `x` and `y`.
    ///
    pub def min(x: BigDecimal, y: BigDecimal): BigDecimal = if (x <= y) x else y

    ///
    /// Returns the larger of `x` and `y`.
    ///
    pub def max(x: BigDecimal, y: BigDecimal): BigDecimal = if (x >= y) x else y

    ///
    /// Returns the absolute value of `x`.
    ///
    pub def abs(x: BigDecimal): BigDecimal = if (x >= 0.0ff) x else -x

    ///
    /// Returns `base` raised to the power of `n`.
    ///
    pub def pow(base: {base = BigDecimal}, n: Int32): BigDecimal =
        if (n < 0i32)
            bug!("BigDecimal.pow: negative exponent.")
        else {
            def loop(acc: BigDecimal, b: BigDecimal, e: Int32): BigDecimal =
                if (e == 0i32)
                    acc
                else if (%%INT32_REM%%(e, 2i32) == 0i32)
                    loop(acc, b * b, %%INT32_DIV%%(e, 2i32))
                else
                    loop(acc * b, b * b, %%INT32_DIV%%(e, 2i32));
            loop(1.0ff, base#base, n)
        }

    ///
    /// Returns the distance between `x` and `y`.
    ///
    pub def dist(x: BigDecimal, y: BigDecimal): BigDecimal = abs(x - y)

    ///
    /// Returns `1` if `x > y`, `-1` if `x < y`, and `0` if `x = y`.
    ///
    pub def compare(x: BigDecimal, y: BigDecimal): Int32 = %%BIGDEC_CMP%%(x, y)

    ///
    /// Returns `1` if `x > 0`, `-1` if `x < 0`, and `0` if `x = 0`.
    ///
    pub def signum(x: BigDecimal): Int32 = compare(x, 0.0ff)

    ///
    /// Parse the string `s` as a BigDecimal, leading or trailing whitespace is trimmed.
    /// A successful parse is wrapped with `Some(x)`, a parse failure is indicated by `None`.
    ///
    pub def fromString(s: String): Option[BigDecimal] = {
        let (ok, v) = %%BIGDEC_FROM_STRING%%(s);
        if (ok) Some(v) else None
    }

    ///
    /// Returns `x` as a "plain string" without an exponent field.
    ///
    pub def toPlainString(x: BigDecimal): String = %%BIGDEC_TO_PLAIN_STRING%%(x)

    ///
    /// Compare `x` and `y` for equality normalizing any difference in scale.
    ///
    pub def numericEquals(x: BigDecimal, y: BigDecimal): Bool = compare(x, y) == 0

    ///
    /// Returns the scale of `x`.
    ///
    pub def scale(x: BigDecimal): Int32 = %%BIGDEC_SCALE%%(x)

    ///
    /// Returns the precision of `x`.
    ///
    pub def precision(x: BigDecimal): Int32 = %%BIGDEC_PRECISION%%(x)

    ///
    /// Returns `x` rounded up to a BigDecimal representing the nearest larger integer value.
    ///
    pub def ceil(x: BigDecimal): BigDecimal = %%BIGDEC_CEIL%%(x)

    ///
    /// Returns `x` rounded down to a BigDecimal representing the nearest smaller integer value.
    ///
    pub def floor(x: BigDecimal): BigDecimal = %%BIGDEC_FLOOR%%(x)

    ///
    /// Returns `x` rounded to a BigDecimal representing the nearest integer value.
    ///
    pub def round(x: BigDecimal): BigDecimal = %%BIGDEC_ROUND%%(x)

    ///
    /// Convert `x` to an `Option[Int8]`.
    ///
    pub def tryToInt8(x: BigDecimal): Option[Int8] = toBigInt(x) |> BigInt.tryToInt8

    ///
    /// Convert `x` to an `Option[Int16]`.
    ///
    pub def tryToInt16(x: BigDecimal): Option[Int16] = toBigInt(x) |> BigInt.tryToInt16

    ///
    /// Convert `x` to an `Option[Int32]`.
    ///
    pub def tryToInt32(x: BigDecimal): Option[Int32] = toBigInt(x) |> BigInt.tryToInt32

    ///
    /// Convert `x` to an `Option[Int64]`.
    ///
    pub def tryToInt64(x: BigDecimal): Option[Int64] = toBigInt(x) |> BigInt.tryToInt64

    ///
    /// Convert `x` to a `BigInt`.
    ///
    pub def toBigInt(x: BigDecimal): BigInt = %%BIGDEC_TO_BIGINT%%(x)

    ///
    /// Convert `x` to an `Option[Float32]`.
    ///
    pub def tryToFloat32(x: BigDecimal): Option[Float32] =
        match Float32.fromString(ToString.toString(x)) {
            case Some(d) if Float32.isFinite(d) => Some(d)
            case _ => None
        }

    ///
    /// Convert `x` to an `Option[Float64]`.
    ///
    pub def tryToFloat64(x: BigDecimal): Option[Float64] =
        match Float64.fromString(ToString.toString(x)) {
            case Some(d) if Float64.isFinite(d) => Some(d)
            case _ => None
        }

}
