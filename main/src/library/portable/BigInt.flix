instance Eq[BigInt] {
    pub def eq(x: BigInt, y: BigInt): Bool = %%BIGINT_CMP%%(x, y) == 0
    redef neq(x: BigInt, y: BigInt): Bool = %%BIGINT_CMP%%(x, y) != 0
}

instance Order[BigInt] {

    redef less(x: BigInt, y: BigInt): Bool = %%BIGINT_CMP%%(x, y) < 0

    redef lessEqual(x: BigInt, y: BigInt): Bool = %%BIGINT_CMP%%(x, y) <= 0

    redef greater(x: BigInt, y: BigInt): Bool = %%BIGINT_CMP%%(x, y) > 0

    redef greaterEqual(x: BigInt, y: BigInt): Bool = %%BIGINT_CMP%%(x, y) >= 0

    pub def compare(x: BigInt, y: BigInt): Comparison =
        match %%BIGINT_CMP%%(x, y) {
            case 0          => Comparison.EqualTo
            case z if z < 0 => Comparison.LessThan
            case _          => Comparison.GreaterThan
        }

}

instance Hash[BigInt] {
    pub def hash(x: BigInt): Int32 = %%BIGINT_HASH%%(x)
}

instance ToString[BigInt] {
    pub def toString(x: BigInt): String = %%BIGINT_TO_STRING%%(x)
}

instance FromString[BigInt] {
    pub def fromString(x: String): Option[BigInt] = BigInt.fromString(x)
}

instance Add[BigInt] {
    pub def add(x: BigInt, y: BigInt): BigInt = %%BIGINT_ADD%%(x, y)
}

instance Sub[BigInt] {
    pub def sub(x: BigInt, y: BigInt): BigInt = %%BIGINT_SUB%%(x, y)
}

instance Mul[BigInt] {
    pub def mul(x: BigInt, y: BigInt): BigInt = %%BIGINT_MUL%%(x, y)
}

instance Div[BigInt] {
    pub def div(x: BigInt, y: BigInt): BigInt =
        if (y == 0ii)
            0ii
        else
            %%BIGINT_DIV%%(x, y)
}

instance Neg[BigInt] {
    pub def neg(x: BigInt): BigInt = %%BIGINT_NEG%%(x)
}

pub mod BigInt {

    ///
    /// Returns the smaller of `x` and `y`.
    ///
    pub def min(x: BigInt, y: BigInt): BigInt = if (x <= y) x else y

    ///
    /// Returns the larger of `x` and `y`.
    ///
    pub def max(x: BigInt, y: BigInt): BigInt = if (x >= y) x else y

    ///
    /// Returns the absolute value of `x`.
    ///
    pub def abs(x: BigInt): BigInt = if (x >= 0ii) x else -x

    ///
    /// Returns `base` raised to the power of `n`.
    ///
    pub def pow(base: {base = BigInt}, n: Int32): BigInt = {
        if (n < 0i32) {
            0ii
        } else {
            def loop(acc: BigInt, b: BigInt, e: Int32): BigInt =
                if (e == 0i32)
                    acc
                else if (%%INT32_REM%%(e, 2i32) == 0i32)
                    loop(acc, b * b, %%INT32_DIV%%(e, 2i32))
                else
                    loop(acc * b, b * b, %%INT32_DIV%%(e, 2i32));
            loop(1ii, base#base, n)
        }
    }

    ///
    /// Returns the Euclidean modulo of `x` and `n`.
    /// The result is always non-negative.
    ///
    pub def modulo(x: BigInt, n: BigInt): BigInt =
        if (n == 0ii)
            0ii
        else
            remainder(remainder(x, n) + n, n)

    ///
    /// Returns the remainder of `x / n`.
    /// The result can be negative.
    ///
    /// See also `BigInt.modulo`.
    ///
    pub def remainder(x: BigInt, n: BigInt): BigInt =
        if (n == 0ii)
            0ii
        else
            %%BIGINT_REM%%(x, n)

    ///
    /// Returns the distance between `x` and `y`.
    ///
    pub def dist(x: BigInt, y: BigInt): BigInt = abs(x - y)

    ///
    /// Returns `1` if `x > y`, `-1` if `x < y`, and `0` if `x = y`.
    ///
    pub def compare(x: BigInt, y: BigInt): Int32 = %%BIGINT_CMP%%(x, y)

    ///
    /// Returns `1` if `x > 0`, `-1` if `x < 0`, and `0` if `x = 0`.
    ///
    pub def signum(x: BigInt): Int32 = compare(x, 0ii)

    ///
    /// Returns the bit of `x` at `position` (either 0 or 1).
    ///
    /// The bits of x have positions: 0 (rightmost bit), 1, 2, ...
    ///
    pub def getBit(x: BigInt, position: Int32): Int32 =
        if (rightShift(x, position) `remainder` 2ii == 0ii) 0 else 1

    ///
    /// Returns the greatest common non-negative divisor of `x` and `y`.
    ///
    pub def gcd(x: BigInt, y: BigInt): BigInt = if (y == 0ii) abs(x) else gcd(y, x `remainder` y)

    ///
    /// Returns `x` with the bit at position `position` set (to 1).
    ///
    pub def setBit(x: BigInt, position: Int32): BigInt = bitwiseOr(x, leftShift(1ii, position))

    ///
    /// Returns `x` with the bit at position `position` cleared (to 0).
    ///
    pub def clearBit(x: BigInt, position: Int32): BigInt = bitwiseAnd(x, bitwiseNot(leftShift(1ii, position)))

    ///
    /// Returns `x` with the bit at position `position` flipped.
    ///
    pub def flipBit(x: BigInt, position: Int32): BigInt = bitwiseXor(x, leftShift(1ii, position))

    ///
    /// Returns the integer binary logarithm of `x`.
    ///
    pub def log2(x: BigInt): Int32 =
        %%BIGINT_BIT_LENGTH%%(x)

    ///
    /// Returns `x` with the bits shifted left by `y` places.
    ///
    pub def leftShift(x: BigInt, y: Int32): BigInt =
        %%BIGINT_SHL%%(x, y)

    ///
    /// Returns `x` with the bits shifted right by `y` places.
    ///
    pub def rightShift(x: BigInt, y: Int32): BigInt =
        %%BIGINT_SHR%%(x, y)

    ///
    /// Returns the bitwise AND of `x` and `y`.
    ///
    pub def bitwiseAnd(x: BigInt, y: BigInt): BigInt =
        %%BIGINT_AND%%(x, y)

    ///
    /// Returns the bitwise NOT of `x`.
    ///
    pub def bitwiseNot(x: BigInt): BigInt =
        %%BIGINT_NOT%%(x)

    ///
    /// Returns the bitwise OR of `x` and `y`.
    ///
    pub def bitwiseOr(x: BigInt, y: BigInt): BigInt =
        %%BIGINT_OR%%(x, y)

    ///
    /// Returns the bitwise XOR of `x` and `y`.
    ///
    pub def bitwiseXor(x: BigInt, y: BigInt): BigInt =
        %%BIGINT_XOR%%(x, y)

    ///
    /// Return a string representation of `x`.
    ///
    pub def toString(x: BigInt): String = ToString.toString(x)

    ///
    /// Parse the string `s` as a BigInt, leading or trailing whitespace is trimmed.
    /// A successful parse is wrapped with `Some(x)`, a parse failure is indicated by `None`.
    ///
    pub def fromString(s: String): Option[BigInt] = {
        let (ok, v) = %%BIGINT_FROM_STRING%%(s);
        if (ok) Some(v) else None
    }

    ///
    /// Convert `x` to an `Option[Int8]`.
    ///
    /// Returns `Some(x as Int8)` if the numeric value of `x` can be represented exactly.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Int8
    /// (i.e. -128 to 127).
    ///
    pub def tryToInt8(x: BigInt): Option[Int8] = Int8.fromString(toString(x))

    ///
    /// Convert `x` to an `Option[Int16]`.
    ///
    /// Returns `Some(x as Int16)` if the numeric value of `x` can be represented exactly.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Int16
    /// (i.e. -32768 to 32767).
    ///
    pub def tryToInt16(x: BigInt): Option[Int16] = Int16.fromString(toString(x))

    ///
    /// Convert `x` to an `Option[Int32]`.
    ///
    /// Returns `Some(x as Int32)` if the numeric value of `x` can be represented exactly.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Int32
    /// (i.e. -2147483648 to 2147483647).
    ///
    pub def tryToInt32(x: BigInt): Option[Int32] = Int32.fromString(toString(x))

    ///
    /// Convert `x` to an `Option[Int64]`.
    ///
    /// Returns `Some(x as Int64)` if the numeric value of `x` can be represented exactly.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Int64
    /// (i.e. -9223372036854775808 to 9223372036854775807).
    ///
    pub def tryToInt64(x: BigInt): Option[Int64] = Int64.fromString(toString(x))

    ///
    /// Convert `x` to an `Option[Float32]`.
    ///
    /// Returns `Some(x as Float32)` if the numeric value of `x` is within the range of Float32.
    ///
    /// Warning: even if `x` is in the range of Float32 it may lose precision.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Float32.
    ///
    pub def tryToFloat32(x: BigInt): Option[Float32] =
        match Float32.fromString(toString(x)) {
            case Some(d) if Float32.isFinite(d) => Some(d)
            case _ => None
        }

    ///
    /// Convert `x` to an `Option[Float64]`.
    ///
    /// Returns `Some(x as Float64)` if the numeric value of `x` is within the range of Float64.
    ///
    /// Warning: even if `x` is in the range of Float64 it may lose precision.
    ///
    /// Returns `None` if the numeric value of `x` is outside the range of Float64.
    ///
    pub def tryToFloat64(x: BigInt): Option[Float64] =
        match Float64.fromString(toString(x)) {
            case Some(d) if Float64.isFinite(d) => Some(d)
            case _ => None
        }

    ///
    /// Convert `x` to a `BigDecimal`.
    ///
    pub def toBigDecimal(x: BigInt): BigDecimal =
        match BigDecimal.fromString(toString(x)) {
            case Some(v) => v
            case None => bug!("BigInt.toBigDecimal: unexpected parse failure.")
        }

    ///
    /// Helper function for the `clamp` conversion functions.
    ///
    def clamp(min: {min = BigInt}, max: {max = BigInt}, x: BigInt): BigInt =
        if (x < min#min)
             min#min
        else
            if (x > max#max)
                max#max
            else
                x

    ///
    /// Convert `x` to an Int8.
    ///
    /// Returns `x` clamped within the Int8 range `min` to `max`.
    ///
    pub def clampToInt8(min: {min = Int8}, max: {max = Int8}, x: BigInt): Int8 = {
        let minii = %%BIGINT_FROM_INT64%%(Int8.toInt64(min#min));
        let maxii = %%BIGINT_FROM_INT64%%(Int8.toInt64(max#max));
        let y = clamp(min = minii, max = maxii, x);
        match tryToInt8(y) {
            case Some(v) => v
            case None => min#min
        }
    }

    ///
    /// Convert `x` to an Int16.
    ///
    /// Returns `x` clamped within the Int16 range `min` to `max`.
    ///
    pub def clampToInt16(min: {min = Int16}, max: {max = Int16}, x: BigInt): Int16 = {
        let minii = %%BIGINT_FROM_INT64%%(Int16.toInt64(min#min));
        let maxii = %%BIGINT_FROM_INT64%%(Int16.toInt64(max#max));
        let y = clamp(min = minii, max = maxii, x);
        match tryToInt16(y) {
            case Some(v) => v
            case None => min#min
        }
    }

    ///
    /// Convert `x` to an Int32.
    ///
    /// Returns `x` clamped within the Int32 range `min` to `max`.
    ///
    pub def clampToInt32(min: {min = Int32}, max: {max = Int32}, x: BigInt): Int32 = {
        let minii = %%BIGINT_FROM_INT64%%(Int32.toInt64(min#min));
        let maxii = %%BIGINT_FROM_INT64%%(Int32.toInt64(max#max));
        let y = clamp(min = minii, max = maxii, x);
        match tryToInt32(y) {
            case Some(v) => v
            case None => min#min
        }
    }

    ///
    /// Convert `x` to an Int64.
    ///
    /// Returns `x` clamped within the Int64 range `min` to `max`.
    ///
    pub def clampToInt64(min: {min = Int64}, max: {max = Int64}, x: BigInt): Int64 = {
        let minii = %%BIGINT_FROM_INT64%%(min#min);
        let maxii = %%BIGINT_FROM_INT64%%(max#max);
        let y = clamp(min = minii, max = maxii, x);
        match tryToInt64(y) {
            case Some(v) => v
            case None => min#min
        }
    }

    ///
    /// Convert `x` to a Float32.
    ///
    /// Returns `x` clamped within the Float32 range `min` to `max`.
    ///
    pub def clampToFloat32(min: {min = Float32}, max: {max = Float32}, x: BigInt): Float32 =
        if (not Float32.isFinite(min#min) or not Float32.isFinite(max#max))
            Float32.nan()
        else match Float32.fromString(toString(x)) {
            case None => Float32.nan()
            case Some(d) =>
                if (d < min#min) min#min
                else if (d > max#max) max#max
                else d
        }

    ///
    /// Convert `x` to a Float64.
    ///
    /// Returns `x` clamped within the Float64 range `min` to `max`.
    ///
    pub def clampToFloat64(min: {min = Float64}, max: {max = Float64}, x: BigInt): Float64 =
        if (not Float64.isFinite(min#min) or not Float64.isFinite(max#max))
            Float64.nan()
        else match Float64.fromString(toString(x)) {
            case None => Float64.nan()
            case Some(d) =>
                if (d < min#min) min#min
                else if (d > max#max) max#max
                else d
        }

}
