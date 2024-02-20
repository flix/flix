mod Reflect {

    ///
    /// Returns a default value for the inferred type.
    /// The value returned should not be depended on;
    /// it acts as a placeholder to be replaced by a proper value.
    ///
    pub def default(): a = typematch (Proxy.Proxy: Proxy[a]) {
        case _: Proxy[Unit]         => unchecked_cast(() as a)
        case _: Proxy[Bool]         => unchecked_cast(false as a)
        case _: Proxy[Char]         => unchecked_cast('0' as a)
        case _: Proxy[Float32]      => unchecked_cast(0.0f32 as a)
        case _: Proxy[Float64]      => unchecked_cast(0.0f64 as a)
        case _: Proxy[BigDecimal]   => unchecked_cast(0.0ff as a)
        case _: Proxy[Int8]         => unchecked_cast(0i8 as a)
        case _: Proxy[Int16]        => unchecked_cast(0i16 as a)
        case _: Proxy[Int32]        => unchecked_cast(0i32 as a)
        case _: Proxy[Int64]        => unchecked_cast(0i64 as a)
        case _: Proxy[BigInt]       => unchecked_cast(0ii as a)
        case _: _                   => unchecked_cast(null as a)
    }

    ///
    /// Returns true if `x` and `y` represent the same runtime value.
    ///
    pub def areIdentical(x: a, y: a): Bool = typematch (x, y) {
        case pair: (Bool, Bool) => $BOOL_EQ$(fst(pair), snd(pair))
        case pair: (Char, Char) => $CHAR_EQ$(fst(pair), snd(pair))
        case pair: (Float32, Float32) => $FLOAT32_EQ$(fst(pair), snd(pair))
        case pair: (Float64, Float64) => $FLOAT64_EQ$(fst(pair), snd(pair))
        case pair: (Int8, Int8) => $INT8_EQ$(fst(pair), snd(pair))
        case pair: (Int16, Int16) => $INT16_EQ$(fst(pair), snd(pair))
        case pair: (Int32, Int32) => $INT32_EQ$(fst(pair), snd(pair))
        case pair: (Int64, Int64) => $INT64_EQ$(fst(pair), snd(pair))
        case _: _ => $OBJECT_EQ$(x, y)
    }
}
