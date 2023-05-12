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

}
