namespace Reflect {

    ///
    /// Returns a default value for the inferred type.
    /// The value returned should not be depended on;
    /// it acts as a placeholder to be replaced by a proper value.
    ///
    @Unsafe
    pub def default(): a = typematch (Proxy: Proxy[a]) {
        case _: Proxy[Unit] => unsafe_cast () as a
        case _: Proxy[Bool] => unsafe_cast false as a
        case _: Proxy[Char] => unsafe_cast '0' as a
        case _: Proxy[Float32] => unsafe_cast 0.0f32 as a
        case _: Proxy[Float64] => unsafe_cast 0.0f64 as a
        case _: Proxy[BigDecimal] => unsafe_cast 0.0ff as a
        case _: Proxy[Int8] => unsafe_cast 0i8 as a
        case _: Proxy[Int16] => unsafe_cast 0i16 as a
        case _: Proxy[Int32] => unsafe_cast 0i32 as a
        case _: Proxy[Int64] => unsafe_cast 0i64 as a
        case _: Proxy[BigInt] => unsafe_cast 0ii as a
        case _: _ => unsafe_cast null as a
    }
}
