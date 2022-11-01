namespace Reflect {

    ///
    /// Returns a default value for the inferred type.
    /// The value returned should not be depended on;
    /// it acts as a placeholder to be replaced by a proper value.
    ///
    @Unsafe
    pub def default(): a = typematch (Proxy: Proxy[a]) {
        case _: Proxy[Unit] => () as a
        case _: Proxy[Bool] => false as a
        case _: Proxy[Char] => '0' as a
        case _: Proxy[Float32] => 0.0f32 as a
        case _: Proxy[Float64] => 0.0f64 as a
        case _: Proxy[BigDecimal] => 0.0ff as a
        case _: Proxy[Int8] => 0i8 as a
        case _: Proxy[Int16] => 0i16 as a
        case _: Proxy[Int32] => 0i32 as a
        case _: Proxy[Int64] => 0i64 as a
        case _: Proxy[BigInt] => 0ii as a
        case _: _ => null as a
    }
}
