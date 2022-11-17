namespace Reflect {

    ///
    /// Returns a default value for the inferred type.
    /// The value returned should not be depended on;
    /// it acts as a placeholder to be replaced by a proper value.
    ///
    @Unsafe
    pub def default(): a = typematch (Proxy: Proxy[a]) return a {
        case _: Proxy[Unit] => ()
        case _: Proxy[Bool] => false
        case _: Proxy[Char] => '0'
        case _: Proxy[Float32] => 0.0f32
        case _: Proxy[Float64] => 0.0f64
        case _: Proxy[BigDecimal] => 0.0ff
        case _: Proxy[Int8] => 0i8
        case _: Proxy[Int16] => 0i16
        case _: Proxy[Int32] => 0i32
        case _: Proxy[Int64] => 0i64
        case _: Proxy[BigInt] => 0ii
        case _: _ => null as a
    }
}
