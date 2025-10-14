mod Reflect {
    import java.lang.{Object => JObject}

    ///
    /// Returns a default value for the inferred type.
    /// The value returned should not be depended on;
    /// it acts as a placeholder to be replaced by a proper value.
    ///
    pub def default(): a = typematch (Proxy.Proxy: Proxy[a]) {
        case _: Proxy[Bool]         => unchecked_cast(false as a)
        case _: Proxy[Char]         => unchecked_cast('0' as a)
        case _: Proxy[Float32]      => unchecked_cast(0.0f32 as a)
        case _: Proxy[Float64]      => unchecked_cast(0.0f64 as a)
        case _: Proxy[Int8]         => unchecked_cast(0i8 as a)
        case _: Proxy[Int16]        => unchecked_cast(0i16 as a)
        case _: Proxy[Int32]        => unchecked_cast(0i32 as a)
        case _: Proxy[Int64]        => unchecked_cast(0i64 as a)
        case _: _                   => unchecked_cast(null as a)
    }

    /// A reflected value, reflecting its erased runtime representation.
    pub enum JvmValue {
        case JvmBool(Bool)
        case JvmChar(Char)
        case JvmInt8(Int8)
        case JvmInt16(Int16)
        case JvmInt32(Int32)
        case JvmInt64(Int64)
        case JvmFloat32(Float32)
        case JvmFloat64(Float64)
        case JvmObject(JObject)
    }

    /// Returns the erased runtime representation of `x`.
    pub def reflect(x: t): JvmValue =
        typematch x {
            case b: Bool => JvmValue.JvmBool(b)
            case c: Char => JvmValue.JvmChar(c)
            case i: Int8 => JvmValue.JvmInt8(i)
            case i: Int16 => JvmValue.JvmInt16(i)
            case i: Int32 => JvmValue.JvmInt32(i)
            case i: Int64 => JvmValue.JvmInt64(i)
            case f: Float32 => JvmValue.JvmFloat32(f)
            case f: Float64 => JvmValue.JvmFloat64(f)
            case _: _ => JvmValue.JvmObject(unchecked_cast(x as JObject))
        }

    /// A reflected effect, either Pure (no effects) or Impure (some effects).
    pub enum Purity {
        case Pure
        case Impure
    }

    /// Returns the purity of `ef`.
    pub def reflectEffect(p: ProxyEff[ef]): Purity =
        typematch p {
            case _: ProxyEff[{}] => Purity.Pure
            case _: _            => Purity.Impure
        }

    /// Returns the bool of `b`.
    pub def reflectBool(p: ProxyBool[b]): Bool =
        typematch p {
            case _: ProxyBool[true] => true
            case _: _               => false
        }

}
