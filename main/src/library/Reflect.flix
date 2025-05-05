mod Reflect {

    ///
    /// Returns a default value for the inferred type.
    /// The value returned should not be depended on;
    /// it acts as a placeholder to be replaced by a proper value.
    ///
    pub def default(): a = {
        let proxy: Proxy[a] = Proxy.Proxy;
        typematch proxy {
            case _: Proxy[Unit]         => unchecked_cast(() as a)
            case _: Proxy[BigDecimal]   => unchecked_cast(0.0ff as a)
            case _: Proxy[BigInt]       => unchecked_cast(0ii as a)
            case _: _ => match JvmReflection.reflectType(proxy) {
                case JvmType.JvmBool    => unchecked_cast(false as a)
                case JvmType.JvmChar    => unchecked_cast('0' as a)
                case JvmType.JvmFloat32 => unchecked_cast(0.0f32 as a)
                case JvmType.JvmFloat64 => unchecked_cast(0.0f64 as a)
                case JvmType.JvmInt8    => unchecked_cast(0i8 as a)
                case JvmType.JvmInt16   => unchecked_cast(0i16 as a)
                case JvmType.JvmInt32   => unchecked_cast(0i32 as a)
                case JvmType.JvmInt64   => unchecked_cast(0i64 as a)
                case JvmType.JvmObject  => unchecked_cast(null as a)
            }
        }
    }

    }

}
