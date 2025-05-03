mod Reflect {

    ///
    /// Returns a default value for the inferred type.
    /// The value returned should not be depended on;
    /// it acts as a placeholder to be replaced by a proper value.
    ///
    pub def default(): a = match $JVM_TYPE_OF_PROXY$((Proxy.Proxy: Proxy[a])) {
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
