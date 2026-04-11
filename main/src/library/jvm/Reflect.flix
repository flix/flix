///
/// JVM-only extensions for `Reflect`.
///

pub mod Reflect {

    import java.lang.{Object => JObject}

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

}
