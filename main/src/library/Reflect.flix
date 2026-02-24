pub mod Reflect {
    import java.lang.{Object => JObject}

    ///
    /// Returns a default value for the inferred type.
    /// The value returned should not be depended on;
    /// it acts as a placeholder to be replaced by a proper value.
    ///
    pub def default(): a = match Reflect.reflectType((Proxy.Proxy: Proxy[a])) {
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
    pub def reflectValue(x: t): JvmValue = %%REFLECT_VALUE%%(x)

    /// Returns the erased runtime representation of `t`.
    pub def reflectType(x: Proxy[t]): JvmType = %%REFLECT_TYPE%%(x)

    /// Returns the purity of `ef`.
    pub def reflectEff(p: ProxyEff[ef]): Purity = %%REFLECT_EFF%%(p)

    /// A reflected type, reflecting its erased runtime representation.
    pub enum JvmType {
        case JvmBool
        case JvmChar
        case JvmInt8
        case JvmInt16
        case JvmInt32
        case JvmInt64
        case JvmFloat32
        case JvmFloat64
        case JvmObject
    }

    /// A reflected effect, either Pure (no effects) or Impure (some effects).
    pub enum Purity {
        case Pure
        case Impure
    }

    ///
    /// Represents a zero-width position within a source, identified by line and column coordinates.
    ///
    /// Both line and column numbers use one-based indexing.
    /// (e.g., the first character of a source is at line 1, column 1).
    ///
    /// Fields:
    ///
    /// 1. `line`: The line number (1-based). Must be >= 1.
    /// 2. `column`: The column number (1-based) within the specified line. Must be >= 1.
    ///
    pub enum SourcePosition with Coerce {
        case SourcePosition(Int32, Int32)
    }

    ///
    /// Represents a span or range within a source, defined by a source identifier and
    /// start/end source positions.
    ///
    /// Fields:
    ///
    /// 1. `source`: The source identifier (typically a file path)
    /// 2. `start`: The beginning position of the span (inclusive)
    /// 3. `end`: The ending position of the span (inclusive)
    ///
    pub enum SourceLocation with Coerce {
        case SourceLocation(String, SourcePosition, SourcePosition)
    }

    ///
    /// Creates a `SourceLocation` from a source identifier and raw line/column coordinates.
    ///
    /// Returns a `SourceLocation` spanning from (`line1`, `col1`) to (`line2`, `col2`) in the given source.
    ///
    pub def mkSourceLocation(source: String, line1: Int32, col1: Int32, line2: Int32, col2: Int32) : SourceLocation =
        use SourceLocation.SourceLocation;
        use SourcePosition.SourcePosition;
        SourceLocation(
                source,
                SourcePosition(line1, col1),
                SourcePosition(line2, col2)
        )
}
