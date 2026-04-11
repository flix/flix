///
/// Portable `Debug` overlay used by the portable stdlib profile.
///
eff Debug

pub mod Debug {

    ///
    /// Prints `x` to standard out.
    ///
    pub def dprint(x: a): Unit \ (Debug + Formattable.Aef[a]) with Formattable[a] = {
        let s = RichString.toString(Formattable.format(x));
        unsafe IO as Debug { %%PRINT%%(s) }
    }

    ///
    /// Prints `x` to standard out followed by a new line.
    ///
    pub def dprintln(x: a): Unit \ (Debug + Formattable.Aef[a]) with Formattable[a] = {
        let s = RichString.toString(Formattable.format(x));
        unsafe IO as Debug { %%PRINTLN%%(s) }
    }

    ///
    /// Returns an automatic string representation of `x`.
    ///
    pub def stringify(x: a): String = {
        typematch x {
            case b: Bool => if (b) "true" else "false"
            case c: Char => "\'${(c: Char)}\'"
            case y: Float32 => ToString.toString(y) + "f32"
            case y: Float64 => ToString.toString(y)
            case y: BigDecimal =>
                if (Object.isNull(y)) "null" else ToString.toString(y) + "ff"
            case y: Int8 => ToString.toString(y) + "i8"
            case y: Int16 => ToString.toString(y) + "i16"
            case y: Int32 => ToString.toString(y)
            case y: Int64 => ToString.toString(y) + "i64"
            case y: BigInt =>
                if (Object.isNull(y)) "null" else ToString.toString(y) + "ii"
            case s: String =>
                if (Object.isNull(s)) "null" else "\"" + s + "\""
            case y: _ =>
                if (Object.isNull(y)) "null" else "<object>"
        }
    }

}
