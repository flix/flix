mod Debug {
    ///
    /// Prints the stringified representation of `x` with the given `prefix`.
    ///
    /// Note: For internal use only. The actual `debug(exp)` expression is parsed and ultimately rewritten to call this function.
    ///
    @Internal
    pub def debugWithPrefix(prefix: String, x: a): a \ IO =
        import static get java.lang.System.out: ##java.io.PrintStream \ IO as getOut;
        import java.io.PrintStream.println(String): Unit \ IO;
        println(getOut(), "${prefix}%{x}");
        x

    ///
    /// Escapes the given string per Flix's escaping rules.
    ///
    @Internal
    def escape(s: String): String = {
        import java.lang.String.replace(##java.lang.CharSequence, ##java.lang.CharSequence): String \ {} as jReplace;
        def replace(from: String, to: String, subject: String): String = jReplace(subject, checked_cast(from), checked_cast(to));
        s
            // NB: \\ must come first to avoid clobbering other cases
            |> replace("\\", "\\\\")
            |> replace("\n", "\\n")
            |> replace("\r", "\\r")
            |> replace("\"", "\\\"")
            |> replace("\'", "\\\'")
            |> replace("\t", "\\t")
    }

    ///
    /// Returns an automatic string representation of `x`.
    ///
    @Internal @Unsafe
    pub def stringify(x: a): String = region r {
        import static java.util.Objects.isNull(##java.lang.Object): Bool \ {};
        let _ = r;
        typematch x {
            case _: Unit => "()"
            case b: Bool =>  if (b) "true" else "false"
            case c: Char => "\'" + escape("${c: Char}") + "\'"
            case y: Float32 =>
                import static java.lang.Float.toString(Float32): String \ {};
                toString(y) + "f32"
            case y: Float64 =>
                import static java.lang.Double.toString(Float64): String \ {};
                toString(y)
            case y: BigDecimal =>
                import java.math.BigDecimal.toString(): String \ {};
                if (isNull(checked_cast(y))) {
                    "null"
                } else {
                    toString(y) + "ff"
                }
            case y: Int8 =>
                import static java.lang.Byte.toString(Int8): String \ {};
                toString(y) + "i8"
            case y: Int16 =>
                import static java.lang.Short.toString(Int16): String \ {};
                toString(y) + "i16"
            case y: Int32 =>
                import static java.lang.Integer.toString(Int32): String \ {};
                toString(y)
            case y: Int64 =>
                import static java.lang.Long.toString(Int64): String \ {};
                toString(y) + "i64"
            case y: BigInt =>
                import java.math.BigInteger.toString(): String \ {};
                if (isNull(checked_cast(y))) {
                    "null"
                } else {
                    toString(y) + "ii"
                }
            case s: String =>
                if (isNull(checked_cast(s))) {
                    "null"
                } else {
                    "\"" + escape(s) + "\""
                }
            case y: Array[Bool, r] =>
                import static java.util.Arrays.toString(Array[Bool, r]): String \ {};
                toString(y)
            case y: Array[Char, r] =>
                import static java.util.Arrays.toString(Array[Char, r]): String \ {};
                toString(y)
            case y: Array[Float32, r] =>
                import static java.util.Arrays.toString(Array[Float32, r]): String \ {};
                toString(y)
            case y: Array[Float64, r] =>
                import static java.util.Arrays.toString(Array[Float64, r]): String \ {};
                toString(y)
            case y: Array[Int8, r] =>
                import static java.util.Arrays.toString(Array[Int8, r]): String \ {};
                toString(y)
            case y: Array[Int16, r] =>
                import static java.util.Arrays.toString(Array[Int16, r]): String \ {};
                toString(y)
            case y: Array[Int32, r] =>
                import static java.util.Arrays.toString(Array[Int32, r]): String \ {};
                toString(y)
            case y: Array[Int64, r] =>
                import static java.util.Arrays.toString(Array[Int64, r]): String \ {};
                toString(y)
            case y: Array[Unit, r] =>
                import static java.util.Arrays.toString(Array[##java.lang.Object, r]): String \ {};
                toString(unchecked_cast(y as Array[##java.lang.Object, r]))
            case y: Array[BigInt, r] =>
                import static java.util.Arrays.toString(Array[##java.lang.Object, r]): String \ {};
                toString(unchecked_cast(y as Array[##java.lang.Object, r]))
            case y: Array[BigDecimal, r] =>
                import static java.util.Arrays.toString(Array[##java.lang.Object, r]): String \ {};
                toString(unchecked_cast(y as Array[##java.lang.Object, r]))
            case y: Array[String, r] =>
                import static java.util.Arrays.toString(Array[##java.lang.Object, r]): String \ {};
                toString(unchecked_cast(y as Array[##java.lang.Object, r]))
            case y: Array[Array[_, _], _] =>
                import static java.util.Arrays.deepToString(Array[##java.lang.Object, r]): String \ {};
                deepToString(unchecked_cast(y as Array[##java.lang.Object, r]))
            case y: Array[_, _] =>
                import static java.util.Arrays.toString(Array[##java.lang.Object, r]): String \ {};
                toString(unchecked_cast(y as Array[##java.lang.Object, r]))
            case y: _ =>
                import static java.util.Objects.toString(##java.lang.Object): String \ {};
                toString(unchecked_cast(y as ##java.lang.Object))
        }
    }

}
