mod Debug {
    ///
    /// Prints the stringified representation of `x` with the given `prefix`.
    ///
    /// Note: For internal use only. The actual `debug(exp)` expression is parsed and ultimately rewritten to call this function.
    ///
    @internal
    pub def debugWithPrefix(prefix: String, x: a): a \ IO =
        import static get java.lang.System.out: ##java.io.PrintStream \ IO as getOut;
        import java.io.PrintStream.println(String): Unit \ IO;
        println(getOut(), "${prefix}%{x}");
        x

    ///
    /// Escapes the given string per Flix's escaping rules.
    ///
    @internal
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
    @internal
    pub def stringify(x: a): String = region rc {
        import static java.util.Objects.isNull(##java.lang.Object): Bool \ {};
        let _ = rc;
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
            case y: Array[Bool, rc] =>
                import static java.util.Arrays.toString(Array[Bool, rc]): String \ {};
                toString(y)
            case y: Array[Char, rc] =>
                import static java.util.Arrays.toString(Array[Char, rc]): String \ {};
                toString(y)
            case y: Array[Float32, rc] =>
                import static java.util.Arrays.toString(Array[Float32, rc]): String \ {};
                toString(y)
            case y: Array[Float64, rc] =>
                import static java.util.Arrays.toString(Array[Float64, rc]): String \ {};
                toString(y)
            case y: Array[Int8, rc] =>
                import static java.util.Arrays.toString(Array[Int8, rc]): String \ {};
                toString(y)
            case y: Array[Int16, rc] =>
                import static java.util.Arrays.toString(Array[Int16, rc]): String \ {};
                toString(y)
            case y: Array[Int32, rc] =>
                import static java.util.Arrays.toString(Array[Int32, rc]): String \ {};
                toString(y)
            case y: Array[Int64, rc] =>
                import static java.util.Arrays.toString(Array[Int64, rc]): String \ {};
                toString(y)
            case y: Array[Unit, rc] =>
                import static java.util.Arrays.toString(Array[##java.lang.Object, rc]): String \ {};
                toString(unchecked_cast(y as Array[##java.lang.Object, rc]))
            case y: Array[BigInt, rc] =>
                import static java.util.Arrays.toString(Array[##java.lang.Object, rc]): String \ {};
                toString(unchecked_cast(y as Array[##java.lang.Object, rc]))
            case y: Array[BigDecimal, rc] =>
                import static java.util.Arrays.toString(Array[##java.lang.Object, rc]): String \ {};
                toString(unchecked_cast(y as Array[##java.lang.Object, rc]))
            case y: Array[String, rc] =>
                import static java.util.Arrays.toString(Array[##java.lang.Object, rc]): String \ {};
                toString(unchecked_cast(y as Array[##java.lang.Object, rc]))
            case y: Array[Array[_, _], _] =>
                import static java.util.Arrays.deepToString(Array[##java.lang.Object, rc]): String \ {};
                deepToString(unchecked_cast(y as Array[##java.lang.Object, rc]))
            case y: Array[_, _] =>
                import static java.util.Arrays.toString(Array[##java.lang.Object, rc]): String \ {};
                toString(unchecked_cast(y as Array[##java.lang.Object, rc]))
            case y: _ =>
                import static java.util.Objects.toString(##java.lang.Object): String \ {};
                toString(unchecked_cast(y as ##java.lang.Object))
        }
    }

}
