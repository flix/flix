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
    @Internal
    pub def stringify(x: a): String = region rc {
        import static java.util.Objects.isNull(##java.lang.Object): Bool \ {};
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
            case y: Array[_, rc] =>
                let s = ref "[" @ rc;
                let first = ref true @ rc;

                let len = $ARRAY_LENGTH$(y);
                def loop(i) = {
                    if (i >= len)
                        ()
                    else {
                        let v = $ARRAY_LOAD$(y, i);
                        if (not deref first) s := deref s + ", " else first := false;
                        s := deref s + Debug.stringify(v);
                        loop(i + 1)
                    }
                };
                loop(0);

                s := deref s + "]";
                deref s
            case y: Vector[_] =>
                let s = ref "Vector#{" @ rc;
                let first = ref true @ rc;

                let len = $VECTOR_LENGTH$(y);
                def loop(i) = {
                    if (i >= len)
                        ()
                    else {
                        let v = $VECTOR_GET$(y, i);
                        if (not deref first) s := deref s + ", " else first := false;
                        s := deref s + Debug.stringify(v);
                        loop(i + 1)
                    }
                };
                loop(0);

                s := deref s + "}";
                deref s
            case y: _ =>
                import static java.util.Objects.toString(##java.lang.Object): String \ {};
                toString(unchecked_cast(y as ##java.lang.Object))
        }
    }

}
