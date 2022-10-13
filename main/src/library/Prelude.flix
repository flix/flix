///
/// Static is a type alias for false and denotes the global lifetime.
///
pub type alias Static = false

///
/// The Reified Flix Bools.
///
pub enum ReifiedBool with Eq, Order, ToString {
    case ErasedBool,
    case ReifiedTrue,
    case ReifiedFalse
}

///
/// The Reified Flix Types.
///
pub enum ReifiedType with Eq, Order, ToString {
    case ErasedType,
    case ReifiedUnit,
    case ReifiedBool,
    case ReifiedChar,
    case ReifiedFloat32,
    case ReifiedFloat64,
    case ReifiedInt8,
    case ReifiedInt16,
    case ReifiedInt32,
    case ReifiedInt64,
    case ReifiedBigInt,
    case ReifiedString,
    case ReifiedArray(ReifiedType)
}

///
/// The Generic IO Effect.
///
/// The `IO` effect is the most general effect of any
/// operation that interacts with the environment.
///
pub eff IO

///
/// The Non-Deterministic Effect.
///
/// The `NonDet` effect represents "functions" that behave like relations,
/// i.e. they have no side-effects, and given the same input their output
//// there is a fixed set of output.
///
pub eff NonDet

///
/// The identity function.
///
pub def identity(x: a): a = x

///
/// Returns the function `f` with input arguments swapped.
/// That is, given the function `f: (a, b) -> c`, returns a function `(b, a) -> c`
///
pub def flip(f: (a, b) -> c \ ef): (b, a) -> c \ ef = (x, y) -> f(y, x)

///
/// Partially applying this function in the form ``f `on` g`` lifts the binary function `f` to work on
/// inputs transformed by `g`.
///
pub def on(f: (b, b) -> c \ ef1, g: a -> b \ ef2, x: a, y: a): c \ { ef1, ef2 } = f(g(x), g(y))

///
/// Returns the first component of `t`.
///
pub def fst(p: (a, b)): a =
    let (x, _) = p;
        x

///
/// Returns the second component of `t`.
///
pub def snd(p: (a, b)): b =
    let (_, y) = p;
        y

///
/// Returns the pair `p` with the components swapped.
/// That is, returns `(y, x)` if `p = (x, y)`.
///
pub def swap(p: (a, b)): (b, a) =
    let (x, y) = p;
        (y, x)

///
/// Forwards function composition. Applies the function on the left first.
///
/// Given the functions `f: a -> b` and `g: b -> c` returns a function `a -> c`
///
pub def >>(f: a -> b \ ef1, g: b -> c \ ef2): a -> c \ { ef1, ef2 } = x -> g(f(x))

///
/// Pipes the given value `x` into the function `f`.
///
/// Given a value `x` and a function `f` returns `f(x)`.
///
pub def |>(x: a, f: a -> b \ ef): b \ ef = f(x)

///
/// Pipes the given pair `x` into the function `f`.
///
/// Given a pair `x` and a function `f` returns `f(x)`.
///
pub def ||>(x: (a, b), f: a -> (b -> c \ ef)): c \ ef = f(fst(x), snd(x))

///
/// Pipes the given value `x` into the function `f`.
///
/// Given a value `x` and a function `f` returns `x`.
///
pub def !>( x: a, f: a -> Unit \ ef): a \ ef = f(x); x

///
/// Converts `x` to a string and prints it to standard out.
///
pub def print(x: a): Unit \ IO with ToString[a] =
    import static get java.lang.System.out: ##java.io.PrintStream \ IO as getOut;
    import java.io.PrintStream.print(String): Unit \ IO;
    x |> ToString.toString |> print(getOut())

///
/// Converts `x` to a string and prints it to standard out followed by a new line.
///
pub def println(x: a): Unit \ IO with ToString[a] =
    import static get java.lang.System.out: ##java.io.PrintStream \ IO as getOut;
    import java.io.PrintStream.println(String): Unit \ IO;
    x |> ToString.toString |> println(getOut())

///
/// Escapes the given string per Flix's escaping rules.
///
@Internal
pub def escape(s: String): String = region r {
    import new java.lang.StringBuilder(): ##java.lang.StringBuilder \ r as newStringBuilder;
    import java.lang.StringBuilder.toString(): String \ r;
    import java.lang.StringBuilder.append(String): ##java.lang.StringBuilder \ r;
    import static java.lang.Character.isISOControl(Char): Bool \ {};
    import static java.lang.Character.isWhitespace(Char): Bool \ {};
    import static java.lang.Character.isSpaceChar(Char): Bool \ {};
    import static java.lang.String.format(String, Array[##java.lang.Object, r]): String \ r;
    import static java.lang.Integer.valueOf(Int32): ##java.lang.Integer \ {};

    /// Escape control codes and whitespace characters (except normal space)
    def shouldUnicodeEscape(c: Char): Bool = {
        isISOControl(c) or (isWhitespace(c) and not isSpaceChar(c))
    };

    let sb = newStringBuilder();
    def visit(l: List[Char]): Unit \ Write(r) = match l {
        case Nil => ()
        case '\n' :: tl => discard append(sb, "\\n"); visit(tl)
        case '\r' :: tl => discard append(sb, "\\r"); visit(tl)
        case '\\' :: tl => discard append(sb, "\\\\"); visit(tl)
        case '\"' :: tl => discard append(sb, "\\\""); visit(tl)
        case '\'' :: tl => discard append(sb, "\\\'"); visit(tl)
        case '\t' :: tl => discard append(sb, "\\t"); visit(tl)

        case '$' :: '{' :: tl => discard append(sb, "\\" + "$" + "{"); visit(tl)
        case '$' :: tl => discard append(sb, "$"); visit(tl)

        case '%' :: '{' :: tl => discard append(sb, "\\" + "%" + "{"); visit(tl)
        case '%' :: tl => discard append(sb, "%"); visit(tl)

        case c :: tl if (shouldUnicodeEscape(c)) =>
            let number = c as Int32 |> valueOf;
            let code = format("%04x", [number as ##java.lang.Object]);
            discard append(sb, "\\u" + code);
            visit(tl)

        case c :: tl => discard append(sb, "${c}"); visit(tl)
    };
    visit(String.toList(s));
    toString(sb)
}

///
/// Prints the given prefix along with a automatic string representation of `x`.
///
/// Note: For internal use only. The actual `debug(exp)` expression is parsed and ultimately rewritten to call this function.
///
@Internal
def debug(prefix: String, x: a): a \ IO =
    import static get java.lang.System.out: ##java.io.PrintStream \ IO as getOut;
    import java.io.PrintStream.println(String): Unit \ IO;
    println(getOut(), "${prefix}%{x}");
    x

///
/// Returns an automatic string representation of `x`.
///
@Internal
pub def stringify(x: a): String = {
    let magic = match reifyType a {
        case ErasedType =>
            import java.lang.Object.toString(): String;
            toString as a -> String
        case ReifiedUnit =>
            _ -> "()"
        case ReifiedBool =>
            (b -> if (b) "true" else "false") as a -> String
        case ReifiedChar =>
            import static java.lang.Character.toString(Char): String \ {};
            toString as a -> String
        case ReifiedFloat32 =>
            import static java.lang.Float.toString(Float32): String \ {};
            toString as a -> String
        case ReifiedFloat64 =>
            import static java.lang.Double.toString(Float64): String \ {};
            toString as a -> String
        case ReifiedInt8 =>
            import static java.lang.Byte.toString(Int8): String \ {};
            toString as a -> String
        case ReifiedInt16 =>
            import static java.lang.Short.toString(Int16): String \ {};
            toString as a -> String
        case ReifiedInt32 =>
            import static java.lang.Integer.toString(Int32): String \ {};
            toString as a -> String
        case ReifiedInt64 =>
            import static java.lang.Long.toString(Int64): String \ {};
            toString as a -> String
        case ReifiedBigInt =>
            import java.lang.Object.toString(): String;
            toString as a -> String
        case ReifiedString =>
            (s -> "\"" + escape(s) + "\"") as a -> String
        case ReifiedArray(tpe) =>
            match tpe {
                case ErasedType =>
                    import static java.util.Arrays.toString(Array[##java.lang.Object, Static]): String;
                    toString as a -> String
                case ReifiedUnit =>
                    import static java.util.Arrays.toString(Array[##java.lang.Object, Static]): String;
                    toString as a -> String
                case ReifiedBool =>
                    import static java.util.Arrays.toString(Array[Bool, Static]): String;
                    toString as a -> String
                case ReifiedChar =>
                    import static java.util.Arrays.toString(Array[Char, Static]): String;
                    toString as a -> String
                case ReifiedFloat32 =>
                    import static java.util.Arrays.toString(Array[Float32, Static]): String;
                    toString as a -> String
                case ReifiedFloat64 =>
                    import static java.util.Arrays.toString(Array[Float64, Static]): String;
                    toString as a -> String
                case ReifiedInt8 =>
                    import static java.util.Arrays.toString(Array[Int8, Static]): String;
                    toString as a -> String
                case ReifiedInt16 =>
                    import static java.util.Arrays.toString(Array[Int16, Static]): String;
                    toString as a -> String
                case ReifiedInt32 =>
                    import static java.util.Arrays.toString(Array[Int32, Static]): String;
                    toString as a -> String
                case ReifiedInt64 =>
                    import static java.util.Arrays.toString(Array[Int64, Static]): String;
                    toString as a -> String
                case ReifiedBigInt =>
                    import static java.util.Arrays.toString(Array[##java.lang.Object, Static]): String;
                    toString as a -> String
                case ReifiedString =>
                    import static java.util.Arrays.toString(Array[##java.lang.Object, Static]): String;
                    toString as a -> String
                case ReifiedArray(_) =>
                    import static java.util.Arrays.deepToString(Array[##java.lang.Object, Static]): String;
                    deepToString as a -> String
            }
    };
    magic(x)
}

///
/// Crashes the current process with the given message `m`.
///
pub def bug!(m: String): a = {
    import static get java.lang.System.err: ##java.io.PrintStream \ {} as getErr;
    import java.io.PrintStream.println(String): Unit \ IO;
    import java.lang.String.repeat(Int32): ##java.lang.String \ {};
    let prt = println(getErr());
    prt(repeat("*", 80));
    prt("**") ;
    prt("**  BUG: ${m}") ;
    prt("**") ;
    prt(repeat("*", 80));
    prt("");
    ?panic
} as \ {}

///
/// Asserts that this expression is unreachable. Crashes if not.
///
pub def unreachable!(): a = bug!("Reached unreachable expression.")

///
/// Casts a pure function to `ef`.
///
/// `upcast` is safe to use anywhere.
///
pub def upcast(f: a -> b): a -> b \ ef =
    x -> f(x) as \ ef

///
/// Meta-programming facility to express the time complexity of a function `f`.
///
@Internal
pub def time(_: a -> Int32): Int32 = 1

///
/// Meta-programming facility to express the space complexity of a function `f`.
///
@Internal
pub def space(_: a -> Int32): Int32 = 1

///
/// Prints the argument unless it is ().
///
@Internal
pub def printUnlessUnit(x: a): Unit \ IO with ToString[a] = {
    match (reifyType a) {
        case ReifiedUnit => ()
        case _ => println(x)
    }
}
