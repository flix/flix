///
/// Static is a type alias for false and denotes the global lifetime.
///
pub type alias Static = false

///
/// An enum that holds type information where a witness is not available.
///
pub enum Proxy[_] {
    case Proxy
}

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
    case ReifiedBigDecimal,
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
/// Prints the stringified representation of `x`.
///
/// WARNING: The debug function is pure hence if the
/// result is unused it may be removed by the compiler!
///
def debug(x: a): a = debugWithPrefix("", x) as \ {}

///
/// Prints the stringified representation of `x`.
///
/// WARNING: The debug function is pure hence if the
/// result is unused it may be removed by the compiler!
///
def debug!(x: a): a = debugWithPrefix("", x) as \ {}

///
/// Prints the stringified representation of `x`.
///
/// WARNING: The debug function is pure hence if the
/// result is unused it may be removed by the compiler!
///
def debug!!(x: a): a = debugWithPrefix("", x) as \ {}

///
/// Prints the stringified representation of `x` with the given `prefix`.
///
/// Note: For internal use only. The actual `debug(exp)` expression is parsed and ultimately rewritten to call this function.
///
@Internal
def debugWithPrefix(prefix: String, x: a): a \ IO =
    import static get java.lang.System.out: ##java.io.PrintStream \ IO as getOut;
    import java.io.PrintStream.println(String): Unit \ IO;
    println(getOut(), "${prefix}%{x}");
    x

///
/// Escapes the given string per Flix's escaping rules.
///
@Internal
pub def escape(s: String): String = {
    import java.lang.String.replace(##java.lang.CharSequence, ##java.lang.CharSequence): String \ {} as jReplace;
    def replace(old: String, new: String, subject: String): String = jReplace(subject, old as ##java.lang.CharSequence, new as ##java.lang.CharSequence);
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
pub def stringify(x: a): String = typematch x {
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
        toString(y) + "ff"
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
        toString(y) + "ii"
    case s: String =>
        "\"" + escape(s) + "\""
    case y: Array[Unit, _] =>
        import static java.util.Arrays.toString(Array[##java.lang.Object, Static]): String;
        toString(y as Array[##java.lang.Object, Static]) as \ {}
    case y: Array[Bool, _] =>
        import static java.util.Arrays.toString(Array[Bool, Static]): String;
        toString(y as Array[Bool, Static]) as \ {}
    case y: Array[Char, _] =>
        import static java.util.Arrays.toString(Array[Char, Static]): String;
        toString(y as Array[Char, Static]) as \ {}
    case y: Array[Float32, _] =>
        import static java.util.Arrays.toString(Array[Float32, Static]): String;
        toString(y as Array[Float32, Static]) as \ {}
    case y: Array[Float64, _] =>
        import static java.util.Arrays.toString(Array[Float64, Static]): String;
        toString(y as Array[Float64, Static]) as \ {}
    case y: Array[BigDecimal, _] =>
        import static java.util.Arrays.toString(Array[##java.lang.Object, Static]): String;
        toString(y as Array[##java.lang.Object, Static]) as \ {}
    case y: Array[Int8, _] =>
        import static java.util.Arrays.toString(Array[Int8, Static]): String;
        toString(y as Array[Int8, Static]) as \ {}
    case y: Array[Int16, _] =>
        import static java.util.Arrays.toString(Array[Int16, Static]): String;
        toString(y as Array[Int16, Static]) as \ {}
    case y: Array[Int32, _] =>
        import static java.util.Arrays.toString(Array[Int32, Static]): String;
        toString(y as Array[Int32, Static]) as \ {}
    case y: Array[Int64, _] =>
        import static java.util.Arrays.toString(Array[Int64, Static]): String;
        toString(y as Array[Int64, Static]) as \ {}
    case y: Array[BigInt, _] =>
        import static java.util.Arrays.toString(Array[##java.lang.Object, Static]): String;
        toString(y as Array[##java.lang.Object, Static]) as \ {}
    case y: Array[String, _] =>
        import static java.util.Arrays.toString(Array[##java.lang.Object, Static]): String;
        toString(y as Array[##java.lang.Object, Static]) as \ {}
    case y: Array[Array[_, _], _] =>
        import static java.util.Arrays.deepToString(Array[##java.lang.Object, Static]): String;
        deepToString(y as Array[##java.lang.Object, Static]) as \ {}
    case y: Array[_, _] =>
        import static java.util.Arrays.toString(Array[##java.lang.Object, Static]): String;
        toString(y as Array[##java.lang.Object, Static]) as \ {}
    case y: _ =>
        import static java.util.Objects.toString(##java.lang.Object): String;
        toString(y as ##java.lang.Object) as \ {}
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
