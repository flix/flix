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
    case ReifiedArray(ReifiedType)
}

///
/// The identity function.
///
pub def identity(x: a): a = x


///
/// Returns the function `f` with input arguments swapped.
/// That is, given the function `f: (a, b) -> c`, returns a function `(b, a) -> c`
///
pub def flip(f: (a, b) -> c & ef): (b, a) -> c & ef = (x, y) -> f(y, x)

///
/// Partially applying this function in the form ``f `on` g`` lifts the binary function `f` to work on
/// inputs transformed by `g`.
///
pub def on(f: (b, b) -> c & ef1, g: a -> b & ef2, x: a, y: a): c \ { ef1, ef2 } = f(g(x), g(y))

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
pub def >>(f: a -> b & ef1, g: b -> c & ef2): a -> c \ { ef1, ef2 } = x -> g(f(x))

///
/// Pipes the given value `x` into the function `f`.
///
/// Given a value `x` and a function `f` returns `f(x)`.
///
pub def |>(x: a, f: a -> b & ef): b & ef = f(x)

///
/// Pipes the given pair `x` into the function `f`.
///
/// Given a pair `x` and a function `f` returns `f(x)`.
///
pub def ||>(x: (a, b), f: a -> (b -> c & ef)): c & ef = f(fst(x), snd(x))

///
/// Pipes the given value `x` into the function `f`.
///
/// Given a value `x` and a function `f` returns `x`.
///
pub def !>( x: a, f: a -> Unit & ef): a & ef = f(x); x

///
/// Converts `x` to a string and prints it to standard out.
///
pub def print(x: a): Unit & Impure with ToString[a] =
    import static get java.lang.System.out: ##java.io.PrintStream & Impure as getOut;
    import java.io.PrintStream.print(String): Unit & Impure;
    x |> ToString.toString |> print(getOut())

///
/// Converts `x` to a string and prints it to standard out followed by a new line.
///
pub def println(x: a): Unit & Impure with ToString[a] =
    import static get java.lang.System.out: ##java.io.PrintStream & Impure as getOut;
    import java.io.PrintStream.println(String): Unit & Impure;
    x |> ToString.toString |> println(getOut())

///
/// Crashes the current process with the given message `m`.
///
pub def bug!(m: String): a =
    let f = () -> {
        import static get java.lang.System.err: ##java.io.PrintStream & Pure as getErr;
        import java.io.PrintStream.println(String): Unit & Impure;
        import java.lang.String.repeat(Int32): ##java.lang.String & Pure;
        let prt = println(getErr());
        prt(repeat("*", 80));
        prt("**") ;
        prt("**  BUG: ${m}") ;
        prt("**") ;
        prt(repeat("*", 80));
        prt("")
    };
    let _ = f() as & Pure;
    ?panic

///
/// Asserts that this expression is unreachable. Crashes if not.
///
pub def unreachable!(): a = bug!("Reached unreachable expression.")

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
pub def printUnlessUnit(x: a): Unit & Impure with ToString[a] = {
    match (reifyType a) {
        case ReifiedUnit => ()
        case _ => println(x)
    }
}
