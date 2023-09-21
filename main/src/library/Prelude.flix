///
/// Static is a type alias for false and denotes the global lifetime.
///
pub type alias Static = Impure

///
/// The empty type.
///
pub enum Void

///
/// An enum that holds type information where a witness is not available.
///
pub enum Proxy[_] {
    case Proxy
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
pub def identity(x: a): a \ ef = checked_ecast(x)

///
/// The constant function.
///
pub def constant(x: a): b -> a \ ef = _ -> checked_ecast(x)

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
/// Converts `x` to a string and prints it to standard out followed by a new line.
///
pub def println(x: a): Unit \ IO with ToString[a] =
    import static get java.lang.System.out: ##java.io.PrintStream \ IO as getOut;
    import java.io.PrintStream.println(String): Unit \ IO;
    x |> ToString.toString |> println(getOut())

///
/// Touches the given region capability `rc`.
///
pub def touch(_: Region[r]): Unit \ r = checked_ecast(())

///
/// Prints the stringified representation of `x`.
///
/// WARNING: The debug function is pure hence if the
/// result is unused it may be removed by the compiler!
///
def debug(x: a): a = masked_cast(Debug.debugWithPrefix("", x))

///
/// Prints the stringified representation of `x`.
///
/// WARNING: The debug function is pure hence if the
/// result is unused it may be removed by the compiler!
///
def debug!(x: a): a = masked_cast(Debug.debugWithPrefix("", x))

///
/// Prints the stringified representation of `x`.
///
/// WARNING: The debug function is pure hence if the
/// result is unused it may be removed by the compiler!
///
def debug!!(x: a): a = masked_cast(Debug.debugWithPrefix("", x))

///
/// Crashes the current process with the given message `m`.
///
pub def bug!(m: String): a = masked_cast({
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
})

///
/// Asserts that this expression is unreachable. Crashes if not.
///
pub def unreachable!(): a = bug!("Reached unreachable expression.")

///
/// Prints the argument unless it is ().
///
@internal
pub def printUnlessUnit(x: a): Unit \ IO with ToString[a] = {
    typematch x {
        case _: Unit => ()
        case _: _ => println(x)
    }
}
