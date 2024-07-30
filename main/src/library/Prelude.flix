///
/// Static denotes the region of global lifetime.
///
pub type alias Static = IO

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
/// The pure identity function.
///
pub def identity(x: a): a = x

///
/// The identity function, automatically casting to the required effect.
///
pub def eidentity(x: a): a \ ef = checked_ecast(x)

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
/// Coerces the given value `x`.
///
pub def coerce(x: t): Coerce.Out[t] with Coerce[t] = Coerce.coerce(x)

///
/// Converts `x` to a string and prints it to standard out followed by a new line.
///
pub def println(x: a): Unit \ IO with ToString[a] =
    import static java_get_field java.lang.System.out: ##java.io.PrintStream \ IO as getOut;
    import java.io.PrintStream.println(String): Unit \ IO;
    x |> ToString.toString |> println(getOut())

///
/// Touches the given region capability `rc`.
///
pub def touch(_: Region[r]): Unit \ r = checked_ecast(())

///
/// Crashes the current process with the given message `m`.
///
pub def bug!(m: String): a = masked_cast({
    import static java_get_field java.lang.System.err: ##java.io.PrintStream \ {} as getErr;
    import java.io.PrintStream.println(String): Unit \ IO;
    let prt = println(getErr());
    prt("*".repeat(80));
    prt("**") ;
    prt("**  BUG: ${m}") ;
    prt("**") ;
    prt("*".repeat(80));
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
@Internal
pub def printUnlessUnit(x: a): Unit \ IO with ToString[a] = {
    typematch x {
        case _: Unit => ()
        case _: _    => println(x)
    }
}

pub enum Purity[a: Type, b: Type, ef: Eff] {
    case Pure(a -> b)
    case Impure(a -> b \ ef)
}

pub enum Purity2[a: Type, b: Type, c: Type, ef: Eff] {
    case Pure(a -> b -> c)
    case Impure(a -> b -> c \ ef)
}

pub enum Purity3[a: Type, b: Type, c: Type, d: Type, ef: Eff] {
    case Pure(a -> b -> c -> d)
    case Impure(a -> b -> c -> d \ ef)
}

///
/// Returns `Purity.Pure(g)` if `f` has no effects.
/// Returns `Purity.Impure(g)` otherwise.
/// The function `g` can then be used in a context that requires
/// it to be pure.
///
pub def purityOf(f: a -> b \ ef): Purity[a, b, ef] = typematch f {
    case g: a -> b \ {} => Purity.Pure(g)
    case _: _           => Purity.Impure(f)
}

///
/// Returns `Purity2.Pure(g)` if `f` has no effects.
/// Returns `Purity2.Impure(g)` otherwise.
/// The function `g` can then be used in a context that requires
/// it to be pure.
///
pub def purityOf2(f: a -> b -> c \ ef): Purity2[a, b, c, ef] = typematch f {
    case g: a -> b -> c \ {} => Purity2.Pure(g)
    case _: _                => Purity2.Impure(f)
}

///
/// Returns `Purity3.Pure(g)` if `f` has no effects.
/// Returns `Purity3.Impure(g)` otherwise.
/// The function `g` can then be used in a context that requires
/// it to be pure.
///
pub def purityOf3(f: a -> b -> c -> d \ ef): Purity3[a, b, c, d, ef] = typematch f {
    case g: a -> b -> c -> d \ {} => Purity3.Pure(g)
    case _: _                     => Purity3.Impure(f)
}
