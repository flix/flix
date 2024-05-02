use Gen.{Gen, Arbitrary, return}
use Functor.{map}

/// A result of testing a property
///
/// `ok` is `Some` if the test passed, `None` if the test is not applicable
///
/// `stamp` is a list of strings that describe the test
///
/// `arguments` is a list of the arguments (in string form) used in the test
pub type alias TResult = {
    ok = Option[Bool],
    stamp = List[String],
    arguments = List[String]
}

/// A property generates results for some testable type
pub enum Property(Gen[TResult])

/// A result that indicates unapplicable test
pub def nothing(): TResult =
    {ok = None, stamp = Nil, arguments = Nil}

/// Creates a property from a generator always returning the same result
pub def result(r: TResult): Property =
    Property.Property(return(r))
    
/// Evaluates a property and returns a generator for the result
pub def evaluate(x: a): Gen[TResult] with Testable[a] = 
    let Property.Property(gen) = Testable.property(x);
    gen

/// The trait for a type that can be tested
pub trait Testable[a] {
    pub def property(x: a): Property
}

/// The unit type is not applicable for testing, as there is no tangible result
instance Testable[Unit] {
    pub def property(_: Unit): Property =
        result(nothing())
}

/// Instance of `Testable` for `Bool`
instance Testable[Bool] {
    pub def property(b: Bool): Property =
        result({ok = Some(b), stamp = Nil, arguments = Nil})
}

pub enum TestableResult(TResult)

instance Testable[TestableResult] {
    pub def property(r: TestableResult): Property =
        let TestableResult.TestableResult(res) = r;
        result(res)
}

instance Testable[Property] {
    pub def property(prop: Property): Property =
        prop
}

/// Functions must be wrapped in TFunc to be testable
pub enum TFunc[a, b](a -> b)

/// Functions (with 2 params) must be wrapped in TFunc2 to be testable
pub enum TFunc2[a, b, c](a -> b -> c)

/// Instance of `Testable` for functions of type `a -> b`
instance Testable[TFunc[a, b]] with Arbitrary[a], ToString[a], Testable[b] {
    pub def property(f: TFunc[a, b]): Property =
        let TFunc.TFunc(func) = f;
        forAll(Arbitrary.arbitrary(), func)
}

/// Instance of `Testable` for functions of type `(a, b) -> c`
instance Testable[TFunc2[a, b, c]] with Arbitrary[a], Arbitrary[b], ToString[a], ToString[b], Testable[c] {
    pub def property(f: TFunc2[a, b, c]): Property =
        let TFunc2.TFunc2(func) = f;
        forAll2(Arbitrary.arbitrary(), Arbitrary.arbitrary(), func)
}

pub def forAll(aGen: Gen[a], f: a -> b): Property with ToString[a], Testable[b] =
    Property.Property(
        forM (
            a <- aGen;
            res <- evaluate(f(a))
        ) yield (
            addArgument(a, res)
        )
    )

pub def forAll2(aGen: Gen[a], bGen: Gen[b], f: (a, b) -> c): Property with ToString[a], ToString[b], Testable[c] =
    Property.Property(
        forM (
            a <- aGen;
            b <- bGen;
            res <- evaluate(f(a, b))
        ) yield (
            addArgument(a, res) |> addArgument(b)
        )
    )

pub def forAll3(aGen: Gen[a], bGen: Gen[b], cGen: Gen[c], f: (a, b, c) -> d): Property with ToString[a], ToString[b], ToString[c], Testable[d] =
    Property.Property(
        forM (
            a <- aGen;
            b <- bGen;
            c <- cGen;
            res <- evaluate(f(a, b, c))
        ) yield (
            addArgument(a, res) |> addArgument(b) |> addArgument(c)
        )
    )

pub def addArgument(arg: a, res: TResult): TResult with ToString[a] =
    let aString = ToString.toString(arg);
    {arguments = List.append(res.arguments, aString :: Nil) | res}

pub def implies(b: Bool, x: t): Property with Testable[t] =
    match b {
        case true => Testable.property(x),
        case false => Testable.property()
    }

pub def ==>(b: Bool, x: t): Property with Testable[t] =
    implies(b, x)

/// Labels a property with a given stamp
pub def label(s: String, x: t): Property with Testable[t] =
    let func = res -> {stamp = s :: res.stamp | res};
    Property.Property(
        map(func, evaluate(x))
    )

/// Conditionally labels a test c
pub def classify(b: Bool, name: String): t -> Property with Testable[t] =
    match b {
        case true => label(name),
        case false => Testable.property
    }
    
/// Stamps a property with the string "trivial" if true
pub def trivial(): Bool -> t -> Property with Testable[t] =
    b -> classify(b, "trivial")

/// Collects the distribution of a property, and labels it with the value of the argument
///
/// Example:
/// let prop = forAll(chooseInt(1, 10), n -> collect(n, n == 5));
/// flixCheck(prop)
pub def collect(element: a): t -> Property with ToString[a], Testable[t] =
    label(ToString.toString(element))