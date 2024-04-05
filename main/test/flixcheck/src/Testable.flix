mod Testable {
    use Monadic2.{Gen, Arbitrary, return}
    use Functor.{map}

    type alias Result = {
        ok = Option[Bool], 
        stamp = List[String], 
        arguments = List[String]
    }

    pub def nothing(): Result =
        {ok = None, stamp = Nil, arguments = Nil}

    pub enum Property(Gen[Result])

    pub def result(r: Result): Property =
        Property.Property(return(r))

    pub def evaluate(x: a): Gen[Result] with Testable[a] = 
        let Property.Property(gen) = Testable.property(x);
        gen

    pub trait Testable[a] {
        pub def property(x: a): Property
    }

    instance Testable[Unit] {
        pub def property(_: Unit): Property =
            result(nothing())
    }

    instance Testable[Bool] {
        pub def property(b: Bool): Property =
            result({ok = Some(b), stamp = Nil, arguments = Nil})
    }

    // instance Testable[Result] {
    //     pub def property(r: Result): Property =
    //         ???
    // }

    instance Testable[Property] {
        pub def property(prop: Property): Property =
            prop
    }

    // instance Testable[a -> b] with Arbitrary[a], ToString[a], Testable[b] {
    //     pub def property(f: a -> b): Property =
    //         ???
    // }

    pub def forAll(aGen: Gen[a], f: a -> b): Property with ToString[a], Testable[b] =
        Property.Property(
            forM (
                a <- aGen;
                res <- evaluate(f(a))
            ) yield (
                addArgument(a, res)
            )
        )
        
    pub def addArgument(arg: a, res: Result): Result with ToString[a] =
        let aString = ToString.toString(arg);
        {arguments = aString :: res.arguments | res}

    pub def ==>(b: Bool, x: a): Property with Testable[a] =
        match b {
            case true => Testable.property(x),
            case false => Testable.property()
        }

    pub def label(s: String, x: a): Property with Testable[a] =
        let func = res -> {stamp = s :: res.stamp | res};
        Property.Property(
            map(func, evaluate(x))
        )

    pub def classify(b: Bool, name: String): a -> Property with Testable[a] =
        match b {
            case true => label(name),
            case false => Testable.property
        }

    pub def trivial(): Bool -> a -> Property with Testable[a] =
        bool -> classify(bool, "trivial")

    pub def collect(x: a): b -> Property with ToString[a], Testable[b] =
        label(ToString.toString(x))
}