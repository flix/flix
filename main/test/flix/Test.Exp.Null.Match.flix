namespace Test/Exp/Null/Match {

    @test
    def testUseCase01(): Bool =
        let f = (x, y) -> {
            let s = match? (x, y) {
                case (a, _) => a
                case (_, b) => b
            };
            toUpperCase(s)
        };
        f("hello"?, "world"?) == "HELLO"

    @test
    def testUseCase02(): Bool =
        let f = (x, y) -> {
            let s = match? (x, y) {
                case (a, _) => a
                case (_, b) => b
            };
            toUpperCase(s)
        };
        f(null, "world"?) == "WORLD"

    @test
    def testUseCase03(): Bool =
        let f = (x, y) -> {
            let s = match? (x, y) {
                case (a, _) => a
                case (_, b) => b
            };
            toUpperCase(s)
        };
        f("hello"?, null) == "HELLO"

    @test
    def testUseCase04(): Bool =
        let f = p -> {
            match? (p.ssn, p.name) {
                case (ssn, _)  => isMaleSSN(ssn)
                case (_, name) => isMaleName(name)
            }
        };
        f({ssn = "123"?, name = "Lucky Luke"?})

    @test
    def testUseCase05(): Bool =
        let f = p -> {
            match? (p.ssn, p.name) {
                case (ssn, _)  => isMaleSSN(ssn)
                case (_, name) => isMaleName(name)
            }
        };
        f({ssn = null, name = "Lucky Luke"?})

    @test
    def testUseCase06(): Bool =
        let f = p -> {
            match? (p.ssn, p.name) {
                case (ssn, _)  => isMaleSSN(ssn)
                case (_, name) => isMaleName(name)
            }
        };
        f({ssn = "123"?, name = null})

    @test
    def testMatchNullableUnit01(): Bool =
        match? ()? {
            case x => true
            case _ => false
        }

    @test
    def testMatchNullableUnit02(): Bool =
        match? null {
            case x => false
            case _ => true
        }

    @test
    def testMatchNullableUnit03(): Bool =
        match? ()? {
            case null => false
            case x    => true
        }

    @test
    def testMatchNullableUnit04(): Bool =
        match? null {
            case null => true
            case x    => false
        }

    @test
    def testMatchNullableString01(): Bool =
        match? "Hello World"? {
            case x => true
            case _ => false
        }

    @test
    def testMatchNullableRecord01(): Bool =
        match? {}? {
            case x => true
            case _ => false
        }

    @test
    def testMatchNullableRecord02(): Bool =
        match? {name = "Lucky Luke"}? {
            case x => true
            case _ => false
        }

    @test
    def testMatchNullableRecord03(): Bool =
        match? {fstName = "Lucky", lstName = "Luke"}? {
            case x => true
            case _ => false
        }

    @test
    def testMatchNullableTuple01(): Bool =
        let v = (123, 456);
        match? v? {
            case x => true
            case _ => false
        }

    @test
    def testMatchNullableOption01(): Bool =
        match? None? {
            case x => true
            case _ => false
        }

    @test
    def testMatchNullableOption02(): Bool =
        match? Some(123)? {
            case x => true
            case _ => false
        }


    @test
    def testMatchNullableList01(): Bool =
        match? Nil? {
            case x => true
            case _ => false
        }

    @test
    def testMatchNullableList02(): Bool =
        let v = (1 :: Nil);
        match? v? {
            case x => true
            case _ => false
        }

    @test
    def testMatchNullableResult01(): Bool =
        match? Ok(123)? {
            case x => true
            case _ => false
        }

    @test
    def testMatchNullableResult02(): Bool =
        match? Err(123)? {
            case x => true
            case _ => false
        }

    pub enum Option[t] {
        case None,
        case Some(t)
    }

    pub enum Result[t, e] {
        case Ok(t),
        case Err(e)
    }

    pub enum List[t] {
        case Nil,
        case Cons(t, List[t])
    }

    pub def toUpperCase(s: String): String =
        import java.lang.String.toUpperCase();
        s.toUpperCase() as & Pure

    pub def isMaleSSN(_: String): Bool = true

    pub def isMaleName(_: String): Bool = true

    pub def isMaleFullName(_: String, _: String): Bool = true

    pub def isMaleLicense(_: String): Bool = true

}
