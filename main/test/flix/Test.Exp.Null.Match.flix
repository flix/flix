namespace Test/Exp/Null/Match {

    @test
    def testUseCase01(): Bool =
        let f = (x, y) -> {
            let s = choose (x, y) {
                case (Present(a), _) => a
                case (_, Present(b)) => b
            };
            toUpperCase(s)
        };
        f(Present("hello"), Present("world")) == "HELLO"

    @test
    def testUseCase02(): Bool =
        let f = (x, y) -> {
            let s = choose (x, y) {
                case (Present(a), _) => a
                case (_, Present(b)) => b
            };
            toUpperCase(s)
        };
        f(null, Present("world")) == "WORLD"

    @test
    def testUseCase03(): Bool =
        let f = (x, y) -> {
            let s = choose (x, y) {
                case (Present(a), _) => a
                case (_, Present(b)) => b
            };
            toUpperCase(s)
        };
        f(Present("hello"), null) == "HELLO"

    @test
    def testUseCase04(): Bool =
        let f = p -> {
            choose (p.ssn, p.name) {
                case (Present(ssn), _)  => isMaleSSN(ssn)
                case (_, Present(name)) => isMaleName(name)
            }
        };
        f({ssn = Present("123"), name = Present("Lucky Luke")})

    @test
    def testUseCase05(): Bool =
        let f = p -> {
            choose (p.ssn, p.name) {
                case (Present(ssn), _)  => isMaleSSN(ssn)
                case (_, Present(name)) => isMaleName(name)
            }
        };
        f({ssn = null, name = Present("Lucky Luke")})

    @test
    def testUseCase06(): Bool =
        let f = p -> {
            choose (p.ssn, p.name) {
                case (Present(ssn), _)  => isMaleSSN(ssn)
                case (_, Present(name)) => isMaleName(name)
            }
        };
        f({ssn = Present("123"), name = null})

    @test
    def testUseCase07(): Bool =
        let f = p -> {
            choose (p.ssn, p.name) {
                case (Present(ssn), Absent)         => isMaleSSN(ssn)
                case (Absent, Present(name))        => isMaleName(name)
                case (Present(ssn), Present(name))  => isMaleSSN(ssn)
            }
        };
        f({ssn = Present("123"), name = Present("Lucky Luke")})

    @test
    def testUseCase08(): Bool =
        let f = p -> {
            choose (p.ssn, p.name) {
                case (Present(ssn), Absent)         => isMaleSSN(ssn)
                case (Absent, Present(name))        => isMaleName(name)
                case (Present(ssn), Present(name))  => isMaleSSN(ssn)
            }
        };
        f({ssn = null, name = Present("Lucky Luke")})

    @test
    def testUseCase09(): Bool =
        let f = p -> {
            choose (p.ssn, p.name) {
                case (Present(ssn), Absent)         => isMaleSSN(ssn)
                case (Absent, Present(name))        => isMaleName(name)
                case (Present(ssn), Present(name))  => isMaleSSN(ssn)
            }
        };
        f({ssn = Present("123"), name = null})

    @test
    def testUseCase10(): Bool =
        let f = p -> {
            choose (p.x, p.y) {
                case (Present(x), Present(y))   => 1
                case (Absent, Present(y))       => 2
                case (Present(x), Absent)       => 3
            }
        };
        f({x = Present("a"), y = Present("b")}) == 1

    @test
    def testUseCase11(): Bool =
        let f = p -> {
            choose (p.x, p.y) {
                case (Present(x), Present(y))   => 1
                case (Absent, Present(y))       => 2
                case (Present(x), Absent)       => 3
            }
        };
        f({x = null, y = Present("b")}) == 2

    @test
    def testUseCase12(): Bool =
        let f = p -> {
            choose (p.x, p.y) {
                case (Present(x), Present(y))   => 1
                case (Absent, Present(y))       => 2
                case (Present(x), Absent)       => 3
            }
        };
        f({x = Present("a"), y = null}) == 3

    @test
    def testMatchNullableUnit01(): Bool =
        choose Present() {
            case Present(x) => true
            case _          => false
        }

    @test
    def testMatchNullableUnit02(): Bool =
        choose Absent {
            case Present(x) => false
            case _          => true
        }

    @test
    def testMatchNullableUnit03(): Bool =
        choose Present() {
            case Absent     => false
            case Present(x) => true
        }

    @test
    def testMatchNullableUnit04(): Bool =
        choose null {
            case Absent     => true
            case Present(x) => false
        }

    @test
    def testMatchNullableString01(): Bool =
        choose Present("Hello World") {
            case Present(x) => true
            case _          => false
        }

    @test
    def testMatchNullableString02(): Bool =
        choose Absent {
            case Present(x) => false
            case _          => true
        }

    @test
    def testMatchNullableString03(): Bool =
        choose Present("Hello World") {
            case Absent     => false
            case Present(x) => true
        }

    @test
    def testMatchNullableString04(): Bool =
        choose Absent {
            case Absent     => true
            case Present(x) => false
        }

    @test
    def testMatchNullableRecord01(): Bool =
        choose Present({}) {
            case Present(x) => true
            case _          => false
        }

    @test
    def testMatchNullableRecord02(): Bool =
        choose Present({name = "Lucky Luke"}) {
            case Present(x) => true
            case _          => false
        }

    @test
    def testMatchNullableRecord03(): Bool =
        choose Present({fstName = "Lucky", lstName = "Luke"}) {
            case Present(x) => true
            case _          => false
        }

    @test
    def testMatchNullableTuple01(): Bool =
        choose Present(123, 456) {
            case Present(x) => true
            case _          => false
        }

    @test
    def testMatchNullableOption01(): Bool =
        choose Present(None) {
            case Present(x) => true
            case _          => false
        }

    @test
    def testMatchNullableOption02(): Bool =
        choose Present(Some(123)) {
            case Present(x) => true
            case _          => false
        }


    @test
    def testMatchNullableList01(): Bool =
        choose Present(Nil) {
            case Present(x) => true
            case _          => false
        }

    @test
    def testMatchNullableList02(): Bool =
        choose Present(1 :: Nil) {
            case Present(x) => true
            case _          => false
        }

    @test
    def testMatchNullableResult01(): Bool =
        choose Present(Ok(123)) {
            case Present(x) => true
            case _          => false
        }

    @test
    def testMatchNullableResult02(): Bool =
        choose Present(Err(123)) {
            case Present(x) => true
            case _          => false
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
