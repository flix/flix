namespace Test/Exp/NullMatch {

    @test
    def testUseCase01(): Bool =
        let f = (x, y) -> {
            let s = match? x {
                case _ => y
                case w => w
            };
            toUpperCase(s)
        };
        f("hello"?, "world"?) == "HELLO"

    @test
    def testUseCase02(): Bool =
        let f = (x, y) -> {
            let s = match? x {
                case _ => y
                case w => w
            };
            toUpperCase(s)
        };
        f("hello"?, null) == "HELLO"

    @test
    def testUseCase03(): Bool =
        let f = (x, y) -> {
            let s = match? x {
                case _ => y
                case w => w
            };
            toUpperCase(s)
        };
        f(null, "world"?) == "HELLO"

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
        f({ssn = "123", name = null})

    @test
    def testUseCase08(): Bool =
        let f = p -> {
            match? (p.ssn, p.fstName, p.lstName) {
                case (ssn, _, _)           => isMaleSSN(ssn)
                case (_, fstName, lstName) => isMaleFullName(fstName, lstName)
            }
        };
        f({ssn = null, fstName = "Lucky"?, lstName = "Luke"?})

//
//    @test
//    def testUnitNullMatch01(): Bool =
//        match? () {
//          case null => false
//          case _    => true
//        }
//
//    @test
//    def testStringNullMatch01(): Bool =
//        match? "Hello World!" {
//          case null => false
//          case _    => true
//        }
//
//    @test
//    def testObjectNullMatch01(): Bool =
//        import new java.lang.Object() as newObj;
//        match? (newObj() as & Pure) {
//          case null => false
//          case _    => true
//        }
//
//    @test
//    def testRecordNullMatch01(): Bool =
//        match? {x = "hello", y = "world"} {
//          case null => false
//          case _    => true
//        }
//
//    @test
//    def testSchemaNullMatch01(): Bool =
//        match? #{A("Hello World").} {
//          case null => false
//          case _    => true
//        }
//
//    @test
//    def testOptionNullMatch01(): Bool =
//        match? None {
//          case null => false
//          case _    => true
//        }
//
//    @test
//    def testOptionNullMatch02(): Bool =
//        match? Some("Hello World") {
//          case null => false
//          case _    => true
//        }
//
//    @test
//    def testResultNullMatch01(): Bool =
//        match? Ok("Hello World") {
//          case null => false
//          case _    => true
//        }
//
//    @test
//    def testResultNullMatch02(): Bool =
//        match? Err("Hello World") {
//          case null => false
//          case _    => true
//        }
//
//    @test
//    def testListNullMatch01(): Bool =
//        match? Nil {
//          case null => false
//          case _    => true
//        }
//
//    @test
//    def testListNullMatch02(): Bool =
//        match? "Hello" :: "World" :: Nil {
//          case null => false
//          case _    => true
//        }

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

    pub def toUpperCase(s: NonNull[String]): String = match? s {
        case x =>
            import java.lang.String.toUpperCase();
            s.toUpperCase() as & Pure
    }

    def isMaleSSN(_: String): Bool = true

    def isMaleName(_: String): Bool = true

    def isMaleFullName(_: String, _: String): Bool = true

    def isMaleLicense(_: String): Bool = true

}
