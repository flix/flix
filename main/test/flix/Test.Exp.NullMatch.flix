namespace Test/Exp/NullMatch {

//    @test
//    def testUseCase01(): Bool =
//        let f = (x, y) -> {
//            let s = match? x {
//                case _ => y
//                case w => w
//            };
//            toUpperCase(s)
//        };
//        f("hello"?, "world"?) == "HELLO"
//
//    @test
//    def testUseCase02(): Bool =
//        let f = (x, y) -> {
//            let s = match? x {
//                case null => y
//                case w => w
//            };
//            toUpperCase(s)
//        };
//        f("hello", null) == "HELLO"
//
//    @test
//    def testUseCase03(): Bool =
//        let f = (x, y) -> {
//            let s = match? x {
//                case null => y
//                case w => w
//            };
//            toUpperCase(s)
//        };
//        f(null, "world") == "WORLD"
//
//    def toUpperCase(s: String): String =
//        import java.lang.String.toUpperCase();
//        s.toUpperCase() as & Pure
//
//    @test
//    def testUseCase04(): Bool =
//        let f = p -> {
//            match? p.ssn {
//                case null => isMaleName(p.name)
//                case ssn  => isMaleSSN(ssn)
//            }
//        };
//        f({ssn = "123", name = "Lucky Luke"})
//
//    @test
//    def testUseCase05(): Bool =
//        let f = p -> {
//            match? p.ssn {
//                case null => isMaleName(p.name)
//                case ssn  => isMaleSSN(ssn)
//            }
//        };
//        f({ssn = null, name = "Lucky Luke"})
//
//    @test
//    def testUseCase06(): Bool =
//        let f = p -> {
//            match? p.ssn {
//                case null => isMaleName(p.name)
//                case ssn  => isMaleSSN(ssn)
//            }
//        };
//        f({ssn = "123", name = null})
//
//    @test
//    def testUseCase07(): Bool =
//        let f = p -> {
//            match? p.ssn {
//                case null => isMaleFullName(p.fstName, p.lstName)
//                case ssn  => isMaleSSN(ssn)
//            }
//        };
//        f({ssn = "123", fstName = "Lucky", lstName = "Luke"})
//
//    @test
//    def testUseCase08(): Bool =
//        let f = p -> {
//            match? p.ssn {
//                case null => isMaleFullName(p.fstName, p.lstName)
//                case ssn  => isMaleSSN(ssn)
//            }
//        };
//        f({ssn = null, fstName = "Lucky", lstName = "Luke"})
//
//    @test
//    def testUseCase09(): Bool =
//        let f = p -> {
//            match? p.ssn {
//                case null => isMaleFullName(p.fstName, p.lstName)
//                case ssn  => isMaleSSN(ssn)
//            }
//        };
//        f({ssn = "123", fstName = null, lstName = null})
//
//    @test
//    def testUseCase10(): Bool =
//        let f = p -> {
//            match? p.fstName {
//                case null => true
//                case fstName => fstName == "Lucky" && p.lstName == "Luke"
//            }
//        };
//        f({fstName = "Lucky", lstName = "Luke"})
//
//    @test
//    def testUseCase11(): Bool =
//        let f = p -> {
//            match? p.fstName {
//                case null => true
//                case fstName => fstName == "Lucky" && p.lstName == "Luke"
//            }
//        };
//        f({fstName = null, lstName = "Luke"})
//
//    @test
//    def testUseCase12(): Bool =
//        let f = p -> {
//            match? p.fstName {
//                case null => true
//                case fstName => fstName == "Lucky" && p.lstName == "Luke"
//            }
//        };
//        f({fstName = null, lstName = null})
//
//    @test
//    def testUseCase13(): Bool =
//        let f = p -> {
//            match? p.ssn {
//                case null => match? p.license {
//                    case null    => isMaleName(p.name)
//                    case license => isMaleLicense(license)
//                }
//                case ssn => isMaleSSN(ssn)
//            }
//        };
//        f({ssn = null, name = null, license = "123"})
//
//    def isMaleSSN(_: String): Bool = true
//    def isMaleName(_: String): Bool = true
//    def isMaleFullName(_: String, _: String): Bool = true
//    def isMaleLicense(_: String): Bool = true
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

}
