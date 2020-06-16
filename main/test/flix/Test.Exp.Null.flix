namespace Test/Exp/Null {

    @test
    def testUseCase01(): Bool =
        let f = (x, y) -> {
            let s = match? x {
                case null => y
                case w => w
            };
            toUpperCase(s)
        };
        f("hello", "world") == "HELLO"

    @test
    def testUseCase02(): Bool =
        let f = (x, y) -> {
            let s = match? x {
                case null => y
                case w => w
            };
            toUpperCase(s)
        };
        f("hello", null) == "HELLO"

    @test
    def testUseCase03(): Bool =
        let f = (x, y) -> {
            let s = match? x {
                case null => y
                case w => w
            };
            toUpperCase(s)
        };
        f(null, "world") == "WORLD"

    def toUpperCase(s: String): String =
        import java.lang.String.toUpperCase();
        s.toUpperCase() as & Pure

    @test
    def testUseCase04(): Bool =
        let f = p -> {
            match? p.ssn {
                case null => isMaleName(p.name)
                case ssn  => isMaleSSN(ssn)
            }
        };
        f({ssn = "123", name = "Lucky Luke"})

    @test
    def testUseCase05(): Bool =
        let f = p -> {
            match? p.ssn {
                case null => isMaleName(p.name)
                case ssn  => isMaleSSN(ssn)
            }
        };
        f({ssn = null, name = "Lucky Luke"})

    @test
    def testUseCase06(): Bool =
        let f = p -> {
            match? p.ssn {
                case null => isMaleName(p.name)
                case ssn  => isMaleSSN(ssn)
            }
        };
        f({ssn = "123", name = null})

    @test
    def testUseCase07(): Bool =
        let f = p -> {
            match? p.ssn {
                case null => isMaleFullName(p.fstName, p.lstName)
                case ssn  => isMaleSSN(ssn)
            }
        };
        f({ssn = "123", fstName = "Lucky", lstName = "Luke"})

    @test
    def testUseCase08(): Bool =
        let f = p -> {
            match? p.ssn {
                case null => isMaleFullName(p.fstName, p.lstName)
                case ssn  => isMaleSSN(ssn)
            }
        };
        f({ssn = null, fstName = "Lucky", lstName = "Luke"})

    @test
    def testUseCase09(): Bool =
        let f = p -> {
            match? p.ssn {
                case null => isMaleFullName(p.fstName, p.lstName)
                case ssn  => isMaleSSN(ssn)
            }
        };
        f({ssn = "123", fstName = null, lstName = null})

    def isMaleSSN(_: String): Bool = true
    def isMaleName(_: String): Bool = true
    def isMaleFullName(_: String, _: String): Bool = true

    @test
    def testUnitNull01(): Unit = null

    @test
    def testStringNull01(): String = null

    @test
    def testRecordNull01(): {} = null

    @test
    def testObjectNull01(): ##java.lang.Object = null

    @test
    def testRecordNull02(): {x: Int} = null

    @test
    def testRecordNull03(): {x: Int, y: Int} = null

    @test
    def testRecordNull04(): {x: String, y: String} = { x = "hello", y = null}

    @test
    def testRecordNull05(): {x: String, y: String} = { x = null, y = "world"}

    @test
    def testSchemaNull01(): #{} = null

    @test
    def testSchemaNull02(): #{A(Int)} = null

    @test
    def testSchemaNull03(): #{A(Int), B(Int)} = null

    @test
    def testSchemaNull04(): #{A(String), B(String)} = #{A(null).}

    @test
    def testSchemaNull05(): #{A(String), B(String)} = #{B(null).}

    @test
    def testOptionNull01(): Option[String] = null

    @test
    def testOptionNull02(): Option[String] = Some(null)

    @test
    def testResultNull01(): Result[String, String] = null

    @test
    def testResultNull02(): Result[String, String] = Ok(null)

    @test
    def testResultNull03(): Result[String, String] = Err(null)

    @test
    def testListNull01(): List[String] = null

    @test
    def testListNull02(): List[String] = null :: Nil

    @test
    def testListNull03(): List[String] = null :: null :: Nil

    @test
    def testTupleNull01(): (String, String) = null

    @test
    def testTupleNull02(): (String, String) = ("hello", null)

    @test
    def testTupleNull03(): (String, String) = (null, "world")

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

}
