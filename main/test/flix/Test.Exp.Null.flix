namespace Test/Exp/Null/Null {

    @test
    def testNull01(): Null = null

    @test
    def testNullUnit01(): Unit = null as Unit

    @test
    def testNullString01(): String = null as String

    @test
    def testNullArray01(): Array[String] = null as Array[String]

    @test
    def testNullChannel01(): Channel[String] = null as Channel[String]

    @test
    def testNullRecord01(): {} = null as {}

    @test
    def testNullRecord02(): {name: String} = null as {name: String}

    @test
    def testNullRecord03(): {name: String} = {name = null as String}

    @test
    def testNullRecord04(): {fstName: String, lstName: String} = null as {fstName: String, lstName: String}

    @test
    def testNullRecord05(): {fstName: String, lstName: String} = {fstName = null as String, lstName = null as String}

    @test
    def testNullRef01(): Ref[String] = null as Ref[String]

    @test
    def testNullTuple01(): (String, String) = null as (String, String)

    @test
    def testNullTuple02(): (String, String) = (null as String, "abc")

    @test
    def testNullTuple03(): (String, String) = ("abc", null as String)

    @test
    def testNullOption01(): Option[String] = null as Option[String]

//    @test
//    def testNullOption02(): Option[String?] = Some(null)
//
//    @test
//    def testNullOption03(): Option[String?]? = null
//
//    @test
//    def testNullList01(): List[String]? = null
//
//    @test
//    def testNullList02(): List[String?] = Nil
//
//    @test
//    def testNullList03(): List[String?]? = null
//
//    @test
//    def testNullList04(): List[String?] = null :: Nil
//
//    @test
//    def testNullList05(): List[String?] = null :: null :: Nil
//
//    @test
//    def testNullResult01(): Result[String, String]? = null
//
//    @test
//    def testNullResult02(): Result[String, String?]? = null
//
//    @test
//    def testNullResult03(): Result[String?, String]? = null
//
//    @test
//    def testNullResult04(): Result[String?, String?]? = null
//
//    @test
//    def testNullResult05(): Result[String?, String] = Ok(null)
//
//    @test
//    def testNullResult06(): Result[String, String?] = Err(null)
//
//    @test
//    def testNullArrow01(): (String -> String)? = null
//
//    @test
//    def testNullArrow02(): (String -> String?)? = null
//
//    @test
//    def testNullArrow03(): (String? -> String)? = null
//
//    @test
//    def testNullArrow04(): (String? -> String?)? = null
//
//    @test
//    def testNullArrow05(): ((String, String) -> String?)? = null
//
//    @test
//    def testNullArrow06(): ((String?, String) -> String?)? = null
//
//    @test
//    def testNullArrow07(): ((String, String?) -> String?)? = null
//
//    @test
//    def testNullArrow08(): ((String?, String?) -> String?)? = null
//
//    @test
//    def testNullArrow09(): (String -> String?) = _x -> null
//
//    @test
//    def testNullArrow10(): (String? -> String) = _x -> "abc"

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
