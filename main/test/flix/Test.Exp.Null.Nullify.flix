namespace Test/Exp/Null/Nullify {

    @test
    def testNullifyUnit01(): Unit? = ()?

    @test
    def testNullifyString01(): String? = "Hello World!"?

    @test
    def testNullifyRecord01(): {}? = {}?

    @test
    def testNullifyRecord02(): {name: String}? = {name = "Lucky"}?

    @test
    def testNullifyRecord03(): {name: String?}? = {name = "Lucky"?}?

    @test
    def testNullifyRecord04(): {fstName: String?, lstName: String?}? = {fstName = "Lucky"?, lstName = "Luke"?}?

    @test
    def testNullifyTuple01(): (String, String)? = ("Hello", "World")?

    @test
    def testNullifyTuple02(): (String?, String)? = ("Hello"?, "World")?

    @test
    def testNullifyTuple03(): (String, String?)? = ("Hello", "World"?)?

    @test
    def testNullifyTuple04(): (String?, String?)? = ("Hello"?, "World"?)?

    @test
    def testNullifyOption01(): Option[String]? = None?

    @test
    def testNullifyOption02(): Option[String]? = Some("Hello World")?

    @test
    def testNullifyOption03(): Option[String?] = None

    @test
    def testNullifyOption04(): Option[String?] = Some("Hello World"?)

    @test
    def testNullifyOption05(): Option[String?]? = None?

    @test
    def testNullifyOption06(): Option[String?]? = Some(null)?

    @test
    def testNullifyOption07(): Option[String?]? = Some("Hello World"?)?

    @test
    def testNullifyList01(): List[String]? = Nil?

    @test
    def testNullifyList02(): List[String]? = ("Hello World" :: Nil)?

    @test
    def testNullifyList03(): List[String?] = Nil

    @test
    def testNullifyList04(): List[String?] = "Hello World"? :: Nil

    @test
    def testNullifyList05(): List[String?]? = Nil?

    @test
    def testNullifyList06(): List[String?]? = ("Hello World"? :: Nil)?

    @test
    def testNullifyResult01(): Result[String, String]? = Ok("Hello World")?

    @test
    def testNullifyResult02(): Result[String, String]? = Err("Hello World")?

    @test
    def testNullifyResult03(): Result[String?, String] = Ok("Hello World"?)

    @test
    def testNullifyResult04(): Result[String, String?] = Err("Hello World"?)

    @test
    def testNullifyResult05(): Result[String?, String?] = Ok("Hello World"?)

    @test
    def testNullifyResult06(): Result[String?, String?] = Err("Hello World"?)

    @test
    def testNullifyResult07(): Result[String?, String?]? = Ok("Hello World"?)?

    @test
    def testNullifyResult08(): Result[String?, String?]? = Err("Hello World"?)?

    @test
    def testNullifyArrow01(): (String -> String)? = (_ -> "Hello World")?

    @test
    def testNullifyArrow02(): String -> String? = _ -> "Hello World"?

    @test
    def testNullifyArrow03(): String? -> String = _ -> "Hello World"

    @test
    def testNullifyArrow04(): (String? -> String?)? = (_ -> "Hello World"?)?

    @test
    def testNullifyArrow05(): (String ? true -> String) = _ -> "Hello World"

    @test
    def testNullifyArrow06(): (String -> String ? true) = _ -> "Hello World"?

    @test
    def testNullifyArrow07(): (String ? true -> String ? true) = _ -> "Hello World"?

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
