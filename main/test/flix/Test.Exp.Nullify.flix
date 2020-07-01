namespace Test/Exp/Nullify {

    @test
    def testNullifyUnit01(): Unit? = ()?

    @test
    def testNullifyString01(): String? = "Hello World!"?

    @test
    def testNullifyArray01(): Array[String]? & Impure = []?

    @test
    def testNullifyArray02(): Array[String?]? & Impure = ["Hello"?, "World"?]?

    @test
    def testNullifyRecord01(): {}? = {}?

    @test
    def testNullifyRecord02(): {name: String}? = {name = "Lucky"}?

    @test
    def testNullifyRecord03(): {name: String?}? = {name = "Lucky"?}?

    @test
    def testNullifyRecord04(): {fstName: String?, lstName: String?}? = {fstName = "Lucky"?, lstName = "Luke"?}?

    @test
    def testNullifyRef01(): Ref[String]? & Impure = (ref "Hello World")?

    @test
    def testNullifyRef02(): Ref[String?] & Impure = ref "Hello World"?

    @test
    def testNullifyRef03(): Ref[String?]? & Impure = (ref "Hello World"?)?

    @test
    def testNullifyTuple01(): (String, String)? = ("Hello", "World")?

    @test
    def testNullifyTuple02(): (String?, String)? = ("Hello"?, "World")?

    @test
    def testNullifyTuple03(): (String, String?)? = ("Hello", "World"?)?

    @test
    def testNullifyTuple03(): (String?, String?)? = ("Hello"?, "World"?)?

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
    def testNullifyList01(): Result[String, String]? = Ok("Hello World")?

    @test
    def testNullifyList02(): Result[String, String]? = Err("Hello World")?

    @test
    def testNullifyList03(): Result[String?, String] = Ok("Hello World"?)

    @test
    def testNullifyList04(): Result[String, String?] = Err("Hello World"?)

    @test
    def testNullifyList05(): Result[String?, String?] = Ok("Hello World"?)

    @test
    def testNullifyList06(): Result[String?, String?] = Err("Hello World"?)

    @test
    def testNullifyList07(): Result[String?, String?]? = Ok("Hello World"?)?

    @test
    def testNullifyList08(): Result[String?, String?]? = Err("Hello World"?)?


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
