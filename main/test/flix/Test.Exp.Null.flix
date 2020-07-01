namespace Test/Exp/Null {

    @test
    def testNullableUnit01(): Unit? = null

    @test
    def testNullableString01(): String? = null

    @test
    def testNullableArray01(): Array[String]? = null

    @test
    def testNullableArray02(): Array[String?]? = null

    @test
    def testNullableArray03(): Array[String?] & Impure = [null; 10]

    @test
    def testNullableArray04(): Array[String?] & Impure = [null, null, null]

    @test
    def testNullableChannel01(): Channel[String]? = null

    @test
    def testNullableChannel02(): Channel[String?]? = null

    @test
    def testNullableRecord01(): {}? = null

    @test
    def testNullableRecord02(): {name: String}? = null

    @test
    def testNullableRecord03(): {name: String?} = {name = null}

    @test
    def testNullableRecord04(): {fstName: String, lstName: String}? = null

    @test
    def testNullableRecord05(): {fstName: String?, lstName: String?} = {fstName = null, lstName = null}

    @test
    def testNullableRecord06(): {fstName: String, lstName: String?} = {fstName = "Lucky", lstName = null}

    @test
    def testNullableRecord07(): {fstName: String?, lstName: String} = {fstName = null, lstName = "Luke"}

    @test
    def testNullableRef01(): Ref[String]? = null

    @test
    def testNullableRef02(): Ref[String?]? = null

    @test
    def testNullableRef03(): Ref[String?] & Impure = ref null

    @test
    def testNullableTuple01(): (String, String)? = null

    @test
    def testNullableTuple02(): (String?, String)? = null

    @test
    def testNullableTuple03(): (String, String?)? = null

    @test
    def testNullableTuple04(): (String?, String?)? = null

    @test
    def testNullableTuple05(): (String, String?) = ("Lucky", null)

    @test
    def testNullableTuple06(): (String?, String) = (null, "Luke")

    @test
    def testNullableTuple07(): (String?, String?) = (null, null)

    @test
    def testNullableOption01(): Option[String]? = null

    @test
    def testNullableOption02(): Option[String?] = Some(null)

    @test
    def testNullableOption03(): Option[String?]? = null

    @test
    def testNullableList01(): List[String]? = null

    @test
    def testNullableList02(): List[String?] = Nil

    @test
    def testNullableList03(): List[String?]? = null

    @test
    def testNullableList04(): List[String?] = null :: Nil

    @test
    def testNullableList05(): List[String?] = null :: null :: Nil

    @test
    def testNullableResult01(): Result[String, String]? = null

    @test
    def testNullableResult02(): Result[String, String?]? = null

    @test
    def testNullableResult03(): Result[String?, String]? = null

    @test
    def testNullableResult04(): Result[String?, String?]? = null

    @test
    def testNullableResult05(): Result[String?, String] = Ok(null)

    @test
    def testNullableResult06(): Result[String, String?] = Err(null)

    @test
    def testNullableArrow01(): (String -> String)? = null

    @test
    def testNullableArrow02(): (String -> String?)? = null

    @test
    def testNullableArrow03(): (String? -> String)? = null

    @test
    def testNullableArrow04(): (String? -> String?)? = null

    @test
    def testNullableArrow05(): ((String, String) -> String?)? = null

    @test
    def testNullableArrow06(): ((String?, String) -> String?)? = null

    @test
    def testNullableArrow07(): ((String, String?) -> String?)? = null

    @test
    def testNullableArrow08(): ((String?, String?) -> String?)? = null

    @test
    def testNullableArrow09(): (String -> String?) = _x -> null

    @test
    def testNullableArrow10(): (String? -> String) = _x -> "abc"

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
