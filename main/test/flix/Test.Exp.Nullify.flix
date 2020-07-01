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



}
