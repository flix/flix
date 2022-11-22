namespace Test/Exp/Null {

    @test
    def testNull01(): Null = null

    @test
    def testNullUnit01(): Unit = unsafe_cast null: Unit

    @test
    def testNullString01(): String = unsafe_cast null: String

    @test
    def testNullArray01(): Array[String, Static] = unsafe_cast null: Array[String, Static]

    @test
    def testNullChannel01(): Receiver[String, Static] = unsafe_cast null: Receiver[String, Static]

    @test
    def testNullRecord01(): {} = unsafe_cast null: {}

    @test
    def testNullRecord02(): {name = String} = unsafe_cast null: {name = String}

    @test
    def testNullRecord03(): {name = String} = {name = unsafe_cast null: String}

    @test
    def testNullRecord04(): {fstName = String, lstName = String} = unsafe_cast null: {fstName = String, lstName = String}

    @test
    def testNullRecord05(): {fstName = String, lstName = String} = {fstName = unsafe_cast null: String, lstName = unsafe_cast null: String}

    @test
    def testNullRef01(): Ref[String, Static] = unsafe_cast null: Ref[String, Static]

    @test
    def testNullTuple01(): (String, String) = unsafe_cast null: (String, String)

    @test
    def testNullTuple02(): (String, String) = (unsafe_cast null: String, "abc")

    @test
    def testNullTuple03(): (String, String) = ("abc", unsafe_cast null: String)

    @test
    def testNullOption01(): Option[String] = unsafe_cast null: Option[String]

    @test
    def testNullOption02(): Option[String] = Some(unsafe_cast null: String)

    @test
    def testNullList01(): List[String] = unsafe_cast null: List[String]

    @test
    def testNullList02(): List[String] = (unsafe_cast null: String) :: Nil

    @test
    def testNullResult01(): Result[String, String] = unsafe_cast null: Result[String, String]

    @test
    def testNullResult03(): Result[String, String] = Ok(unsafe_cast null: String)

    @test
    def testNullResult04(): Result[String, String] = Err(unsafe_cast null: String)

    @test
    def testNullArrow01(): (String -> String) = (_: String) -> unsafe_cast null: String

}
