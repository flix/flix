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

}
