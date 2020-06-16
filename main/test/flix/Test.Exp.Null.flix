namespace Test/Exp/Null {

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
    def testListNull09(): List[String] = null

    @test
    def testNull09(): (Int, Int) = null

    @test
    def testNull10(): Channel[Int] = null

    @test
    def testNull11(): Ref[Int] = null


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
