namespace Test/Exp/Null {

    @test
    def testNull01(): Unit = null

    @test
    def testNull02(): String = null

    @test
    def testNull03(): {} = null

    @test
    def testNull04(): {x: Int, y: Int} = null

    @test
    def testNull05(): #{} = null

    @test
    def testNull06(): #{A(Int)} = null

    @test
    def testNull07(): Option[Int] = null

    @test
    def testNull08(): Result[Int, Int] = null

    @test
    def testNull09(): (Int, Int) = null

    @test
    def testNull10(): Channel[Int] = null

    @test
    def testNull11(): Ref[Int] = null

    @test
    def testNull12(): ##java.lang.Object = null

    pub enum Option[t] {
        case None,
        case Some(t)
    }

    pub enum Result[t, e] {
        case Ok(t),
        case Err(e)
    }

}
