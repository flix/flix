namespace Test/Exp/Concurrency/NewChannel {

    @test
    def testNewChannel01(): Channel[Unit] & Impure = chan Unit 0

    @test
    def testNewChannel02(): Channel[Unit] & Impure = chan Unit 1

    @test
    def testNewChannel03(): Channel[Bool] & Impure = chan Bool 0

    @test
    def testNewChannel04(): Channel[Bool] & Impure = chan Bool 1

    @test
    def testNewChannel05(): Channel[Char] & Impure = chan Char 0

    @test
    def testNewChannel06(): Channel[Char] & Impure = chan Char 1

    @test
    def testNewChannel07(): Channel[Float32] & Impure = chan Float32 0

    @test
    def testNewChannel08(): Channel[Float32] & Impure = chan Float32 1

    @test
    def testNewChannel09(): Channel[Float64] & Impure = chan Float64 0

    @test
    def testNewChannel10(): Channel[Float64] & Impure = chan Float64 1

    @test
    def testNewChannel11(): Channel[Int8] & Impure = chan Int8 0

    @test
    def testNewChannel12(): Channel[Int8] & Impure = chan Int8 1

    @test
    def testNewChannel13(): Channel[Int16] & Impure = chan Int16 0

    @test
    def testNewChannel14(): Channel[Int16] & Impure = chan Int16 1

    @test
    def testNewChannel15(): Channel[Int32] & Impure = chan Int32 0

    @test
    def testNewChannel16(): Channel[Int32] & Impure = chan Int32 1

    @test
    def testNewChannel17(): Channel[Int64] & Impure = chan Int64 0

    @test
    def testNewChannel18(): Channel[Int64] & Impure = chan Int64 1

    @test
    def testNewChannel19(): Channel[BigInt] & Impure = chan BigInt 0

    @test
    def testNewChannel20(): Channel[BigInt] & Impure = chan BigInt 1

    @test
    def testNewChannel21(): Channel[String] & Impure = chan String 0

    @test
    def testNewChannel22(): Channel[String] & Impure = chan String 1

    @test
    def testNewChannel23(): Channel[Option[Int32]] & Impure = chan Option[Int32] 0

    @test
    def testNewChannel24(): Channel[Option[Int32]] & Impure = chan Option[Int32] 1

    @test
    def testNewChannel25(): Channel[Result[Int32, String]] & Impure = chan Result[Int32, String] 0

    @test
    def testNewChannel26(): Channel[Result[Int32, String]] & Impure = chan Result[Int32, String] 1

    @test
    def testNewChannel27(): Channel[Channel[Int32]] & Impure = chan Channel[Int32] 0

    @test
    def testNewChannel28(): Channel[Channel[Int32]] & Impure = chan Channel[Int32] 1

    @test
    def testNewChannel29(): Channel[Channel[String]] & Impure = chan Channel[String] 0

    @test
    def testNewChannel30(): Channel[Channel[String]] & Impure = chan Channel[String] 1

}
