namespace Test/Exp/Concurrency/NewChannel {

    @test
    def testNewChannel01(): (Sender[Unit], Receiver[Unit]) \ IO = Channel.unbuffered()

    @test
    def testNewChannel02(): (Sender[Unit], Receiver[Unit]) \ IO = Channel.buffered(1)

    @test
    def testNewChannel03(): (Sender[Bool], Receiver[Bool]) \ IO = Channel.unbuffered()

    @test
    def testNewChannel04(): (Sender[Bool], Receiver[Bool]) \ IO = Channel.buffered(1)

    @test
    def testNewChannel05(): (Sender[Char], Receiver[Char]) \ IO = Channel.unbuffered()

    @test
    def testNewChannel06(): (Sender[Char], Receiver[Char]) \ IO = Channel.buffered(1)

    @test
    def testNewChannel07(): (Sender[Float32], Receiver[Float32]) \ IO = Channel.unbuffered()

    @test
    def testNewChannel08(): (Sender[Float32], Receiver[Float32]) \ IO = Channel.buffered(1)

    @test
    def testNewChannel09(): (Sender[Float64], Receiver[Float64]) \ IO = Channel.unbuffered()

    @test
    def testNewChannel10(): (Sender[Float64], Receiver[Float64]) \ IO = Channel.buffered(1)

    @test
    def testNewChannel11(): (Sender[Int8], Receiver[Int8]) \ IO = Channel.unbuffered()

    @test
    def testNewChannel12(): (Sender[Int8], Receiver[Int8]) \ IO = Channel.buffered(1)

    @test
    def testNewChannel13(): (Sender[Int16], Receiver[Int16]) \ IO = Channel.unbuffered()

    @test
    def testNewChannel14(): (Sender[Int16], Receiver[Int16]) \ IO = Channel.buffered(1)

    @test
    def testNewChannel15(): (Sender[Int32], Receiver[Int32]) \ IO = Channel.unbuffered()

    @test
    def testNewChannel16(): (Sender[Int32], Receiver[Int32]) \ IO = Channel.buffered(1)

    @test
    def testNewChannel17(): (Sender[Int64], Receiver[Int64]) \ IO = Channel.unbuffered()

    @test
    def testNewChannel18(): (Sender[Int64], Receiver[Int64]) \ IO = Channel.buffered(1)

    @test
    def testNewChannel19(): (Sender[BigInt], Receiver[BigInt]) \ IO = Channel.unbuffered()

    @test
    def testNewChannel20(): (Sender[BigInt], Receiver[BigInt]) \ IO = Channel.buffered(1)

    @test
    def testNewChannel21(): (Sender[String], Receiver[String]) \ IO = Channel.unbuffered()

    @test
    def testNewChannel22(): (Sender[String], Receiver[String]) \ IO = Channel.buffered(1)

    @test
    def testNewChannel23(): (Sender[Option[Int32]], Receiver[Option[Int32]]) \ IO = Channel.unbuffered()

    @test
    def testNewChannel24(): (Sender[Option[Int32]], Receiver[Option[Int32]]) \ IO = Channel.buffered(1)

    @test
    def testNewChannel25(): (Sender[Result[Int32, String]], Receiver[Result[Int32, String]]) \ IO = Channel.unbuffered()

    @test
    def testNewChannel26(): (Sender[Result[Int32, String]], Receiver[Result[Int32, String]]) \ IO = Channel.buffered(1)

    @test
    def testNewChannel27(): (Sender[(Sender[Int32], Receiver[Int32])], Receiver[(Sender[Int32], Receiver[Int32])]) \ IO = Channel.unbuffered()

    @test
    def testNewChannel28(): (Sender[(Sender[Int32], Receiver[Int32])], Receiver[(Sender[Int32], Receiver[Int32])]) \ IO = Channel.buffered(1)

    @test
    def testNewChannel29(): (Sender[(Sender[String], Receiver[String])], Receiver[(Sender[String], Receiver[String])]) \ IO = Channel.unbuffered()

    @test
    def testNewChannel30(): (Sender[(Sender[String], Receiver[String])], Receiver[(Sender[String], Receiver[String])]) \ IO = Channel.buffered(1)

}
