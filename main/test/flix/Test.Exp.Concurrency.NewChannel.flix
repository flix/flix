namespace Test/Exp/Concurrency/NewChannel {

    @test
    def testNewChannel01(): (Sender[Unit, Static], Receiver[Unit, Static]) \ IO = Channel.unbuffered(Static)

    @test
    def testNewChannel02(): (Sender[Unit, Static], Receiver[Unit, Static]) \ IO = Channel.buffered(Static, 1)

    @test
    def testNewChannel03(): (Sender[Bool, Static], Receiver[Bool, Static]) \ IO = Channel.unbuffered(Static)

    @test
    def testNewChannel04(): (Sender[Bool, Static], Receiver[Bool, Static]) \ IO = Channel.buffered(Static, 1)

    @test
    def testNewChannel05(): (Sender[Char, Static], Receiver[Char, Static]) \ IO = Channel.unbuffered(Static)

    @test
    def testNewChannel06(): (Sender[Char, Static], Receiver[Char, Static]) \ IO = Channel.buffered(Static, 1)

    @test
    def testNewChannel07(): (Sender[Float32, Static], Receiver[Float32, Static]) \ IO = Channel.unbuffered(Static)

    @test
    def testNewChannel08(): (Sender[Float32, Static], Receiver[Float32, Static]) \ IO = Channel.buffered(Static, 1)

    @test
    def testNewChannel09(): (Sender[Float64, Static], Receiver[Float64, Static]) \ IO = Channel.unbuffered(Static)

    @test
    def testNewChannel10(): (Sender[Float64, Static], Receiver[Float64, Static]) \ IO = Channel.buffered(Static, 1)

    @test
    def testNewChannel11(): (Sender[Int8, Static], Receiver[Int8, Static]) \ IO = Channel.unbuffered(Static)

    @test
    def testNewChannel12(): (Sender[Int8, Static], Receiver[Int8, Static]) \ IO = Channel.buffered(Static, 1)

    @test
    def testNewChannel13(): (Sender[Int16, Static], Receiver[Int16, Static]) \ IO = Channel.unbuffered(Static)

    @test
    def testNewChannel14(): (Sender[Int16, Static], Receiver[Int16, Static]) \ IO = Channel.buffered(Static, 1)

    @test
    def testNewChannel15(): (Sender[Int32, Static], Receiver[Int32, Static]) \ IO = Channel.unbuffered(Static)

    @test
    def testNewChannel16(): (Sender[Int32, Static], Receiver[Int32, Static]) \ IO = Channel.buffered(Static, 1)

    @test
    def testNewChannel17(): (Sender[Int64, Static], Receiver[Int64, Static]) \ IO = Channel.unbuffered(Static)

    @test
    def testNewChannel18(): (Sender[Int64, Static], Receiver[Int64, Static]) \ IO = Channel.buffered(Static, 1)

    @test
    def testNewChannel19(): (Sender[BigInt, Static], Receiver[BigInt, Static]) \ IO = Channel.unbuffered(Static)

    @test
    def testNewChannel20(): (Sender[BigInt, Static], Receiver[BigInt, Static]) \ IO = Channel.buffered(Static, 1)

    @test
    def testNewChannel21(): (Sender[String, Static], Receiver[String, Static]) \ IO = Channel.unbuffered(Static)

    @test
    def testNewChannel22(): (Sender[String, Static], Receiver[String, Static]) \ IO = Channel.buffered(Static, 1)

    @test
    def testNewChannel23(): (Sender[Option[Int32], Static], Receiver[Option[Int32], Static]) \ IO = Channel.unbuffered(Static)

    @test
    def testNewChannel24(): (Sender[Option[Int32], Static], Receiver[Option[Int32], Static]) \ IO = Channel.buffered(Static, 1)

    @test
    def testNewChannel25(): (Sender[Result[Int32, String], Static], Receiver[Result[Int32, String], Static]) \ IO = Channel.unbuffered(Static)

    @test
    def testNewChannel26(): (Sender[Result[Int32, String], Static], Receiver[Result[Int32, String], Static]) \ IO = Channel.buffered(Static, 1)

    @test
    def testNewChannel27(): (Sender[(Sender[Int32, Static], Receiver[Int32, Static]), Static], Receiver[(Sender[Int32, Static], Receiver[Int32, Static]), Static]) \ IO = Channel.unbuffered(Static)

    @test
    def testNewChannel28(): (Sender[(Sender[Int32, Static], Receiver[Int32, Static]), Static], Receiver[(Sender[Int32, Static], Receiver[Int32, Static]), Static]) \ IO = Channel.buffered(Static, 1)

    @test
    def testNewChannel29(): (Sender[(Sender[String, Static], Receiver[String, Static]), Static], Receiver[(Sender[String, Static], Receiver[String, Static]), Static]) \ IO = Channel.unbuffered(Static)

    @test
    def testNewChannel30(): (Sender[(Sender[String, Static], Receiver[String, Static]), Static], Receiver[(Sender[String, Static], Receiver[String, Static]), Static]) \ IO = Channel.buffered(Static, 1)

}
