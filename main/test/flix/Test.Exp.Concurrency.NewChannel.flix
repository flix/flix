namespace Test/Exp/Concurrency/NewChannel {

    @test
    def testNewChannel01(): Channel[Unit] = chan Unit 0

    @test
    def testNewChannel02(): Channel[Unit] = chan Unit 1

}
