namespace Test/Exp/Concurrency/NewChannel {

    @test
    def testNewChannel01(): Channel[Unit] @ Impure = chan Unit 0

    @test
    def testNewChannel02(): Channel[Unit] @ Impure = chan Unit 1

    @test
    def testNewChannel03(): Channel[Bool] @ Impure = chan Bool 0

    @test
    def testNewChannel04(): Channel[Bool] @ Impure = chan Bool 1


}
