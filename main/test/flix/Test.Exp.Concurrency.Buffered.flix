namespace Test/Exp/Concurrency/Buffered {

    @test
    def testBufferedChannel01(): Bool @ Impure =
        let c = chan Unit 1;
        c <- ();
        () == <- c

    @test
    def testBufferedChannel02(): Bool @ Impure =
        let c = chan Bool 1;
        c <- true;
        true == <- c

}