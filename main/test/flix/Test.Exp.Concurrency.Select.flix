namespace Test/Exp/Concurrency/Select {

    @test
    def testSelectBuffered01(): Bool @ Impure =
        let c1 = chan Int 1;
        spawn c1 <- 1;
        select {
            case x <- c1 => x == 1
        }

    @test
    def testSelectBuffered02(): Bool @ Impure =
        let c1 = chan Int 1;
        let c2 = chan Int 1;
        spawn c1 <- 1;
        spawn c2 <- 2;
        select {
            case x <- c1 => x == 1
            case x <- c2 => x == 2
        }

    @test
    def testSelectBuffered03(): Bool @ Impure =
        let c1 = chan Int 1;
        let c2 = chan Int 1;
        let c3 = chan Int 1;
        spawn c1 <- 1;
        spawn c2 <- 2;
        spawn c3 <- 3;
        select {
            case x <- c1 => x == 1
            case x <- c2 => x == 2
            case x <- c3 => x == 3
        }

    @test
    def testSelectBuffered04(): Bool @ Impure =
        let c1 = chan Int 1;
        let c2 = chan Int 1;
        let c3 = chan Int 1;
        let c4 = chan Int 1;
        spawn c1 <- 1;
        spawn c2 <- 2;
        spawn c3 <- 3;
        spawn c4 <- 4;
        select {
            case x <- c1 => x == 1
            case x <- c1 => x == 1
            case x <- c2 => x == 2
            case x <- c2 => x == 2
            case x <- c3 => x == 3
            case x <- c3 => x == 3
            case x <- c4 => x == 4
            case x <- c4 => x == 4
        }

    @test
    def testSelectBuffered05(): Bool @ Impure =
        let c1 = chan Int 1;
        let c2 = chan Int 1;
        let c3 = chan Int 1;
        let c4 = chan Int 1;
        spawn c1 <- 1;
        spawn c2 <- 2;
        spawn c3 <- 3;
        spawn c4 <- 4;
        select {
            case x <- c4 => x == 4
            case x <- c3 => x == 3
            case x <- c2 => x == 2
            case x <- c1 => x == 1
            case x <- c4 => x == 4
            case x <- c3 => x == 3
            case x <- c2 => x == 2
            case x <- c1 => x == 1
        }

}