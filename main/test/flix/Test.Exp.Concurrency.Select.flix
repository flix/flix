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

    @test
    def testSelectBuffered06(): Bool @ Impure =
        let c1 = chan Int 1;
        let c2 = chan Int 1;
        let c3 = chan Int 1;
        let c4 = chan Int 1;
        spawn c2 <- 1;
        select {
            case _ <- c4 => false
            case _ <- c3 => false
            case x <- c2 => x == 1
            case _ <- c1 => false
        }

    @test
    def testSelectBuffered07(): Bool @ Impure =
        let c1 = chan Int8 1;
        let c2 = chan Int16 1;
        let c3 = chan Int32 1;
        let c4 = chan Int64 1;
        spawn c1 <- 1i8;
        spawn c2 <- 2i16;
        spawn c3 <- 3i32;
        spawn c4 <- 4i64;
        select {
            case x <- c4 => x == 4i64
            case x <- c3 => x == 3i32
            case x <- c2 => x == 2i16
            case x <- c1 => x == 1i8
        }

}
