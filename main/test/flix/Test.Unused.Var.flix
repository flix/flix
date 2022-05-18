namespace Test/Unused/Var {

    @test
    def testUnusedVar01(): Int32 =
        let _ = 123;
        456

    @test
    def testUnusedVar02(): Int32 =
        let _foo = 123;
        456

    @test
    def testUnusedVar03(): Int32 =
        let _foo = 123;
        let _bar = 456;
        789

    @test
    def testUnusedVar04(): Int32 =
        match 123 {
            case _foo => 456
        }

    @test
    def testUnusedVar05(): Int32 =
        match (1, 2) {
            case (x, _y) => x
        }

    @test
    def testUnusedVar06(): Int32 =
        match (1, 2) {
            case (_x, y) => y
        }

    @test
    def testUnusedVar07(): Int32 =
        match (1, 2) {
            case (_x, _y) => 123
        }

    @test
    def testUnusedVar08(): Int32 =
        match Some(123) {
            case None => 456
            case Some(_foo) => 789
        }

    @test
    def testUnusedVar09(): Int32 =
        match Ok(123) {
            case Ok(_foo) => 456
            case Err(_bar) => 789
        }

    @test
    def testUnusedVar10(): Int32 & Impure =
        let c = chan Int32 1;
        c <- 0;
        select {
            case _foo <- c => 123
        }

    @test
    def testUnusedVar11(): Int32 & Impure =
        let c1 = chan Int32 1;
        let c2 = chan Int32 1;
        c1 <- 0;
        c2 <- 0;
        select {
            case _foo <- c1 => 123
            case _bar <- c2 => 456
        }

    // NB: Compile, but not execute.
    pub def testUnusedVar12(): Int32 =
        let x = 123;
        ?hole

    // NB: Compile, but not execute.
    pub def testUnusedVar13(): Int32 =
        let x = 123;
        let y = 123;
        ?hole

    // NB: Compile, but not execute.
    pub def testUnusedVar14(): Int32 =
        match 123 {
            case foo => ?hole
        }

}
