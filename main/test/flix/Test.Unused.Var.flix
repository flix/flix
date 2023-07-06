mod Test.Unused.Var {

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
    def testUnusedVar10(): Int32 = region rc {
        let (tx, rx) = Channel.buffered(rc, 1);
        Channel.send(0, tx);
        select {
            case _foo <- recv(rx) => 123
        }
    }

    @test
    def testUnusedVar11(): Int32 = region rc {
        let (tx1, rx1) = Channel.buffered(rc, 1);
        let (tx2, rx2) = Channel.buffered(rc, 1);
        Channel.send(0, tx1);
        Channel.send(0, tx2);
        select {
            case _foo <- recv(rx1) => 123
            case _bar <- recv(rx2) => 456
        }
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

    @test
    def testUnusedVar15(): Int32 =
        let _⊆ = 123;
        456

    @test
    def testUnusedVar16(): Int32 =
        let _αναγνωριστικό = 123;
        456

    // Operator names not currently allowed for variables
    // @test
    // def testUnusedVar17(): Int32 =
    //     let _<*> = 123;
    //     456
}
