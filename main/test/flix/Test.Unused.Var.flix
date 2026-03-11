mod Test.Unused.Var {

    use Assert.{fail, success}

    @Test
    def testUnusedVar01(): Unit \ Assert =
        let _ = 123;
        success("")

    @Test
    def testUnusedVar02(): Unit \ Assert =
        let _foo = 123;
        success("")

    @Test
    def testUnusedVar03(): Unit \ Assert =
        let _foo = 123;
        let _bar = 456;
        success("")

    @Test
    def testUnusedVar04(): Unit \ Assert =
        match 123 {
            case _foo => success("")
        }

    @Test
    def testUnusedVar05(): Unit \ Assert =
        match (1, 2) {
            case (_x, _y) => success("")
        }

    @Test
    def testUnusedVar06(): Unit \ Assert =
        match (1, 2) {
            case (_x, _y) => success("")
        }

    @Test
    def testUnusedVar07(): Unit \ Assert =
        match (1, 2) {
            case (_x, _y) => success("")
        }

    @Test
    def testUnusedVar08(): Unit \ Assert =
        match Some(123) {
            case None       => fail("unreachable")
            case Some(_foo) => success("")
        }

    @Test
    def testUnusedVar09(): Unit \ Assert =
        match Ok(123) {
            case Ok(_foo)  => success("")
            case Err(_bar) => fail("unreachable")
        }

    @Test
    def testUnusedVar10(): Unit \ Assert + Chan + NonDet = {
        let (tx, rx) = Channel.buffered(1);
        Channel.send(0, tx);
        select {
            case _foo <- recv(rx) => success("")
        }
    }

    @Test
    def testUnusedVar11(): Unit \ Assert + Chan + NonDet = {
        let (tx1, rx1) = Channel.buffered(1);
        let (tx2, rx2) = Channel.buffered(1);
        Channel.send(0, tx1);
        Channel.send(0, tx2);
        select {
            case _foo <- recv(rx1) => success("")
            case _bar <- recv(rx2) => success("")
        }
    }

    @CompileTest
    def testUnusedVar12(): Int32 =
        let x = 123;
        ?hole

    @CompileTest
    def testUnusedVar13(): Int32 =
        let x = 123;
        let y = 123;
        ?hole

    @CompileTest
    def testUnusedVar14(): Int32 =
        match 123 {
            case foo => ?hole
        }

    @Test
    def testUnusedVar15(): Unit \ Assert =
        let _⊆ = 123;
        success("")

}
