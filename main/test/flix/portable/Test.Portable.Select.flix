mod Test.Portable.Select {

    use Assert.{assertEq, assertTrue}

    @Test
    def basic01(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx1, rx1) = Channel.buffered(1);
        let (tx2, rx2) = Channel.buffered(1);
        spawn Channel.send(1, tx1) @ rc;
        spawn Channel.send(2, tx2) @ rc;

        let r = select {
            case x <- recv(rx1) => x
            case x <- recv(rx2) => x
        };

        assertTrue(r == 1 or r == 2)
    }

    @Test
    def default01(): Unit \ (Assert + Chan + NonDet) = {
        let r = select {
            case x <- recv({ let (_, rx) = Channel.buffered(1); rx }) => x
            case _ => 42
        };
        assertEq(expected = 42, r)
    }

    @Test
    def duplicate01(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx, rx) = Channel.buffered(1);
        spawn Channel.send(21, tx) @ rc;

        let r = select {
            case x <- recv(rx) => x
            case _ <- recv(rx) => 99
        };

        assertEq(expected = 21, r)
    }

    @Test
    def multiBuffered01(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx1, rx1) = Channel.buffered(1);
        let (tx2, rx2) = Channel.buffered(1);
        let (tx3, rx3) = Channel.buffered(1);
        spawn Channel.send(1, tx1) @ rc;
        spawn Channel.send(2, tx2) @ rc;
        spawn Channel.send(3, tx3) @ rc;

        let r = select {
            case x <- recv(rx1) => x == 1
            case x <- recv(rx2) => x == 2
            case x <- recv(rx3) => x == 3
        };

        assertTrue(r)
    }

    @Test
    def unbuffered01(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx1, rx1) = Channel.unbuffered();
        let (tx2, rx2) = Channel.unbuffered();
        spawn Channel.send(1, tx1) @ rc;
        spawn Channel.send(2, tx2) @ rc;

        let selected = select {
            case x <- recv(rx1) => x
            case x <- recv(rx2) => x
        };
        let other = if (selected == 1) Channel.recv(rx2) else Channel.recv(rx1);

        assertTrue((selected == 1 and other == 2) or (selected == 2 and other == 1))
    }

    @Test
    def defaultExpr01(): Unit \ (Assert + Chan + NonDet) = {
        let r = 1 + select {
            case _ <- recv({ let (_, rx) = Channel.buffered(1); rx }) => 2
            case _                                                     => 1
        };

        assertEq(expected = 2, r)
    }

    @Test
    def heterogeneous01(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx1, rx1) = Channel.buffered(1);
        let (tx2, rx2) = Channel.buffered(1);
        let (tx3, rx3) = Channel.buffered(1);
        let (tx4, rx4) = Channel.buffered(1);
        spawn Channel.send(1i8, tx1) @ rc;
        spawn Channel.send(2i16, tx2) @ rc;
        spawn Channel.send(3i32, tx3) @ rc;
        spawn Channel.send(4i64, tx4) @ rc;

        let r = select {
            case x <- recv(rx4) => x == 4i64
            case x <- recv(rx3) => x == 3i32
            case x <- recv(rx2) => x == 2i16
            case x <- recv(rx1) => x == 1i8
        };

        assertTrue(r)
    }

    @Test
    def blockedCancellation01(): Unit \ (Chan + NonDet + IO) = {
        let start = Clock.runWithIO(() -> Clock.now());
        let payload = try {
            region rc {
                let (_, rx1) = Channel.unbuffered();
                let (_, rx2) = Channel.unbuffered();
                spawn {
                    throw Exn.mk(9);
                    ()
                } @ rc;

                select {
                    case _ <- recv(rx1) => ()
                    case _ <- recv(rx2) => ()
                }
            };
            -1
        } catch {
            case exn: Int32 => Exn.payloadAs(exn)
            case _: Exn => -2
        };
        let elapsed = Clock.runWithIO(() -> Clock.now()) - start;
        let _ = if (payload == 9) () else bug!("blockedCancellation01 failed: wrong exception payload.");
        let _ = if (elapsed < 2000i64) () else bug!("blockedCancellation01 failed: region did not cancel blocked select promptly.");
        ()
    }

}
