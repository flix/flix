namespace Test/Exp/Concurrency/NewChannel {

    @test
    def testNewChannel01(): Bool \ IO = region r {
        let (tx, rx) = Channel.unbuffered(r);
        spawn Channel.send((), tx) @ r;
        Channel.recv(rx) == ()
    }

    @test
    def testNewChannel02(): Bool = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        Channel.send((), tx);
        Channel.recv(rx) == ()
    }

    @test
    def testNewChannel03(): Bool \ IO = region r {
        let (tx, rx) = Channel.unbuffered(r);
        spawn Channel.send(true, tx) @ r;
        Channel.recv(rx)
    }

    @test
    def testNewChannel04(): Bool = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        Channel.send(true, tx);
        Channel.recv(rx)
    }

    @test
    def testNewChannel05(): Bool \ IO = region r {
        let (tx, rx) = Channel.unbuffered(r);
        spawn Channel.send('a', tx) @ r;
        Channel.recv(rx) == 'a'
    }

    @test
    def testNewChannel06(): Bool = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        Channel.send('a', tx);
        Channel.recv(rx) == 'a'
    }

    @test
    def testNewChannel07(): Bool \ IO = region r {
        let (tx, rx) = Channel.unbuffered(r);
        spawn Channel.send(1.23, tx) @ r;
        Channel.recv(rx) == 1.23
    }

    @test
    def testNewChanne08(): Bool = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        Channel.send(1.23, tx);
        Channel.recv(rx) == 1.23
    }

    @test
    def testNewChannel09(): Bool \ IO = region r {
        let (tx, rx) = Channel.unbuffered(r);
        spawn Channel.send(1.23f64, tx) @ r;
        Channel.recv(rx) == 1.23f64
    }

    @test
    def testNewChannel10(): Bool = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        Channel.send(1.23f64, tx);
        Channel.recv(rx) == 1.23f64
    }

    @test
    def testNewChannel11(): Bool \ IO = region r {
        let (tx, rx) = Channel.unbuffered(r);
        spawn Channel.send(1i8, tx) @ r;
        Channel.recv(rx) == 1i8
    }

    @test
    def testNewChannel12(): Bool = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        Channel.send(1i8, tx);
        Channel.recv(rx) == 1i8
    }

    @test
    def testNewChannel13(): Bool \ IO = region r {
        let (tx, rx) = Channel.unbuffered(r);
        spawn Channel.send(1i16, tx) @ r;
        Channel.recv(rx) == 1i16
    }

    @test
    def testNewChannel14(): Bool = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        Channel.send(1i16, tx);
        Channel.recv(rx) == 1i16
    }

    @test
    def testNewChannel15(): Bool \ IO = region r {
        let (tx, rx) = Channel.unbuffered(r);
        spawn Channel.send(1, tx) @ r;
        Channel.recv(rx) == 1
    }

    @test
    def testNewChannel16(): Bool = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        Channel.send(1, tx);
        Channel.recv(rx) == 1
    }

    @test
    def testNewChannel17(): Bool \ IO = region r {
        let (tx, rx) = Channel.unbuffered(r);
        spawn Channel.send(1i64, tx) @ r;
        Channel.recv(rx) == 1i64
    }

    @test
    def testNewChannel18(): Bool = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        Channel.send(1i64, tx);
        Channel.recv(rx) == 1i64
    }

    @test
    def testNewChannel19(): Bool \ IO = region r {
        let (tx, rx) = Channel.unbuffered(r);
        spawn Channel.send(1ii, tx) @ r;
        Channel.recv(rx) == 1ii
    }

    @test
    def testNewChannel20(): Bool = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        Channel.send(1ii, tx);
        Channel.recv(rx) == 1ii
    }

    @test
    def testNewChannel21(): Bool \ IO = region r {
        let (tx, rx) = Channel.unbuffered(r);
        spawn Channel.send("foo", tx) @ r;
        Channel.recv(rx) == "foo"
    }

    @test
    def testNewChannel22(): Bool = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        Channel.send("foo", tx);
        Channel.recv(rx) == "foo"
    }

    @test
    def testNewChannel23(): Bool \ IO = region r {
        let (tx, rx) = Channel.unbuffered(r);
        spawn Channel.send(Some(1), tx) @ r;
        Channel.recv(rx) == Some(1)
    }

    @test
    def testNewChannel24(): Bool = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        Channel.send(Some(1), tx);
        Channel.recv(rx) == Some(1)
    }

    @test
    def testNewChannel25(): Bool \ IO = region r {
        let (tx, rx): (Sender[Result[Int32, String], r], Receiver[Result[Int32, String], r]) = Channel.unbuffered(r);
        spawn Channel.send(Ok(1), tx) @ r;
        Channel.recv(rx) == Ok(1)
    }

    @test
    def testNewChannel26(): Bool = region r {
        let (tx, rx): (Sender[Result[Int32, String], r], Receiver[Result[Int32, String], r]) = Channel.buffered(r, 1);
        Channel.send(Ok(1), tx);
        Channel.recv(rx) == Ok(1)
    }

    @test
    def testNewChannel27(): Bool \ IO = region r {
        let (tx, rx) = Channel.unbuffered(r);
        let (tx2, rx2) = Channel.buffered(r, 1);
        spawn Channel.unsafeSend(tx2, tx) @ r;
        Channel.recv(rx) |> Channel.send(1);
        Channel.recv(rx2) == 1
    }
}
