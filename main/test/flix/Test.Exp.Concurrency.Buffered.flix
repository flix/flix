mod Test.Exp.Concurrency.Buffered {

    @test
    def testBufferedChannel01(): Bool \ {} = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        Channel.send((), tx);
        () == Channel.recv(rx)
    }

    @test
    def testBufferedChannel02(): Bool \ {} = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        Channel.send(true, tx);
        true == Channel.recv(rx)
    }

    @test
    def testBufferedChannel03(): Bool \ {} = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        Channel.send(123.456f32, tx);
        123.456f32 == Channel.recv(rx)
    }

    @test
    def testBufferedChannel04(): Bool \ {} = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        Channel.send(123.456f64, tx);
        123.456f64 == Channel.recv(rx)
    }

    @test
    def testBufferedChannel05(): Bool \ {} = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        Channel.send(42i8, tx);
        42i8 == Channel.recv(rx)
    }

    @test
    def testBufferedChannel06(): Bool \ {} = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        Channel.send(42i16, tx);
        42i16 == Channel.recv(rx)
    }

    @test
    def testBufferedChannel07(): Bool \ {} = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        Channel.send(42i32, tx);
        42i32 == Channel.recv(rx)
    }

    @test
    def testBufferedChannel08(): Bool \ {} = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        Channel.send(42i64, tx);
        42i64 == Channel.recv(rx)
    }

    @test
    def testBufferedChannel09(): Bool \ {} = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        Channel.send(42ii, tx);
        42ii == Channel.recv(rx)
    }

    @test
    def testBufferedChannel10(): Bool \ {} = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        Channel.send("Hello World!", tx);
        "Hello World!" == Channel.recv(rx)
    }

    @test
    def testBufferedChannel11(): Bool \ {} = region r {
        let (tx, rx): (Sender[Option[Int32], _], Receiver[Option[Int32], _]) = Channel.buffered(r, 1);
        Channel.send(None, tx);
        None == Channel.recv(rx)
    }

    @test
    def testBufferedChannel12(): Bool \ {} = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        Channel.send(Some(123), tx);
        Some(123) == Channel.recv(rx)
    }

    @test
    def testBufferedChannel13(): Bool \ {} = region r {
        let (tx, rx): (Sender[Result[String, Int32], _], Receiver[Result[String, Int32], _]) = Channel.buffered(r, 1);
        Channel.send(Ok(123), tx);
        Ok(123) == Channel.recv(rx)
    }

    @test
    def testBufferedChannel14(): Bool \ {} = region r {
        let (tx, rx): (Sender[Result[String, Int32], _], Receiver[Result[String, Int32], _]) = Channel.buffered(r, 1);
        Channel.send(Err("Goodbye World!"), tx);
        Err("Goodbye World!") == Channel.recv(rx)
    }

    @test
    def testBufferedChannel15(): Bool \ {} = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        Channel.unsafeSend(Array#{1, 2, 3} @ r, tx);
        2 == Array.get(1, Channel.recv(rx))
    }

    @test
    def testBufferedChannel16(): Bool \ {} = region r {
        let (tx1, rx1) = Channel.buffered(r, 1);
        let (tx2, rx2) = Channel.buffered(r, 1);
        Channel.unsafeSend(rx2, tx1);
        Channel.send(42, tx2);
        42 == Channel.recv(Channel.recv(rx1))
    }

    @test
    def testBufferedChannelWithSpawn01(): Bool \ IO = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        spawn Channel.send((), tx) @ r;
        () == Channel.recv(rx)
    }

    @test
    def testBufferedChannelWithSpawn02(): Bool \ IO = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        spawn Channel.send(true, tx) @ r;
        true == Channel.recv(rx)
    }

    @test
    def testBufferedChannelWithSpawn03(): Bool \ IO = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        spawn Channel.send(123.456f32, tx) @ r;
        123.456f32 == Channel.recv(rx)
    }

    @test
    def testBufferedChannelWithSpawn04(): Bool \ IO = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        spawn Channel.send(123.456f64, tx) @ r;
        123.456f64 == Channel.recv(rx)
    }

    @test
    def testBufferedChannelWithSpawn05(): Bool \ IO = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        spawn Channel.send(42i8, tx) @ r;
        42i8 == Channel.recv(rx)
    }

    @test
    def testBufferedChannelWithSpawn06(): Bool \ IO = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        spawn Channel.send(42i16, tx) @ r;
        42i16 == Channel.recv(rx)
    }

    @test
    def testBufferedChannelWithSpawn07(): Bool \ IO = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        spawn Channel.send(42i32, tx) @ r;
        42i32 == Channel.recv(rx)
    }

    @test
    def testBufferedChannelWithSpawn08(): Bool \ IO = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        spawn Channel.send(42i64, tx) @ r;
        42i64 == Channel.recv(rx)
    }

    @test
    def testBufferedChannelWithSpawn09(): Bool \ IO = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        spawn Channel.send(42ii, tx) @ r;
        42ii == Channel.recv(rx)
    }

    @test
    def testBufferedChannelWithSpawn10(): Bool \ IO = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        spawn Channel.send("Hello World!", tx) @ r;
        "Hello World!" == Channel.recv(rx)
    }

    @test
    def testBufferedChannelWithSpawn11(): Bool \ IO = region r {
        let (tx, rx): (Sender[Option[Int32], _], Receiver[Option[Int32], _]) = Channel.buffered(r, 1);
        spawn Channel.send(None, tx) @ r;
        None == Channel.recv(rx)
    }

    @test
    def testBufferedChannelWithSpawn12(): Bool \ IO = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        spawn Channel.send(Some(123), tx) @ r;
        Some(123) == Channel.recv(rx)
    }

    @test
    def testBufferedChannelWithSpawn13(): Bool \ IO = region r {
        let (tx, rx): (Sender[Result[String, Int32], _], Receiver[Result[String, Int32], _]) = Channel.buffered(r, 1);
        spawn Channel.send(Ok(123), tx) @ r;
        Ok(123) == Channel.recv(rx)
    }

    @test
    def testBufferedChannelWithSpawn14(): Bool \ IO = region r {
        let (tx, rx): (Sender[Result[String, Int32], _], Receiver[Result[String, Int32], _]) = Channel.buffered(r, 1);
        spawn Channel.send(Err("Goodbye World!"), tx) @ r;
        Err("Goodbye World!") == Channel.recv(rx)
    }

    @test
    def testBufferedChannelWithSpawn15(): Bool \ IO = region r {
        let (tx, rx) = Channel.buffered(r, 1);
        spawn Channel.unsafeSend(Array#{1, 2, 3} @ r, tx) @ r;
        2 == Array.get(1, Channel.recv(rx))
    }

    @test
    def testBufferedChannelWithSpawn16(): Bool \ IO = region r {
        let (tx1, rx1) = Channel.buffered(r, 1);
        let (tx2, rx2) = Channel.buffered(r, 1);
        spawn Channel.unsafeSend(rx2, tx1) @ r;
        spawn Channel.send(42, tx2) @ r;
        42 == Channel.recv(Channel.recv(rx1))
    }
}
