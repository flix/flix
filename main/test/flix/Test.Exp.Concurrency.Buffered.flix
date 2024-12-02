mod Test.Exp.Concurrency.Buffered {

    @test
    def testBufferedChannel01(): Bool \ {Chan, NonDet} = {
        let (tx, rx) = Channel.buffered(1);
        Channel.send((), tx);
        () == Channel.recv(rx)
    }

    @test
    def testBufferedChannel02(): Bool \ {Chan, NonDet} = {
        let (tx, rx) = Channel.buffered(1);
        Channel.send(true, tx);
        true == Channel.recv(rx)
    }

    @test
    def testBufferedChannel03(): Bool \ {Chan, NonDet} = {
        let (tx, rx) = Channel.buffered(1);
        Channel.send(123.456f32, tx);
        123.456f32 == Channel.recv(rx)
    }

    @test
    def testBufferedChannel04(): Bool \ {Chan, NonDet} = {
        let (tx, rx) = Channel.buffered(1);
        Channel.send(123.456f64, tx);
        123.456f64 == Channel.recv(rx)
    }

    @test
    def testBufferedChannel05(): Bool \ {Chan, NonDet} = {
        let (tx, rx) = Channel.buffered(1);
        Channel.send(42i8, tx);
        42i8 == Channel.recv(rx)
    }

    @test
    def testBufferedChannel06(): Bool \ {Chan, NonDet} = {
        let (tx, rx) = Channel.buffered(1);
        Channel.send(42i16, tx);
        42i16 == Channel.recv(rx)
    }

    @test
    def testBufferedChannel07(): Bool \ {Chan, NonDet} = {
        let (tx, rx) = Channel.buffered(1);
        Channel.send(42i32, tx);
        42i32 == Channel.recv(rx)
    }

    @test
    def testBufferedChannel08(): Bool \ {Chan, NonDet} = {
        let (tx, rx) = Channel.buffered(1);
        Channel.send(42i64, tx);
        42i64 == Channel.recv(rx)
    }

    @test
    def testBufferedChannel09(): Bool \ {Chan, NonDet} = {
        let (tx, rx) = Channel.buffered(1);
        Channel.send(42ii, tx);
        42ii == Channel.recv(rx)
    }

    @test
    def testBufferedChannel10(): Bool \ {Chan, NonDet} = {
        let (tx, rx) = Channel.buffered(1);
        Channel.send("Hello World!", tx);
        "Hello World!" == Channel.recv(rx)
    }

    @test
    def testBufferedChannel11(): Bool \ {Chan, NonDet} = {
        let (tx, rx): (Sender[Option[Int32]], Receiver[Option[Int32]]) = Channel.buffered(1);
        Channel.send(None, tx);
        None == Channel.recv(rx)
    }

    @test
    def testBufferedChannel12(): Bool \ {Chan, NonDet} = {
        let (tx, rx) = Channel.buffered(1);
        Channel.send(Some(123), tx);
        Some(123) == Channel.recv(rx)
    }

    @test
    def testBufferedChannel13(): Bool \ {Chan, NonDet} = {
        let (tx, rx): (Sender[Result[String, Int32]], Receiver[Result[String, Int32]]) = Channel.buffered(1);
        Channel.send(Ok(123), tx);
        Ok(123) == Channel.recv(rx)
    }

    @test
    def testBufferedChannel14(): Bool \ {Chan, NonDet} = {
        let (tx, rx): (Sender[Result[String, Int32]], Receiver[Result[String, Int32]]) = Channel.buffered(1);
        Channel.send(Err("Goodbye World!"), tx);
        Err("Goodbye World!") == Channel.recv(rx)
    }

    @test
    def testBufferedChannel15(): Bool \ {Chan, NonDet} = region rc {
        let (tx, rx) = Channel.buffered(1);
        Channel.unsafeSend(Array#{1, 2, 3} @ rc, tx);
        2 == Array.get(1, Channel.recv(rx))
    }

    @test
    def testBufferedChannel16(): Bool \ {Chan, NonDet} = {
        let (tx1, rx1) = Channel.buffered(1);
        let (tx2, rx2) = Channel.buffered(1);
        Channel.unsafeSend(rx2, tx1);
        Channel.send(42, tx2);
        42 == Channel.recv(Channel.recv(rx1))
    }

    @test
    def testBufferedChannelWithSpawn01(): Bool \ {Chan, NonDet, IO} = region rc {
        let (tx, rx) = Channel.buffered(1);
        spawn Channel.send((), tx) @ rc;
        () == Channel.recv(rx)
    }

    @test
    def testBufferedChannelWithSpawn02(): Bool \ {Chan, NonDet, IO} = region rc {
        let (tx, rx) = Channel.buffered(1);
        spawn Channel.send(true, tx) @ rc;
        true == Channel.recv(rx)
    }

    @test
    def testBufferedChannelWithSpawn03(): Bool \ {Chan, NonDet, IO} = region rc {
        let (tx, rx) = Channel.buffered(1);
        spawn Channel.send(123.456f32, tx) @ rc;
        123.456f32 == Channel.recv(rx)
    }

    @test
    def testBufferedChannelWithSpawn04(): Bool \ {Chan, NonDet, IO} = region rc {
        let (tx, rx) = Channel.buffered(1);
        spawn Channel.send(123.456f64, tx) @ rc;
        123.456f64 == Channel.recv(rx)
    }

    @test
    def testBufferedChannelWithSpawn05(): Bool \ {Chan, NonDet, IO} = region rc {
        let (tx, rx) = Channel.buffered(1);
        spawn Channel.send(42i8, tx) @ rc;
        42i8 == Channel.recv(rx)
    }

    @test
    def testBufferedChannelWithSpawn06(): Bool \ {Chan, NonDet, IO} = region rc {
        let (tx, rx) = Channel.buffered(1);
        spawn Channel.send(42i16, tx) @ rc;
        42i16 == Channel.recv(rx)
    }

    @test
    def testBufferedChannelWithSpawn07(): Bool \ {Chan, NonDet, IO} = region rc {
        let (tx, rx) = Channel.buffered(1);
        spawn Channel.send(42i32, tx) @ rc;
        42i32 == Channel.recv(rx)
    }

    @test
    def testBufferedChannelWithSpawn08(): Bool \ {Chan, NonDet, IO} = region rc {
        let (tx, rx) = Channel.buffered(1);
        spawn Channel.send(42i64, tx) @ rc;
        42i64 == Channel.recv(rx)
    }

    @test
    def testBufferedChannelWithSpawn09(): Bool \ {Chan, NonDet, IO} = region rc {
        let (tx, rx) = Channel.buffered(1);
        spawn Channel.send(42ii, tx) @ rc;
        42ii == Channel.recv(rx)
    }

    @test
    def testBufferedChannelWithSpawn10(): Bool \ {Chan, NonDet, IO} = region rc {
        let (tx, rx) = Channel.buffered(1);
        spawn Channel.send("Hello World!", tx) @ rc;
        "Hello World!" == Channel.recv(rx)
    }

    @test
    def testBufferedChannelWithSpawn11(): Bool \ {Chan, NonDet, IO} = region rc {
        let (tx, rx): (Sender[Option[Int32]], Receiver[Option[Int32]]) = Channel.buffered(1);
        spawn Channel.send(None, tx) @ rc;
        None == Channel.recv(rx)
    }

    @test
    def testBufferedChannelWithSpawn12(): Bool \ {Chan, NonDet, IO} = region rc {
        let (tx, rx) = Channel.buffered(1);
        spawn Channel.send(Some(123), tx) @ rc;
        Some(123) == Channel.recv(rx)
    }

    @test
    def testBufferedChannelWithSpawn13(): Bool \ {Chan, NonDet, IO} = region rc {
        let (tx, rx): (Sender[Result[String, Int32]], Receiver[Result[String, Int32]]) = Channel.buffered(1);
        spawn Channel.send(Ok(123), tx) @ rc;
        Ok(123) == Channel.recv(rx)
    }

    @test
    def testBufferedChannelWithSpawn14(): Bool \ {Chan, NonDet, IO} = region rc {
        let (tx, rx): (Sender[Result[String, Int32]], Receiver[Result[String, Int32]]) = Channel.buffered(1);
        spawn Channel.send(Err("Goodbye World!"), tx) @ rc;
        Err("Goodbye World!") == Channel.recv(rx)
    }

    @test
    def testBufferedChannelWithSpawn15(): Bool \ {Chan, NonDet, IO} = region rc {
        let (tx, rx) = Channel.buffered(1);
        spawn Channel.unsafeSend(Array#{1, 2, 3} @ rc, tx) @ rc;
        2 == Array.get(1, Channel.recv(rx))
    }

    @test
    def testBufferedChannelWithSpawn16(): Bool \ {Chan, NonDet, IO} = region rc {
        let (tx1, rx1) = Channel.buffered(1);
        let (tx2, rx2) = Channel.buffered(1);
        spawn Channel.unsafeSend(rx2, tx1) @ rc;
        spawn Channel.send(42, tx2) @ rc;
        42 == Channel.recv(Channel.recv(rx1))
    }
}
