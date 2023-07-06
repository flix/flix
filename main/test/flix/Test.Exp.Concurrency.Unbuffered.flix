mod Test.Exp.Concurrency.Unbuffered {

    @test
    def testUnbufferedChannelPutGet01(): Bool \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.send((), tx) @ rc;
        () == Channel.recv(rx)
    }

    @test
    def testUnbufferedChannelPutGet02(): Bool \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.send(true, tx) @ rc;
        true == Channel.recv(rx)
    }

    @test
    def testUnbufferedChannelPutGet03(): Bool \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.send(123.456f32, tx) @ rc;
        123.456f32 == Channel.recv(rx)
    }

    @test
    def testUnbufferedChannelPutGet04(): Bool \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.send(123.456f64, tx) @ rc;
        123.456f64 == Channel.recv(rx)
    }

    @test
    def testUnbufferedChannelPutGet05(): Bool \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.send(42i8, tx) @ rc;
        42i8 == Channel.recv(rx)
    }

    @test
    def testUnbufferedChannelPutGet06(): Bool \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.send(42i16, tx) @ rc;
        42i16 == Channel.recv(rx)
    }

    @test
    def testUnbufferedChannelPutGet07(): Bool \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.send(42i32, tx) @ rc;
        42i32 == Channel.recv(rx)
    }

    @test
    def testUnbufferedChannelPutGet08(): Bool \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.send(42i64, tx) @ rc;
        42i64 == Channel.recv(rx)
    }

    @test
    def testUnbufferedChannelPutGet09(): Bool \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.send(42ii, tx) @ rc;
        42ii == Channel.recv(rx)
    }

    @test
    def testUnbufferedChannelPutGet10(): Bool \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.send("Hello World!", tx) @ rc;
        "Hello World!" == Channel.recv(rx)
    }

    @test
    def testUnbufferedChannelPutGet11(): Bool \ IO = region rc {
        let (tx, rx): (Sender[Option[Int32], _], Receiver[Option[Int32], _]) = Channel.unbuffered(rc);
        spawn Channel.send(None, tx) @ rc;
        None == Channel.recv(rx)
    }

    @test
    def testUnbufferedChannelPutGet12(): Bool \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.send(Some(123), tx) @ rc;
        Some(123) == Channel.recv(rx)
    }

    @test
    def testUnbufferedChannelPutGet13(): Bool \ IO = region rc {
        let (tx, rx): (Sender[Result[String, Int32], _], Receiver[Result[String, Int32], _]) = Channel.unbuffered(rc);
        spawn Channel.send(Ok(123), tx) @ rc;
        Ok(123) == Channel.recv(rx)
    }

    @test
    def testUnbufferedChannelPutGet14(): Bool \ IO = region rc {
        let (tx, rx): (Sender[Result[String, Int32], _], Receiver[Result[String, Int32], _]) = Channel.unbuffered(rc);
        spawn Channel.send(Err("Goodbye World!"), tx) @ rc;
        Err("Goodbye World!") == Channel.recv(rx)
    }

    @test
    def testUnbufferedChannelPutGet15(): Bool \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.unsafeSend(Array#{1, 2, 3} @ rc, tx) @ rc;
        2 == Array.get(1, Channel.recv(rx))
    }

    @test
    def testUnbufferedChannelPutGet16(): Bool \ IO = region rc {
        let (tx1, rx1) = Channel.unbuffered(rc);
        let (tx2, rx2) = Channel.unbuffered(rc);
        spawn Channel.unsafeSend(rx2, tx1) @ rc;
        spawn Channel.send(42, tx2) @ rc;
        42 == Channel.recv(Channel.recv(rx1))
    }

    @test
    def testUnbufferedChannelGetPut01(): Unit \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.recv(rx) @ rc;
        Channel.send((), tx)
    }

    @test
    def testUnbufferedChannelGetPut02(): Unit \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.recv(rx) @ rc;
        Channel.send(true, tx)
    }

    @test
    def testUnbufferedChannelGetPut03(): Unit \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.recv(rx) @ rc;
        Channel.send(123.456f32, tx)
    }

    @test
    def testUnbufferedChannelGetPut04(): Unit \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.recv(rx) @ rc;
        Channel.send(123.456f64, tx)
    }

    @test
    def testUnbufferedChannelGetPut05(): Unit \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.recv(rx) @ rc;
        Channel.send(42i8, tx)
    }

    @test
    def testUnbufferedChannelGetPut06(): Unit \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.recv(rx) @ rc;
        Channel.send(42i16, tx)
    }

    @test
    def testUnbufferedChannelGetPut07(): Unit \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.recv(rx) @ rc;
        Channel.send(42i32, tx)
    }

    @test
    def testUnbufferedChannelGetPut08(): Unit \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.recv(rx) @ rc;
        Channel.send(42i64, tx)
    }

    @test
    def testUnbufferedChannelGetPut09(): Unit \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.recv(rx) @ rc;
        Channel.send(42ii, tx)
    }

    @test
    def testUnbufferedChannelGetPut10(): Unit \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.recv(rx) @ rc;
        Channel.send("Hello World!", tx)
    }

    @test
    def testUnbufferedChannelGetPut11(): Unit \ IO = region rc {
        let (tx, rx): (Sender[Option[Int32], _], Receiver[Option[Int32], _]) = Channel.unbuffered(rc);
        spawn Channel.recv(rx) @ rc;
        Channel.send(None, tx)
    }

    @test
    def testUnbufferedChannelGetPut12(): Unit \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.recv(rx) @ rc;
        Channel.send(Some(123), tx)
    }

    @test
    def testUnbufferedChannelGetPut13(): Unit \ IO = region rc {
        let (tx, rx): (Sender[Result[String, Int32], _], Receiver[Result[String, Int32], _]) = Channel.unbuffered(rc);
        spawn Channel.recv(rx) @ rc;
        Channel.send(Ok(123), tx)
    }

    @test
    def testUnbufferedChannelGetPut14(): Unit \ IO = region rc {
        let (tx, rx): (Sender[Result[String, Int32], _], Receiver[Result[String, Int32], _]) = Channel.unbuffered(rc);
        spawn Channel.recv(rx) @ rc;
        Channel.send(Err("Goodbye World!"), tx)
    }

    @test
    def testUnbufferedChannelGetPut15(): Unit \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.recv(rx) @ rc;
        Channel.unsafeSend(Array#{1, 2, 3} @ rc, tx)
    }

    @test
    def testUnbufferedChannelGetPut16(): Unit \ IO = region rc {
        let (tx1, rx1) = Channel.unbuffered(rc);
        let (tx2, rx2) = Channel.unbuffered(rc);
        spawn Channel.recv(Channel.recv(rx1)) @ rc;
        spawn Channel.unsafeSend(rx2, tx1) @ rc;
        Channel.send(42, tx2)
    }
}
