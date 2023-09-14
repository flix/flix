mod Test.Exp.Concurrency.Unbuffered {

    @Test
    def testUnbufferedChannelPutGet01(): Bool \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.send((), tx) @ rc;
        () == Channel.recv(rx)
    }

    @Test
    def testUnbufferedChannelPutGet02(): Bool \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.send(true, tx) @ rc;
        true == Channel.recv(rx)
    }

    @Test
    def testUnbufferedChannelPutGet03(): Bool \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.send(123.456f32, tx) @ rc;
        123.456f32 == Channel.recv(rx)
    }

    @Test
    def testUnbufferedChannelPutGet04(): Bool \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.send(123.456f64, tx) @ rc;
        123.456f64 == Channel.recv(rx)
    }

    @Test
    def testUnbufferedChannelPutGet05(): Bool \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.send(42i8, tx) @ rc;
        42i8 == Channel.recv(rx)
    }

    @Test
    def testUnbufferedChannelPutGet06(): Bool \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.send(42i16, tx) @ rc;
        42i16 == Channel.recv(rx)
    }

    @Test
    def testUnbufferedChannelPutGet07(): Bool \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.send(42i32, tx) @ rc;
        42i32 == Channel.recv(rx)
    }

    @Test
    def testUnbufferedChannelPutGet08(): Bool \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.send(42i64, tx) @ rc;
        42i64 == Channel.recv(rx)
    }

    @Test
    def testUnbufferedChannelPutGet09(): Bool \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.send(42ii, tx) @ rc;
        42ii == Channel.recv(rx)
    }

    @Test
    def testUnbufferedChannelPutGet10(): Bool \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.send("Hello World!", tx) @ rc;
        "Hello World!" == Channel.recv(rx)
    }

    @Test
    def testUnbufferedChannelPutGet11(): Bool \ IO = region rc {
        let (tx, rx): (Sender[Option[Int32], _], Receiver[Option[Int32], _]) = Channel.unbuffered(rc);
        spawn Channel.send(None, tx) @ rc;
        None == Channel.recv(rx)
    }

    @Test
    def testUnbufferedChannelPutGet12(): Bool \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.send(Some(123), tx) @ rc;
        Some(123) == Channel.recv(rx)
    }

    @Test
    def testUnbufferedChannelPutGet13(): Bool \ IO = region rc {
        let (tx, rx): (Sender[Result[String, Int32], _], Receiver[Result[String, Int32], _]) = Channel.unbuffered(rc);
        spawn Channel.send(Ok(123), tx) @ rc;
        Ok(123) == Channel.recv(rx)
    }

    @Test
    def testUnbufferedChannelPutGet14(): Bool \ IO = region rc {
        let (tx, rx): (Sender[Result[String, Int32], _], Receiver[Result[String, Int32], _]) = Channel.unbuffered(rc);
        spawn Channel.send(Err("Goodbye World!"), tx) @ rc;
        Err("Goodbye World!") == Channel.recv(rx)
    }

    @Test
    def testUnbufferedChannelPutGet15(): Bool \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.unsafeSend(Array#{1, 2, 3} @ rc, tx) @ rc;
        2 == Array.get(1, Channel.recv(rx))
    }

    @Test
    def testUnbufferedChannelPutGet16(): Bool \ IO = region rc {
        let (tx1, rx1) = Channel.unbuffered(rc);
        let (tx2, rx2) = Channel.unbuffered(rc);
        spawn Channel.unsafeSend(rx2, tx1) @ rc;
        spawn Channel.send(42, tx2) @ rc;
        42 == Channel.recv(Channel.recv(rx1))
    }

    @Test
    def testUnbufferedChannelGetPut01(): Unit \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.recv(rx) @ rc;
        Channel.send((), tx)
    }

    @Test
    def testUnbufferedChannelGetPut02(): Unit \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.recv(rx) @ rc;
        Channel.send(true, tx)
    }

    @Test
    def testUnbufferedChannelGetPut03(): Unit \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.recv(rx) @ rc;
        Channel.send(123.456f32, tx)
    }

    @Test
    def testUnbufferedChannelGetPut04(): Unit \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.recv(rx) @ rc;
        Channel.send(123.456f64, tx)
    }

    @Test
    def testUnbufferedChannelGetPut05(): Unit \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.recv(rx) @ rc;
        Channel.send(42i8, tx)
    }

    @Test
    def testUnbufferedChannelGetPut06(): Unit \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.recv(rx) @ rc;
        Channel.send(42i16, tx)
    }

    @Test
    def testUnbufferedChannelGetPut07(): Unit \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.recv(rx) @ rc;
        Channel.send(42i32, tx)
    }

    @Test
    def testUnbufferedChannelGetPut08(): Unit \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.recv(rx) @ rc;
        Channel.send(42i64, tx)
    }

    @Test
    def testUnbufferedChannelGetPut09(): Unit \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.recv(rx) @ rc;
        Channel.send(42ii, tx)
    }

    @Test
    def testUnbufferedChannelGetPut10(): Unit \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.recv(rx) @ rc;
        Channel.send("Hello World!", tx)
    }

    @Test
    def testUnbufferedChannelGetPut11(): Unit \ IO = region rc {
        let (tx, rx): (Sender[Option[Int32], _], Receiver[Option[Int32], _]) = Channel.unbuffered(rc);
        spawn Channel.recv(rx) @ rc;
        Channel.send(None, tx)
    }

    @Test
    def testUnbufferedChannelGetPut12(): Unit \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.recv(rx) @ rc;
        Channel.send(Some(123), tx)
    }

    @Test
    def testUnbufferedChannelGetPut13(): Unit \ IO = region rc {
        let (tx, rx): (Sender[Result[String, Int32], _], Receiver[Result[String, Int32], _]) = Channel.unbuffered(rc);
        spawn Channel.recv(rx) @ rc;
        Channel.send(Ok(123), tx)
    }

    @Test
    def testUnbufferedChannelGetPut14(): Unit \ IO = region rc {
        let (tx, rx): (Sender[Result[String, Int32], _], Receiver[Result[String, Int32], _]) = Channel.unbuffered(rc);
        spawn Channel.recv(rx) @ rc;
        Channel.send(Err("Goodbye World!"), tx)
    }

    @Test
    def testUnbufferedChannelGetPut15(): Unit \ IO = region rc {
        let (tx, rx) = Channel.unbuffered(rc);
        spawn Channel.recv(rx) @ rc;
        Channel.unsafeSend(Array#{1, 2, 3} @ rc, tx)
    }

    @Test
    def testUnbufferedChannelGetPut16(): Unit \ IO = region rc {
        let (tx1, rx1) = Channel.unbuffered(rc);
        let (tx2, rx2) = Channel.unbuffered(rc);
        spawn Channel.recv(Channel.recv(rx1)) @ rc;
        spawn Channel.unsafeSend(rx2, tx1) @ rc;
        Channel.send(42, tx2)
    }
}
