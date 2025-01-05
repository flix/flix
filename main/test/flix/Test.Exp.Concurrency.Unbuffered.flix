mod Test.Exp.Concurrency.Unbuffered {

    @test
    def testUnbufferedChannelPutGet01(): Bool \ {Chan, NonDet} = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.send((), tx) @ rc;
        () == Channel.recv(rx)
    }

    @test
    def testUnbufferedChannelPutGet02(): Bool \ {Chan, NonDet} = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.send(true, tx) @ rc;
        true == Channel.recv(rx)
    }

    @test
    def testUnbufferedChannelPutGet03(): Bool \ {Chan, NonDet} = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.send(123.456f32, tx) @ rc;
        123.456f32 == Channel.recv(rx)
    }

    @test
    def testUnbufferedChannelPutGet04(): Bool \ {Chan, NonDet} = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.send(123.456f64, tx) @ rc;
        123.456f64 == Channel.recv(rx)
    }

    @test
    def testUnbufferedChannelPutGet05(): Bool \ {Chan, NonDet} = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.send(42i8, tx) @ rc;
        42i8 == Channel.recv(rx)
    }

    @test
    def testUnbufferedChannelPutGet06(): Bool \ {Chan, NonDet} = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.send(42i16, tx) @ rc;
        42i16 == Channel.recv(rx)
    }

    @test
    def testUnbufferedChannelPutGet07(): Bool \ {Chan, NonDet} = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.send(42i32, tx) @ rc;
        42i32 == Channel.recv(rx)
    }

    @test
    def testUnbufferedChannelPutGet08(): Bool \ {Chan, NonDet} = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.send(42i64, tx) @ rc;
        42i64 == Channel.recv(rx)
    }

    @test
    def testUnbufferedChannelPutGet09(): Bool \ {Chan, NonDet} = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.send(42ii, tx) @ rc;
        42ii == Channel.recv(rx)
    }

    @test
    def testUnbufferedChannelPutGet10(): Bool \ {Chan, NonDet} = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.send("Hello World!", tx) @ rc;
        "Hello World!" == Channel.recv(rx)
    }

    @test
    def testUnbufferedChannelPutGet11(): Bool \ {Chan, NonDet} = region rc {
        let (tx, rx): (Sender[Option[Int32]], Receiver[Option[Int32]]) = Channel.unbuffered();
        spawn Channel.send(None, tx) @ rc;
        None == Channel.recv(rx)
    }

    @test
    def testUnbufferedChannelPutGet12(): Bool \ {Chan, NonDet} = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.send(Some(123), tx) @ rc;
        Some(123) == Channel.recv(rx)
    }

    @test
    def testUnbufferedChannelPutGet13(): Bool \ {Chan, NonDet} = region rc {
        let (tx, rx): (Sender[Result[String, Int32]], Receiver[Result[String, Int32]]) = Channel.unbuffered();
        spawn Channel.send(Ok(123), tx) @ rc;
        Ok(123) == Channel.recv(rx)
    }

    @test
    def testUnbufferedChannelPutGet14(): Bool \ {Chan, NonDet} = region rc {
        let (tx, rx): (Sender[Result[String, Int32]], Receiver[Result[String, Int32]]) = Channel.unbuffered();
        spawn Channel.send(Err("Goodbye World!"), tx) @ rc;
        Err("Goodbye World!") == Channel.recv(rx)
    }

    @test
    def testUnbufferedChannelPutGet15(): Bool \ {Chan, NonDet} = region rc {
        let (tx1, rx1) = Channel.unbuffered();
        let (tx2, rx2) = Channel.unbuffered();
        spawn Channel.send(rx2, tx1) @ rc;
        spawn Channel.send(42, tx2) @ rc;
        42 == Channel.recv(Channel.recv(rx1))
    }

    @test
    def testUnbufferedChannelGetPut01(): Unit \ {Chan, NonDet} = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.recv(rx) @ rc;
        Channel.send((), tx)
    }

    @test
    def testUnbufferedChannelGetPut02(): Unit \ {Chan, NonDet} = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.recv(rx) @ rc;
        Channel.send(true, tx)
    }

    @test
    def testUnbufferedChannelGetPut03(): Unit \ {Chan, NonDet} = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.recv(rx) @ rc;
        Channel.send(123.456f32, tx)
    }

    @test
    def testUnbufferedChannelGetPut04(): Unit \ {Chan, NonDet} = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.recv(rx) @ rc;
        Channel.send(123.456f64, tx)
    }

    @test
    def testUnbufferedChannelGetPut05(): Unit \ {Chan, NonDet} = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.recv(rx) @ rc;
        Channel.send(42i8, tx)
    }

    @test
    def testUnbufferedChannelGetPut06(): Unit \ {Chan, NonDet} = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.recv(rx) @ rc;
        Channel.send(42i16, tx)
    }

    @test
    def testUnbufferedChannelGetPut07(): Unit \ {Chan, NonDet} = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.recv(rx) @ rc;
        Channel.send(42i32, tx)
    }

    @test
    def testUnbufferedChannelGetPut08(): Unit \ {Chan, NonDet} = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.recv(rx) @ rc;
        Channel.send(42i64, tx)
    }

    @test
    def testUnbufferedChannelGetPut09(): Unit \ {Chan, NonDet} = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.recv(rx) @ rc;
        Channel.send(42ii, tx)
    }

    @test
    def testUnbufferedChannelGetPut10(): Unit \ {Chan, NonDet} = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.recv(rx) @ rc;
        Channel.send("Hello World!", tx)
    }

    @test
    def testUnbufferedChannelGetPut11(): Unit \ {Chan, NonDet} = region rc {
        let (tx, rx): (Sender[Option[Int32]], Receiver[Option[Int32]]) = Channel.unbuffered();
        spawn Channel.recv(rx) @ rc;
        Channel.send(None, tx)
    }

    @test
    def testUnbufferedChannelGetPut12(): Unit \ {Chan, NonDet} = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.recv(rx) @ rc;
        Channel.send(Some(123), tx)
    }

    @test
    def testUnbufferedChannelGetPut13(): Unit \ {Chan, NonDet} = region rc {
        let (tx, rx): (Sender[Result[String, Int32]], Receiver[Result[String, Int32]]) = Channel.unbuffered();
        spawn Channel.recv(rx) @ rc;
        Channel.send(Ok(123), tx)
    }

    @test
    def testUnbufferedChannelGetPut14(): Unit \ {Chan, NonDet} = region rc {
        let (tx, rx): (Sender[Result[String, Int32]], Receiver[Result[String, Int32]]) = Channel.unbuffered();
        spawn Channel.recv(rx) @ rc;
        Channel.send(Err("Goodbye World!"), tx)
    }

    @test
    def testUnbufferedChannelGetPut15(): Unit \ {Chan, NonDet} = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.recv(rx) @ rc;
        Channel.send(Array#{1, 2, 3} @ rc, tx)
    }

    @test
    def testUnbufferedChannelGetPut16(): Unit \ {Chan, NonDet} = region rc {
        let (tx1, rx1) = Channel.unbuffered();
        let (tx2, rx2) = Channel.unbuffered();
        spawn Channel.recv(Channel.recv(rx1)) @ rc;
        spawn Channel.send(rx2, tx1) @ rc;
        Channel.send(42, tx2)
    }
}
