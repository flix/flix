mod Test.Exp.Concurrency.Unbuffered {

    use Assert.assertEq

    @Test
    def testUnbufferedChannelPutGet01(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.send((), tx) @ rc;
        let r = Channel.recv(rx);
        assertEq(expected = (), r)
    }

    @Test
    def testUnbufferedChannelPutGet02(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.send(true, tx) @ rc;
        let r = Channel.recv(rx);
        assertEq(expected = true, r)
    }

    @Test
    def testUnbufferedChannelPutGet03(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.send(123.456f32, tx) @ rc;
        let r = Channel.recv(rx);
        assertEq(expected = 123.456f32, r)
    }

    @Test
    def testUnbufferedChannelPutGet04(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.send(123.456f64, tx) @ rc;
        let r = Channel.recv(rx);
        assertEq(expected = 123.456f64, r)
    }

    @Test
    def testUnbufferedChannelPutGet05(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.send(42i8, tx) @ rc;
        let r = Channel.recv(rx);
        assertEq(expected = 42i8, r)
    }

    @Test
    def testUnbufferedChannelPutGet06(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.send(42i16, tx) @ rc;
        let r = Channel.recv(rx);
        assertEq(expected = 42i16, r)
    }

    @Test
    def testUnbufferedChannelPutGet07(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.send(42i32, tx) @ rc;
        let r = Channel.recv(rx);
        assertEq(expected = 42i32, r)
    }

    @Test
    def testUnbufferedChannelPutGet08(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.send(42i64, tx) @ rc;
        let r = Channel.recv(rx);
        assertEq(expected = 42i64, r)
    }

    @Test
    def testUnbufferedChannelPutGet09(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.send(42ii, tx) @ rc;
        let r = Channel.recv(rx);
        assertEq(expected = 42ii, r)
    }

    @Test
    def testUnbufferedChannelPutGet10(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.send("Hello World!", tx) @ rc;
        let r = Channel.recv(rx);
        assertEq(expected = "Hello World!", r)
    }

    @Test
    def testUnbufferedChannelPutGet11(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx, rx): (Sender[Option[Int32]], Receiver[Option[Int32]]) = Channel.unbuffered();
        spawn Channel.send(None, tx) @ rc;
        let r = Channel.recv(rx);
        assertEq(expected = None, r)
    }

    @Test
    def testUnbufferedChannelPutGet12(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.send(Some(123), tx) @ rc;
        let r = Channel.recv(rx);
        assertEq(expected = Some(123), r)
    }

    @Test
    def testUnbufferedChannelPutGet13(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx, rx): (Sender[Result[String, Int32]], Receiver[Result[String, Int32]]) = Channel.unbuffered();
        spawn Channel.send(Ok(123), tx) @ rc;
        let r = Channel.recv(rx);
        assertEq(expected = Ok(123), r)
    }

    @Test
    def testUnbufferedChannelPutGet14(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx, rx): (Sender[Result[String, Int32]], Receiver[Result[String, Int32]]) = Channel.unbuffered();
        spawn Channel.send(Err("Goodbye World!"), tx) @ rc;
        let r = Channel.recv(rx);
        assertEq(expected = Err("Goodbye World!"), r)
    }

    @Test
    def testUnbufferedChannelPutGet15(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx1, rx1) = Channel.unbuffered();
        let (tx2, rx2) = Channel.unbuffered();
        spawn Channel.send(rx2, tx1) @ rc;
        spawn Channel.send(42, tx2) @ rc;
        let r = Channel.recv(Channel.recv(rx1));
        assertEq(expected = 42, r)
    }

    @Test
    def testUnbufferedChannelGetPut01(): Unit \ (Chan + NonDet) = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.recv(rx) @ rc;
        Channel.send((), tx)
    }

    @Test
    def testUnbufferedChannelGetPut02(): Unit \ (Chan + NonDet) = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.recv(rx) @ rc;
        Channel.send(true, tx)
    }

    @Test
    def testUnbufferedChannelGetPut03(): Unit \ (Chan + NonDet) = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.recv(rx) @ rc;
        Channel.send(123.456f32, tx)
    }

    @Test
    def testUnbufferedChannelGetPut04(): Unit \ (Chan + NonDet) = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.recv(rx) @ rc;
        Channel.send(123.456f64, tx)
    }

    @Test
    def testUnbufferedChannelGetPut05(): Unit \ (Chan + NonDet) = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.recv(rx) @ rc;
        Channel.send(42i8, tx)
    }

    @Test
    def testUnbufferedChannelGetPut06(): Unit \ (Chan + NonDet) = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.recv(rx) @ rc;
        Channel.send(42i16, tx)
    }

    @Test
    def testUnbufferedChannelGetPut07(): Unit \ (Chan + NonDet) = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.recv(rx) @ rc;
        Channel.send(42i32, tx)
    }

    @Test
    def testUnbufferedChannelGetPut08(): Unit \ (Chan + NonDet) = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.recv(rx) @ rc;
        Channel.send(42i64, tx)
    }

    @Test
    def testUnbufferedChannelGetPut09(): Unit \ (Chan + NonDet) = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.recv(rx) @ rc;
        Channel.send(42ii, tx)
    }

    @Test
    def testUnbufferedChannelGetPut10(): Unit \ (Chan + NonDet) = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.recv(rx) @ rc;
        Channel.send("Hello World!", tx)
    }

    @Test
    def testUnbufferedChannelGetPut11(): Unit \ (Chan + NonDet) = region rc {
        let (tx, rx): (Sender[Option[Int32]], Receiver[Option[Int32]]) = Channel.unbuffered();
        spawn Channel.recv(rx) @ rc;
        Channel.send(None, tx)
    }

    @Test
    def testUnbufferedChannelGetPut12(): Unit \ (Chan + NonDet) = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.recv(rx) @ rc;
        Channel.send(Some(123), tx)
    }

    @Test
    def testUnbufferedChannelGetPut13(): Unit \ (Chan + NonDet) = region rc {
        let (tx, rx): (Sender[Result[String, Int32]], Receiver[Result[String, Int32]]) = Channel.unbuffered();
        spawn Channel.recv(rx) @ rc;
        Channel.send(Ok(123), tx)
    }

    @Test
    def testUnbufferedChannelGetPut14(): Unit \ (Chan + NonDet) = region rc {
        let (tx, rx): (Sender[Result[String, Int32]], Receiver[Result[String, Int32]]) = Channel.unbuffered();
        spawn Channel.recv(rx) @ rc;
        Channel.send(Err("Goodbye World!"), tx)
    }

    @Test
    def testUnbufferedChannelGetPut15(): Unit \ (Chan + NonDet) = region rc {
        let (tx, rx) = Channel.unbuffered();
        spawn Channel.recv(rx) @ rc;
        Channel.send(Array#{1, 2, 3} @ rc, tx)
    }

    @Test
    def testUnbufferedChannelGetPut16(): Unit \ (Chan + NonDet) = region rc {
        let (tx1, rx1) = Channel.unbuffered();
        let (tx2, rx2) = Channel.unbuffered();
        spawn Channel.recv(Channel.recv(rx1)) @ rc;
        spawn Channel.send(rx2, tx1) @ rc;
        Channel.send(42, tx2)
    }

}
