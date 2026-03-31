mod Test.Exp.Concurrency.Buffered {

    use Assert.assertEq

    @Test
    def testBufferedChannel01(): Unit \ (Assert + Chan + NonDet) = {
        let (tx, rx) = Channel.buffered(1);
        Channel.send((), tx);
        let r = Channel.recv(rx);
        assertEq(expected = (), r)
    }

    @Test
    def testBufferedChannel02(): Unit \ (Assert + Chan + NonDet) = {
        let (tx, rx) = Channel.buffered(1);
        Channel.send(true, tx);
        let r = Channel.recv(rx);
        assertEq(expected = true, r)
    }

    @Test
    def testBufferedChannel03(): Unit \ (Assert + Chan + NonDet) = {
        let (tx, rx) = Channel.buffered(1);
        Channel.send(123.456f32, tx);
        let r = Channel.recv(rx);
        assertEq(expected = 123.456f32, r)
    }

    @Test
    def testBufferedChannel04(): Unit \ (Assert + Chan + NonDet) = {
        let (tx, rx) = Channel.buffered(1);
        Channel.send(123.456f64, tx);
        let r = Channel.recv(rx);
        assertEq(expected = 123.456f64, r)
    }

    @Test
    def testBufferedChannel05(): Unit \ (Assert + Chan + NonDet) = {
        let (tx, rx) = Channel.buffered(1);
        Channel.send(42i8, tx);
        let r = Channel.recv(rx);
        assertEq(expected = 42i8, r)
    }

    @Test
    def testBufferedChannel06(): Unit \ (Assert + Chan + NonDet) = {
        let (tx, rx) = Channel.buffered(1);
        Channel.send(42i16, tx);
        let r = Channel.recv(rx);
        assertEq(expected = 42i16, r)
    }

    @Test
    def testBufferedChannel07(): Unit \ (Assert + Chan + NonDet) = {
        let (tx, rx) = Channel.buffered(1);
        Channel.send(42i32, tx);
        let r = Channel.recv(rx);
        assertEq(expected = 42i32, r)
    }

    @Test
    def testBufferedChannel08(): Unit \ (Assert + Chan + NonDet) = {
        let (tx, rx) = Channel.buffered(1);
        Channel.send(42i64, tx);
        let r = Channel.recv(rx);
        assertEq(expected = 42i64, r)
    }

    @Test
    def testBufferedChannel09(): Unit \ (Assert + Chan + NonDet) = {
        let (tx, rx) = Channel.buffered(1);
        Channel.send(42ii, tx);
        let r = Channel.recv(rx);
        assertEq(expected = 42ii, r)
    }

    @Test
    def testBufferedChannel10(): Unit \ (Assert + Chan + NonDet) = {
        let (tx, rx) = Channel.buffered(1);
        Channel.send("Hello World!", tx);
        let r = Channel.recv(rx);
        assertEq(expected = "Hello World!", r)
    }

    @Test
    def testBufferedChannel11(): Unit \ (Assert + Chan + NonDet) = {
        let (tx, rx): (Sender[Option[Int32]], Receiver[Option[Int32]]) = Channel.buffered(1);
        Channel.send(None, tx);
        let r = Channel.recv(rx);
        assertEq(expected = None, r)
    }

    @Test
    def testBufferedChannel12(): Unit \ (Assert + Chan + NonDet) = {
        let (tx, rx) = Channel.buffered(1);
        Channel.send(Some(123), tx);
        let r = Channel.recv(rx);
        assertEq(expected = Some(123), r)
    }

    @Test
    def testBufferedChannel13(): Unit \ (Assert + Chan + NonDet) = {
        let (tx, rx): (Sender[Result[String, Int32]], Receiver[Result[String, Int32]]) = Channel.buffered(1);
        Channel.send(Ok(123), tx);
        let r = Channel.recv(rx);
        assertEq(expected = Ok(123), r)
    }

    @Test
    def testBufferedChannel14(): Unit \ (Assert + Chan + NonDet) = {
        let (tx, rx): (Sender[Result[String, Int32]], Receiver[Result[String, Int32]]) = Channel.buffered(1);
        Channel.send(Err("Goodbye World!"), tx);
        let r = Channel.recv(rx);
        assertEq(expected = Err("Goodbye World!"), r)
    }

    @Test
    def testBufferedChannel15(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx, rx) = Channel.buffered(1);
        Channel.send(Array#{1, 2, 3} @ rc, tx);
        let r = Array.get(1, Channel.recv(rx));
        assertEq(expected = 2, r)
    }

    @Test
    def testBufferedChannel16(): Unit \ (Assert + Chan + NonDet) = {
        let (tx1, rx1) = Channel.buffered(1);
        let (tx2, rx2) = Channel.buffered(1);
        Channel.send(rx2, tx1);
        Channel.send(42, tx2);
        let r = Channel.recv(Channel.recv(rx1));
        assertEq(expected = 42, r)
    }

    @Test
    def testBufferedChannelWithSpawn01(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx, rx) = Channel.buffered(1);
        spawn Channel.send((), tx) @ rc;
        let r = Channel.recv(rx);
        assertEq(expected = (), r)
    }

    @Test
    def testBufferedChannelWithSpawn02(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx, rx) = Channel.buffered(1);
        spawn Channel.send(true, tx) @ rc;
        let r = Channel.recv(rx);
        assertEq(expected = true, r)
    }

    @Test
    def testBufferedChannelWithSpawn03(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx, rx) = Channel.buffered(1);
        spawn Channel.send(123.456f32, tx) @ rc;
        let r = Channel.recv(rx);
        assertEq(expected = 123.456f32, r)
    }

    @Test
    def testBufferedChannelWithSpawn04(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx, rx) = Channel.buffered(1);
        spawn Channel.send(123.456f64, tx) @ rc;
        let r = Channel.recv(rx);
        assertEq(expected = 123.456f64, r)
    }

    @Test
    def testBufferedChannelWithSpawn05(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx, rx) = Channel.buffered(1);
        spawn Channel.send(42i8, tx) @ rc;
        let r = Channel.recv(rx);
        assertEq(expected = 42i8, r)
    }

    @Test
    def testBufferedChannelWithSpawn06(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx, rx) = Channel.buffered(1);
        spawn Channel.send(42i16, tx) @ rc;
        let r = Channel.recv(rx);
        assertEq(expected = 42i16, r)
    }

    @Test
    def testBufferedChannelWithSpawn07(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx, rx) = Channel.buffered(1);
        spawn Channel.send(42i32, tx) @ rc;
        let r = Channel.recv(rx);
        assertEq(expected = 42i32, r)
    }

    @Test
    def testBufferedChannelWithSpawn08(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx, rx) = Channel.buffered(1);
        spawn Channel.send(42i64, tx) @ rc;
        let r = Channel.recv(rx);
        assertEq(expected = 42i64, r)
    }

    @Test
    def testBufferedChannelWithSpawn09(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx, rx) = Channel.buffered(1);
        spawn Channel.send(42ii, tx) @ rc;
        let r = Channel.recv(rx);
        assertEq(expected = 42ii, r)
    }

    @Test
    def testBufferedChannelWithSpawn10(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx, rx) = Channel.buffered(1);
        spawn Channel.send("Hello World!", tx) @ rc;
        let r = Channel.recv(rx);
        assertEq(expected = "Hello World!", r)
    }

    @Test
    def testBufferedChannelWithSpawn11(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx, rx): (Sender[Option[Int32]], Receiver[Option[Int32]]) = Channel.buffered(1);
        spawn Channel.send(None, tx) @ rc;
        let r = Channel.recv(rx);
        assertEq(expected = None, r)
    }

    @Test
    def testBufferedChannelWithSpawn12(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx, rx) = Channel.buffered(1);
        spawn Channel.send(Some(123), tx) @ rc;
        let r = Channel.recv(rx);
        assertEq(expected = Some(123), r)
    }

    @Test
    def testBufferedChannelWithSpawn13(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx, rx): (Sender[Result[String, Int32]], Receiver[Result[String, Int32]]) = Channel.buffered(1);
        spawn Channel.send(Ok(123), tx) @ rc;
        let r = Channel.recv(rx);
        assertEq(expected = Ok(123), r)
    }

    @Test
    def testBufferedChannelWithSpawn14(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx, rx): (Sender[Result[String, Int32]], Receiver[Result[String, Int32]]) = Channel.buffered(1);
        spawn Channel.send(Err("Goodbye World!"), tx) @ rc;
        let r = Channel.recv(rx);
        assertEq(expected = Err("Goodbye World!"), r)
    }

    @Test
    def testBufferedChannelWithSpawn15(): Unit \ (Assert + Chan + NonDet) = region rc {
        let (tx1, rx1) = Channel.buffered(1);
        let (tx2, rx2) = Channel.buffered(1);
        spawn Channel.send(rx2, tx1) @ rc;
        spawn Channel.send(42, tx2) @ rc;
        let r = Channel.recv(Channel.recv(rx1));
        assertEq(expected = 42, r)
    }

}
