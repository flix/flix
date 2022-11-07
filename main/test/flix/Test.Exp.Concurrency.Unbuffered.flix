namespace Test/Exp/Concurrency/Unbuffered {

    @test
    def testUnbufferedChannelPutGet01(): Bool \ IO =
        let (s, r) = Channel.unbuffered();
        spawn Channel.send((), s);
        () == Channel.recv(r)

    @test
    def testUnbufferedChannelPutGet02(): Bool \ IO =
        let (s, r) = Channel.unbuffered();
        spawn Channel.send(true, s);
        true == Channel.recv(r)

    @test
    def testUnbufferedChannelPutGet03(): Bool \ IO =
        let (s, r) = Channel.unbuffered();
        spawn Channel.send(123.456f32, s);
        123.456f32 == Channel.recv(r)

    @test
    def testUnbufferedChannelPutGet04(): Bool \ IO =
        let (s, r) = Channel.unbuffered();
        spawn Channel.send(123.456f64, s);
        123.456f64 == Channel.recv(r)

    @test
    def testUnbufferedChannelPutGet05(): Bool \ IO =
        let (s, r) = Channel.unbuffered();
        spawn Channel.send(42i8, s);
        42i8 == Channel.recv(r)

    @test
    def testUnbufferedChannelPutGet06(): Bool \ IO =
        let (s, r) = Channel.unbuffered();
        spawn Channel.send(42i16, s);
        42i16 == Channel.recv(r)

    @test
    def testUnbufferedChannelPutGet07(): Bool \ IO =
        let (s, r) = Channel.unbuffered();
        spawn Channel.send(42i32, s);
        42i32 == Channel.recv(r)

    @test
    def testUnbufferedChannelPutGet08(): Bool \ IO =
        let (s, r) = Channel.unbuffered();
        spawn Channel.send(42i64, s);
        42i64 == Channel.recv(r)

    @test
    def testUnbufferedChannelPutGet09(): Bool \ IO =
        let (s, r) = Channel.unbuffered();
        spawn Channel.send(42ii, s);
        42ii == Channel.recv(r)

    @test
    def testUnbufferedChannelPutGet10(): Bool \ IO =
        let (s, r) = Channel.unbuffered();
        spawn Channel.send("Hello World!", s);
        "Hello World!" == Channel.recv(r)

    @test
    def testUnbufferedChannelPutGet11(): Bool \ IO =
        let (s, r): (Sender[Option[Int32]], Receiver[Option[Int32]]) = Channel.unbuffered();
        spawn Channel.send(None, s);
        None == Channel.recv(r)

    @test
    def testUnbufferedChannelPutGet12(): Bool \ IO =
        let (s, r) = Channel.unbuffered();
        spawn Channel.send(Some(123), s);
        Some(123) == Channel.recv(r)

    @test
    def testUnbufferedChannelPutGet13(): Bool \ IO =
        let (s, r): (Sender[Result[Int32, String]], Receiver[Result[Int32, String]]) = Channel.unbuffered();
        spawn Channel.send(Ok(123), s);
        Ok(123) == Channel.recv(r)

    @test
    def testUnbufferedChannelPutGet14(): Bool \ IO =
        let (s, r): (Sender[Result[Int32, String]], Receiver[Result[Int32, String]]) = Channel.unbuffered();
        spawn Channel.send(Err("Goodbye World!"), s);
        Err("Goodbye World!") == Channel.recv(r)

    @test
    def testUnbufferedChannelPutGet15(): Bool \ IO =
        let (s, r) = Channel.unbuffered();
        spawn Channel.send([1, 2, 3], s);
        2 == (Channel.recv(r))[1]

    @test
    def testUnbufferedChannelPutGet16(): Bool \ IO =
        let (s1, r1) = Channel.unbuffered();
        let (s2, r2) = Channel.unbuffered();
        spawn Channel.send(r2, s1);
        spawn Channel.send(42, s2);
        42 == Channel.recv(Channel.recv(r1))

    @test
    def testUnbufferedChannelGetPut01(): Unit \ IO =
        let (s, r) = Channel.unbuffered();
        spawn Channel.recv(r);
        Channel.send((), s)

    @test
    def testUnbufferedChannelGetPut02(): Unit \ IO =
        let (s, r) = Channel.unbuffered();
        spawn Channel.recv(r);
        Channel.send(true, s)

    @test
    def testUnbufferedChannelGetPut03(): Unit \ IO =
        let (s, r) = Channel.unbuffered();
        spawn Channel.recv(r);
        Channel.send(123.456f32, s)

    @test
    def testUnbufferedChannelGetPut04(): Unit \ IO =
        let (s, r) = Channel.unbuffered();
        spawn Channel.recv(r);
        Channel.send(123.456f64, s)

    @test
    def testUnbufferedChannelGetPut05(): Unit \ IO =
        let (s, r) = Channel.unbuffered();
        spawn Channel.recv(r);
        Channel.send(42i8, s)
    @test

    def testUnbufferedChannelGetPut06(): Unit \ IO =
        let (s, r) = Channel.unbuffered();
        spawn Channel.recv(r);
        Channel.send(42i16, s)

    @test
    def testUnbufferedChannelGetPut07(): Unit \ IO =
        let (s, r) = Channel.unbuffered();
        spawn Channel.recv(r);
        Channel.send(42i32, s)

    @test
    def testUnbufferedChannelGetPut08(): Unit \ IO =
        let (s, r) = Channel.unbuffered();
        spawn Channel.recv(r);
        Channel.send(42i64, s)

    @test
    def testUnbufferedChannelGetPut09(): Unit \ IO =
        let (s, r) = Channel.unbuffered();
        spawn Channel.recv(r);
        Channel.send(42ii, s)

    @test
    def testUnbufferedChannelGetPut10(): Unit \ IO =
        let (s, r) = Channel.unbuffered();
        spawn Channel.recv(r);
        Channel.send("Hello World!", s)

    @test
    def testUnbufferedChannelGetPut11(): Unit \ IO =
        let (s, r) = Channel.unbuffered();
        spawn Channel.recv(r);
        Channel.send(None, s)

    @test
    def testUnbufferedChannelGetPut12(): Unit \ IO =
        let (s, r) = Channel.unbuffered();
        spawn Channel.recv(r);
        Channel.send(Some(123), s)

    @test
    def testUnbufferedChannelGetPut13(): Unit \ IO =
        let (s, r) = Channel.unbuffered();
        spawn Channel.recv(r);
        Channel.send(Ok(123), s)

    @test
    def testUnbufferedChannelGetPut14(): Unit \ IO =
        let (s, r) = Channel.unbuffered();
        spawn Channel.recv(r);
        Channel.send(Err("Goodbye World!"), s)

    @test
    def testUnbufferedChannelGetPut15(): Unit \ IO =
        let (s, r) = Channel.unbuffered();
        spawn Channel.recv(r);
        Channel.send([1, 2, 3], s)

    @test
    def testUnbufferedChannelGetPut16(): Unit \ IO =
        let (s1, r1) = Channel.unbuffered();
        let (s2, r2) = Channel.unbuffered();
        spawn Channel.recv(Channel.recv(r1));
        spawn Channel.send(r2, s1);
        Channel.send(42, s2)
}
