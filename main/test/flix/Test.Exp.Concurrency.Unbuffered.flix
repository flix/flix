namespace Test/Exp/Concurrency/Unbuffered {

    @test
    def testUnbufferedChannelPutGet01(): Bool \ IO =
        let c = chan Unit 0;
        spawn Channel.send((), c);
        () == Channel.recv(c)

    @test
    def testUnbufferedChannelPutGet02(): Bool \ IO =
        let c = chan Bool 0;
        spawn Channel.send(true, c);
        true == Channel.recv(c)

    @test
    def testUnbufferedChannelPutGet03(): Bool \ IO =
        let c = chan Float32 0;
        spawn Channel.send(123.456f32, c);
        123.456f32 == Channel.recv(c)

    @test
    def testUnbufferedChannelPutGet04(): Bool \ IO =
        let c = chan Float64 0;
        spawn Channel.send(123.456f64, c);
        123.456f64 == Channel.recv(c)

    @test
    def testUnbufferedChannelPutGet05(): Bool \ IO =
        let c = chan Int8 0;
        spawn Channel.send(42i8, c);
        42i8 == Channel.recv(c)

    @test
    def testUnbufferedChannelPutGet06(): Bool \ IO =
        let c = chan Int16 0;
        spawn Channel.send(42i16, c);
        42i16 == Channel.recv(c)

    @test
    def testUnbufferedChannelPutGet07(): Bool \ IO =
        let c = chan Int32 0;
        spawn Channel.send(42i32, c);
        42i32 == Channel.recv(c)

    @test
    def testUnbufferedChannelPutGet08(): Bool \ IO =
        let c = chan Int64 0;
        spawn Channel.send(42i64, c);
        42i64 == Channel.recv(c)

    @test
    def testUnbufferedChannelPutGet09(): Bool \ IO =
        let c = chan BigInt 0;
        spawn Channel.send(42ii, c);
        42ii == Channel.recv(c)

    @test
    def testUnbufferedChannelPutGet10(): Bool \ IO =
        let c = chan String 0;
        spawn Channel.send("Hello World!", c);
        "Hello World!" == Channel.recv(c)

    @test
    def testUnbufferedChannelPutGet11(): Bool \ IO =
        let c: Channel[Option[Int32]] = chan Option[Int32] 0;
        spawn Channel.send(None, c);
        None == Channel.recv(c)

    @test
    def testUnbufferedChannelPutGet12(): Bool \ IO =
        let c = chan Option[Int32] 0;
        spawn Channel.send(Some(123), c);
        Some(123) == Channel.recv(c)

    @test
    def testUnbufferedChannelPutGet13(): Bool \ IO =
        let c: Channel[Result[Int32, String]] = chan Result[Int32, String] 0;
        spawn Channel.send(Ok(123), c);
        Ok(123) == Channel.recv(c)

    @test
    def testUnbufferedChannelPutGet14(): Bool \ IO =
        let c: Channel[Result[Int32, String]] = chan Result[Int32, String] 0;
        spawn Channel.send(Err("Goodbye World!"), c);
        Err("Goodbye World!") == Channel.recv(c)

    @test
    def testUnbufferedChannelPutGet15(): Bool \ IO =
        let c = chan Array[Int32, Static] 0;
        spawn Channel.send([1, 2, 3], c);
        2 == (Channel.recv(c))[1]

    @test
    def testUnbufferedChannelPutGet16(): Bool \ IO =
        let c1 = chan Channel[Int32] 0;
        let c2 = chan Int32 0;
        spawn Channel.send(c2, c1);
        spawn Channel.send(42, c2);
        42 == Channel.recv(Channel.recv(c1)
)
    @test
    def testUnbufferedChannelGetPut01(): Unit \ IO =
        let c = chan Unit 0;
        spawn Channel.recv(c);
        Channel.send((), c)

    @test
    def testUnbufferedChannelGetPut02(): Unit \ IO =
        let c = chan Bool 0;
        spawn Channel.recv(c);
        Channel.send(true, c)

    @test
    def testUnbufferedChannelGetPut03(): Unit \ IO =
        let c = chan Float32 0;
        spawn Channel.recv(c);
        Channel.send(123.456f32, c)

    @test
    def testUnbufferedChannelGetPut04(): Unit \ IO =
        let c = chan Float64 0;
        spawn Channel.recv(c);
        Channel.send(123.456f64, c)

    @test
    def testUnbufferedChannelGetPut05(): Unit \ IO =
        let c = chan Int8 0;
        spawn Channel.recv(c);
        Channel.send(42i8, c)
    @test

    def testUnbufferedChannelGetPut06(): Unit \ IO =
        let c = chan Int16 0;
        spawn Channel.recv(c);
        Channel.send(42i16, c)

    @test
    def testUnbufferedChannelGetPut07(): Unit \ IO =
        let c = chan Int32 0;
        spawn Channel.recv(c);
        Channel.send(42i32, c)

    @test
    def testUnbufferedChannelGetPut08(): Unit \ IO =
        let c = chan Int64 0;
        spawn Channel.recv(c);
        Channel.send(42i64, c)

    @test
    def testUnbufferedChannelGetPut09(): Unit \ IO =
        let c = chan BigInt 0;
        spawn Channel.recv(c);
        Channel.send(42ii, c)

    @test
    def testUnbufferedChannelGetPut10(): Unit \ IO =
        let c = chan String 0;
        spawn Channel.recv(c);
        Channel.send("Hello World!", c)

    @test
    def testUnbufferedChannelGetPut11(): Unit \ IO =
        let c = chan Option[Int32] 0;
        spawn Channel.recv(c);
        Channel.send(None, c)

    @test
    def testUnbufferedChannelGetPut12(): Unit \ IO =
        let c = chan Option[Int32] 0;
        spawn Channel.recv(c);
        Channel.send(Some(123), c)

    @test
    def testUnbufferedChannelGetPut13(): Unit \ IO =
        let c = chan Result[Int32, String] 0;
        spawn Channel.recv(c);
        Channel.send(Ok(123), c)

    @test
    def testUnbufferedChannelGetPut14(): Unit \ IO =
        let c = chan Result[Int32, String] 0;
        spawn Channel.recv(c);
        Channel.send(Err("Goodbye World!"), c)

    @test
    def testUnbufferedChannelGetPut15(): Unit \ IO =
        let c = chan Array[Int32, Static] 0;
        spawn Channel.recv(c);
        Channel.send([1, 2, 3], c)

    @test
    def testUnbufferedChannelGetPut16(): Unit \ IO =
        let c1 = chan Channel[Int32] 0;
        let c2 = chan Int32 0;
        spawn Channel.recv(Channel.recv(c1));
        spawn Channel.send(c2, c1);
        Channel.send(42, c2)
}
