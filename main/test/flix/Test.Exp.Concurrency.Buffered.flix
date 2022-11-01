namespace Test/Exp/Concurrency/Buffered {

    @test
    def testBufferedChannel01(): Bool \ IO =
        let c = chan Unit 1;
        Channel.send((), c);
        () == Channel.recv(c)

    @test
    def testBufferedChannel02(): Bool \ IO =
        let c = chan Bool 1;
        Channel.send(true, c);
        true == Channel.recv(c)

    @test
    def testBufferedChannel03(): Bool \ IO =
        let c = chan Float32 1;
        Channel.send(123.456f32, c);
        123.456f32 == Channel.recv(c)

    @test
    def testBufferedChannel04(): Bool \ IO =
        let c = chan Float64 1;
        Channel.send(123.456f64, c);
        123.456f64 == Channel.recv(c)

    @test
    def testBufferedChannel05(): Bool \ IO =
        let c = chan Int8 1;
        Channel.send(42i8, c);
        42i8 == Channel.recv(c)

    @test
    def testBufferedChannel06(): Bool \ IO =
        let c = chan Int16 1;
        Channel.send(42i16, c);
        42i16 == Channel.recv(c)

    @test
    def testBufferedChannel07(): Bool \ IO =
        let c = chan Int32 1;
        Channel.send(42i32, c);
        42i32 == Channel.recv(c)

    @test
    def testBufferedChannel08(): Bool \ IO =
        let c = chan Int64 1;
        Channel.send(42i64, c);
        42i64 == Channel.recv(c)

    @test
    def testBufferedChannel09(): Bool \ IO =
        let c = chan BigInt 1;
        Channel.send(42ii, c);
        42ii == Channel.recv(c)

    @test
    def testBufferedChannel10(): Bool \ IO =
        let c = chan String 1;
        Channel.send("Hello World!", c);
        "Hello World!" == Channel.recv(c)

    @test
    def testBufferedChannel11(): Bool \ IO =
        let c: Channel[Option[Int32]] = chan Option[Int32] 1;
        Channel.send(None, c);
        None == Channel.recv(c)

    @test
    def testBufferedChannel12(): Bool \ IO =
        let c = chan Option[Int32] 1;
        Channel.send(Some(123), c);
        Some(123) == Channel.recv(c)

    @test
    def testBufferedChannel13(): Bool \ IO =
        let c: Channel[Result[Int32, String]] = chan Result[Int32, String] 1;
        Channel.send(Ok(123), c);
        Ok(123) == Channel.recv(c)

    @test
    def testBufferedChannel14(): Bool \ IO =
        let c: Channel[Result[Int32, String]] = chan Result[Int32, String] 1;
        Channel.send(Err("Goodbye World!"), c);
        Err("Goodbye World!") == Channel.recv(c)

    @test
    def testBufferedChannel15(): Bool \ IO =
        let c = chan Array[Int32, Static] 1;
        Channel.send([1, 2, 3], c);
        2 == (Channel.recv(c))[1]

    @test
    def testBufferedChannel16(): Bool \ IO =
        let c1 = chan Channel[Int32] 1;
        let c2 = chan Int32 1;
        Channel.send(c2, c1);
        Channel.send(42, c2);
        42 == Channel.recv(Channel.recv(c1))

    @test
    def testBufferedChannelWithSpawn01(): Bool \ IO =
        let c = chan Unit 1;
        spawn Channel.send((), c);
        () == Channel.recv(c)

    @test
    def testBufferedChannelWithSpawn02(): Bool \ IO =
        let c = chan Bool 1;
        spawn Channel.send(true, c);
        true == Channel.recv(c)

    @test
    def testBufferedChannelWithSpawn03(): Bool \ IO =
        let c = chan Float32 1;
        spawn Channel.send(123.456f32, c);
        123.456f32 == Channel.recv(c)

    @test
    def testBufferedChannelWithSpawn04(): Bool \ IO =
        let c = chan Float64 1;
        spawn Channel.send(123.456f64, c);
        123.456f64 == Channel.recv(c)

    @test
    def testBufferedChannelWithSpawn05(): Bool \ IO =
        let c = chan Int8 1;
        spawn Channel.send(42i8, c);
        42i8 == Channel.recv(c)

    @test
    def testBufferedChannelWithSpawn06(): Bool \ IO =
        let c = chan Int16 1;
        spawn Channel.send(42i16, c);
        42i16 == Channel.recv(c)

    @test
    def testBufferedChannelWithSpawn07(): Bool \ IO =
        let c = chan Int32 1;
        spawn Channel.send(42i32, c);
        42i32 == Channel.recv(c)

    @test
    def testBufferedChannelWithSpawn08(): Bool \ IO =
        let c = chan Int64 1;
        spawn Channel.send(42i64, c);
        42i64 == Channel.recv(c)

    @test
    def testBufferedChannelWithSpawn09(): Bool \ IO =
        let c = chan BigInt 1;
        spawn Channel.send(42ii, c);
        42ii == Channel.recv(c)

    @test
    def testBufferedChannelWithSpawn10(): Bool \ IO =
        let c = chan String 1;
        spawn Channel.send("Hello World!", c);
        "Hello World!" == Channel.recv(c)

    @test
    def testBufferedChannelWithSpawn11(): Bool \ IO =
        let c: Channel[Option[Int32]] = chan Option[Int32] 1;
        spawn Channel.send(None, c);
        None == Channel.recv(c)

    @test
    def testBufferedChannelWithSpawn12(): Bool \ IO =
        let c = chan Option[Int32] 1;
        spawn Channel.send(Some(123), c);
        Some(123) == Channel.recv(c)

    @test
    def testBufferedChannelWithSpawn13(): Bool \ IO =
        let c: Channel[Result[Int32, String]] = chan Result[Int32, String] 1;
        spawn Channel.send(Ok(123), c);
        Ok(123) == Channel.recv(c)

    @test
    def testBufferedChannelWithSpawn14(): Bool \ IO =
        let c: Channel[Result[Int32, String]] = chan Result[Int32, String] 1;
        spawn Channel.send(Err("Goodbye World!"), c);
        Err("Goodbye World!") == Channel.recv(c)

    @test
    def testBufferedChannelWithSpawn15(): Bool \ IO =
        let c = chan Array[Int32, Static] 1;
        spawn Channel.send([1, 2, 3], c);
        2 == (Channel.recv(c))[1]

    @test
    def testBufferedChannelWithSpawn16(): Bool \ IO =
        let c1 = chan Channel[Int32] 1;
        let c2 = chan Int32 1;
        spawn Channel.send(c2, c1);
        spawn Channel.send(42, c2);
        42 == Channel.recv(Channel.recv(c1))

}
