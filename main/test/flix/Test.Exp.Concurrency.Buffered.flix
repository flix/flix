namespace Test/Exp/Concurrency/Buffered {

    @test
    def testBufferedChannel01(): Bool \ IO =
        let (s, r) = Channel.buffered(1);
        Channel.send((), s);
        () == Channel.recv(r)

    @test
    def testBufferedChannel02(): Bool \ IO =
        let (s, r) = Channel.buffered(1);
        Channel.send(true, s);
        true == Channel.recv(r)

    @test
    def testBufferedChannel03(): Bool \ IO =
        let (s, r) = Channel.buffered(1);
        Channel.send(123.456f32, s);
        123.456f32 == Channel.recv(r)

    @test
    def testBufferedChannel04(): Bool \ IO =
        let (s, r) = Channel.buffered(1);
        Channel.send(123.456f64, s);
        123.456f64 == Channel.recv(r)

    @test
    def testBufferedChannel05(): Bool \ IO =
        let (s, r) = Channel.buffered(1);
        Channel.send(42i8, s);
        42i8 == Channel.recv(r)

    @test
    def testBufferedChannel06(): Bool \ IO =
        let (s, r) = Channel.buffered(1);
        Channel.send(42i16, s);
        42i16 == Channel.recv(r)

    @test
    def testBufferedChannel07(): Bool \ IO =
        let (s, r) = Channel.buffered(1);
        Channel.send(42i32, s);
        42i32 == Channel.recv(r)

    @test
    def testBufferedChannel08(): Bool \ IO =
        let (s, r) = Channel.buffered(1);
        Channel.send(42i64, s);
        42i64 == Channel.recv(r)

    @test
    def testBufferedChannel09(): Bool \ IO =
        let (s, r) = Channel.buffered(1);
        Channel.send(42ii, s);
        42ii == Channel.recv(r)

    @test
    def testBufferedChannel10(): Bool \ IO =
        let (s, r) = Channel.buffered(1);
        Channel.send("Hello World!", s);
        "Hello World!" == Channel.recv(r)

    @test
    def testBufferedChannel11(): Bool \ IO =
        let (s, r): (Sender[Option[Int32], _], Receiver[Option[Int32], _]) = Channel.buffered(1);
        Channel.send(None, s);
        None == Channel.recv(r)

    @test
    def testBufferedChannel12(): Bool \ IO =
        let (s, r) = Channel.buffered(1);
        Channel.send(Some(123), s);
        Some(123) == Channel.recv(r)

    @test
    def testBufferedChannel13(): Bool \ IO =
        let (s, r): (Sender[Result[Int32, String], _], Receiver[Result[Int32, String], _]) = Channel.buffered(1);
        Channel.send(Ok(123), s);
        Ok(123) == Channel.recv(r)

    @test
    def testBufferedChannel14(): Bool \ IO =
        let (s, r): (Sender[Result[Int32, String], _], Receiver[Result[Int32, String], _]) = Channel.buffered(1);
        Channel.send(Err("Goodbye World!"), s);
        Err("Goodbye World!") == Channel.recv(r)

    @test
    def testBufferedChannel15(): Bool \ IO =
        let (s, r) = Channel.buffered(1);
        Channel.unsafeSend([1, 2, 3], s);
        2 == (Channel.recv(r))[1]

    @test
    def testBufferedChannel16(): Bool \ IO =
        let (s1, r1) = Channel.buffered(1);
        let (s2, r2) = Channel.buffered(1);
        Channel.unsafeSend(r2, s1);
        Channel.send(42, s2);
        42 == Channel.recv(Channel.recv(r1))

    @test
    def testBufferedChannelWithSpawn01(): Bool \ IO =
        let (s, r) = Channel.buffered(1);
        spawn Channel.send((), s);
        () == Channel.recv(r)

    @test
    def testBufferedChannelWithSpawn02(): Bool \ IO =
        let (s, r) = Channel.buffered(1);
        spawn Channel.send(true, s);
        true == Channel.recv(r)

    @test
    def testBufferedChannelWithSpawn03(): Bool \ IO =
        let (s, r) = Channel.buffered(1);
        spawn Channel.send(123.456f32, s);
        123.456f32 == Channel.recv(r)

    @test
    def testBufferedChannelWithSpawn04(): Bool \ IO =
        let (s, r) = Channel.buffered(1);
        spawn Channel.send(123.456f64, s);
        123.456f64 == Channel.recv(r)

    @test
    def testBufferedChannelWithSpawn05(): Bool \ IO =
        let (s, r) = Channel.buffered(1);
        spawn Channel.send(42i8, s);
        42i8 == Channel.recv(r)

    @test
    def testBufferedChannelWithSpawn06(): Bool \ IO =
        let (s, r) = Channel.buffered(1);
        spawn Channel.send(42i16, s);
        42i16 == Channel.recv(r)

    @test
    def testBufferedChannelWithSpawn07(): Bool \ IO =
        let (s, r) = Channel.buffered(1);
        spawn Channel.send(42i32, s);
        42i32 == Channel.recv(r)

    @test
    def testBufferedChannelWithSpawn08(): Bool \ IO =
        let (s, r) = Channel.buffered(1);
        spawn Channel.send(42i64, s);
        42i64 == Channel.recv(r)

    @test
    def testBufferedChannelWithSpawn09(): Bool \ IO =
        let (s, r) = Channel.buffered(1);
        spawn Channel.send(42ii, s);
        42ii == Channel.recv(r)

    @test
    def testBufferedChannelWithSpawn10(): Bool \ IO =
        let (s, r) = Channel.buffered(1);
        spawn Channel.send("Hello World!", s);
        "Hello World!" == Channel.recv(r)

    @test
    def testBufferedChannelWithSpawn11(): Bool \ IO =
        let (s, r): (Sender[Option[Int32], _], Receiver[Option[Int32], _]) = Channel.buffered(1);
        spawn Channel.send(None, s);
        None == Channel.recv(r)

    @test
    def testBufferedChannelWithSpawn12(): Bool \ IO =
        let (s, r) = Channel.buffered(1);
        spawn Channel.send(Some(123), s);
        Some(123) == Channel.recv(r)

    @test
    def testBufferedChannelWithSpawn13(): Bool \ IO =
        let (s, r): (Sender[Result[Int32, String], _], Receiver[Result[Int32, String], _]) = Channel.buffered(1);
        spawn Channel.send(Ok(123), s);
        Ok(123) == Channel.recv(r)

    @test
    def testBufferedChannelWithSpawn14(): Bool \ IO =
        let (s, r): (Sender[Result[Int32, String], _], Receiver[Result[Int32, String], _]) = Channel.buffered(1);
        spawn Channel.send(Err("Goodbye World!"), s);
        Err("Goodbye World!") == Channel.recv(r)

    @test
    def testBufferedChannelWithSpawn15(): Bool \ IO =
        let (s, r) = Channel.buffered(1);
        spawn Channel.unsafeSend([1, 2, 3], s);
        2 == (Channel.recv(r))[1]

    @test
    def testBufferedChannelWithSpawn16(): Bool \ IO =
        let (s1, r1) = Channel.buffered(1);
        let (s2, r2) = Channel.buffered(1);
        spawn Channel.unsafeSend(r2, s1);
        spawn Channel.send(42, s2);
        42 == Channel.recv(Channel.recv(r1))

}
