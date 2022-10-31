namespace Test/Exp/Concurrency/Buffered {

    @test
    def testBufferedChannel01(): Bool \ IO =
        let c = chan Unit 1;
        c <- ();
        () == Channel.recv(c)

    @test
    def testBufferedChannel02(): Bool \ IO =
        let c = chan Bool 1;
        c <- true;
        true == Channel.recv(c)

    @test
    def testBufferedChannel03(): Bool \ IO =
        let c = chan Float32 1;
        c <- 123.456f32;
        123.456f32 == Channel.recv(c)

    @test
    def testBufferedChannel04(): Bool \ IO =
        let c = chan Float64 1;
        c <- 123.456f64;
        123.456f64 == Channel.recv(c)

    @test
    def testBufferedChannel05(): Bool \ IO =
        let c = chan Int8 1;
        c <- 42i8;
        42i8 == Channel.recv(c)

    @test
    def testBufferedChannel06(): Bool \ IO =
        let c = chan Int16 1;
        c <- 42i16;
        42i16 == Channel.recv(c)

    @test
    def testBufferedChannel07(): Bool \ IO =
        let c = chan Int32 1;
        c <- 42i32;
        42i32 == Channel.recv(c)

    @test
    def testBufferedChannel08(): Bool \ IO =
        let c = chan Int64 1;
        c <- 42i64;
        42i64 == Channel.recv(c)

    @test
    def testBufferedChannel09(): Bool \ IO =
        let c = chan BigInt 1;
        c <- 42ii;
        42ii == Channel.recv(c)

    @test
    def testBufferedChannel10(): Bool \ IO =
        let c = chan String 1;
        c <- "Hello World!";
        "Hello World!" == Channel.recv(c)

    @test
    def testBufferedChannel11(): Bool \ IO =
        let c = chan Option[Int32] 1;
        c <- None;
        None == Channel.recv(c)

    @test
    def testBufferedChannel12(): Bool \ IO =
        let c = chan Option[Int32] 1;
        c <- Some(123);
        Some(123) == Channel.recv(c)

    @test
    def testBufferedChannel13(): Bool \ IO =
        let c = chan Result[Int32, String] 1;
        c <- Ok(123);
        Ok(123) == Channel.recv(c)

    @test
    def testBufferedChannel14(): Bool \ IO =
        let c = chan Result[Int32, String] 1;
        c <- Err("Goodbye World!");
        Err("Goodbye World!") == Channel.recv(c)

    @test
    def testBufferedChannel15(): Bool \ IO =
        let c = chan Array[Int32, Static] 1;
        c <- [1, 2, 3];
        2 == (Channel.recv(c))[1]

    @test
    def testBufferedChannel16(): Bool \ IO =
        let c1 = chan Channel[Int32] 1;
        let c2 = chan Int32 1;
        c1 <- c2;
        c2 <- 42;
        42 == Channel.recv(Channel.recv(c1))

    @test
    def testBufferedChannelWithSpawn01(): Bool \ IO =
        let c = chan Unit 1;
        spawn c <- ();
        () == Channel.recv(c)

    @test
    def testBufferedChannelWithSpawn02(): Bool \ IO =
        let c = chan Bool 1;
        spawn c <- true;
        true == Channel.recv(c)

    @test
    def testBufferedChannelWithSpawn03(): Bool \ IO =
        let c = chan Float32 1;
        spawn c <- 123.456f32;
        123.456f32 == Channel.recv(c)

    @test
    def testBufferedChannelWithSpawn04(): Bool \ IO =
        let c = chan Float64 1;
        spawn c <- 123.456f64;
        123.456f64 == Channel.recv(c)

    @test
    def testBufferedChannelWithSpawn05(): Bool \ IO =
        let c = chan Int8 1;
        spawn c <- 42i8;
        42i8 == Channel.recv(c)

    @test
    def testBufferedChannelWithSpawn06(): Bool \ IO =
        let c = chan Int16 1;
        spawn c <- 42i16;
        42i16 == Channel.recv(c)

    @test
    def testBufferedChannelWithSpawn07(): Bool \ IO =
        let c = chan Int32 1;
        spawn c <- 42i32;
        42i32 == Channel.recv(c)

    @test
    def testBufferedChannelWithSpawn08(): Bool \ IO =
        let c = chan Int64 1;
        spawn c <- 42i64;
        42i64 == Channel.recv(c)

    @test
    def testBufferedChannelWithSpawn09(): Bool \ IO =
        let c = chan BigInt 1;
        spawn c <- 42ii;
        42ii == Channel.recv(c)

    @test
    def testBufferedChannelWithSpawn10(): Bool \ IO =
        let c = chan String 1;
        spawn c <- "Hello World!";
        "Hello World!" == Channel.recv(c)

    @test
    def testBufferedChannelWithSpawn11(): Bool \ IO =
        let c = chan Option[Int32] 1;
        spawn c <- None;
        None == Channel.recv(c)

    @test
    def testBufferedChannelWithSpawn12(): Bool \ IO =
        let c = chan Option[Int32] 1;
        spawn c <- Some(123);
        Some(123) == Channel.recv(c)

    @test
    def testBufferedChannelWithSpawn13(): Bool \ IO =
        let c = chan Result[Int32, String] 1;
        spawn c <- Ok(123);
        Ok(123) == Channel.recv(c)

    @test
    def testBufferedChannelWithSpawn14(): Bool \ IO =
        let c = chan Result[Int32, String] 1;
        spawn c <- Err("Goodbye World!");
        Err("Goodbye World!") == Channel.recv(c)

    @test
    def testBufferedChannelWithSpawn15(): Bool \ IO =
        let c = chan Array[Int32, Static] 1;
        spawn c <- [1, 2, 3];
        2 == (Channel.recv(c))[1]

    @test
    def testBufferedChannelWithSpawn16(): Bool \ IO =
        let c1 = chan Channel[Int32] 1;
        let c2 = chan Int32 1;
        spawn c1 <- c2;
        spawn c2 <- 42;
        42 == Channel.recv(Channel.recv(c1))

}
