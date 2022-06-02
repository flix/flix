namespace Test/Exp/Concurrency/Buffered {

    @test
    def testBufferedChannel01(): Bool & Impure =
        let c = chan Unit 1;
        c <- ();
        () == <- c

    @test
    def testBufferedChannel02(): Bool & Impure =
        let c = chan Bool 1;
        c <- true;
        true == <- c

    @test
    def testBufferedChannel03(): Bool & Impure =
        let c = chan Float32 1;
        c <- 123.456f32;
        123.456f32 == <- c

    @test
    def testBufferedChannel04(): Bool & Impure =
        let c = chan Float64 1;
        c <- 123.456f64;
        123.456f64 == <- c

    @test
    def testBufferedChannel05(): Bool & Impure =
        let c = chan Int8 1;
        c <- 42i8;
        42i8 == <- c

    @test
    def testBufferedChannel06(): Bool & Impure =
        let c = chan Int16 1;
        c <- 42i16;
        42i16 == <- c

    @test
    def testBufferedChannel07(): Bool & Impure =
        let c = chan Int32 1;
        c <- 42i32;
        42i32 == <- c

    @test
    def testBufferedChannel08(): Bool & Impure =
        let c = chan Int64 1;
        c <- 42i64;
        42i64 == <- c

    @test
    def testBufferedChannel09(): Bool & Impure =
        let c = chan BigInt 1;
        c <- 42ii;
        42ii == <- c

    @test
    def testBufferedChannel10(): Bool & Impure =
        let c = chan String 1;
        c <- "Hello World!";
        "Hello World!" == <- c

    @test
    def testBufferedChannel11(): Bool & Impure =
        let c = chan Option[Int32] 1;
        c <- None;
        None == <- c

    @test
    def testBufferedChannel12(): Bool & Impure =
        let c = chan Option[Int32] 1;
        c <- Some(123);
        Some(123) == <- c

    @test
    def testBufferedChannel13(): Bool & Impure =
        let c = chan Result[Int32, String] 1;
        c <- Ok(123);
        Ok(123) == <- c

    @test
    def testBufferedChannel14(): Bool & Impure =
        let c = chan Result[Int32, String] 1;
        c <- Err("Goodbye World!");
        Err("Goodbye World!") == <- c

    @test
    def testBufferedChannel15(): Bool & Impure =
        let c = chan UnscopedArray[Int32] 1;
        c <- [1, 2, 3];
        2 == (<- c)[1]

    @test
    def testBufferedChannel16(): Bool & Impure =
        let c1 = chan Channel[Int32] 1;
        let c2 = chan Int32 1;
        c1 <- c2;
        c2 <- 42;
        42 == <- <- c1

    @test
    def testBufferedChannelWithSpawn01(): Bool & Impure =
        let c = chan Unit 1;
        spawn c <- ();
        () == <- c

    @test
    def testBufferedChannelWithSpawn02(): Bool & Impure =
        let c = chan Bool 1;
        spawn c <- true;
        true == <- c

    @test
    def testBufferedChannelWithSpawn03(): Bool & Impure =
        let c = chan Float32 1;
        spawn c <- 123.456f32;
        123.456f32 == <- c

    @test
    def testBufferedChannelWithSpawn04(): Bool & Impure =
        let c = chan Float64 1;
        spawn c <- 123.456f64;
        123.456f64 == <- c

    @test
    def testBufferedChannelWithSpawn05(): Bool & Impure =
        let c = chan Int8 1;
        spawn c <- 42i8;
        42i8 == <- c

    @test
    def testBufferedChannelWithSpawn06(): Bool & Impure =
        let c = chan Int16 1;
        spawn c <- 42i16;
        42i16 == <- c

    @test
    def testBufferedChannelWithSpawn07(): Bool & Impure =
        let c = chan Int32 1;
        spawn c <- 42i32;
        42i32 == <- c

    @test
    def testBufferedChannelWithSpawn08(): Bool & Impure =
        let c = chan Int64 1;
        spawn c <- 42i64;
        42i64 == <- c

    @test
    def testBufferedChannelWithSpawn09(): Bool & Impure =
        let c = chan BigInt 1;
        spawn c <- 42ii;
        42ii == <- c

    @test
    def testBufferedChannelWithSpawn10(): Bool & Impure =
        let c = chan String 1;
        spawn c <- "Hello World!";
        "Hello World!" == <- c

    @test
    def testBufferedChannelWithSpawn11(): Bool & Impure =
        let c = chan Option[Int32] 1;
        spawn c <- None;
        None == <- c

    @test
    def testBufferedChannelWithSpawn12(): Bool & Impure =
        let c = chan Option[Int32] 1;
        spawn c <- Some(123);
        Some(123) == <- c

    @test
    def testBufferedChannelWithSpawn13(): Bool & Impure =
        let c = chan Result[Int32, String] 1;
        spawn c <- Ok(123);
        Ok(123) == <- c

    @test
    def testBufferedChannelWithSpawn14(): Bool & Impure =
        let c = chan Result[Int32, String] 1;
        spawn c <- Err("Goodbye World!");
        Err("Goodbye World!") == <- c

    @test
    def testBufferedChannelWithSpawn15(): Bool & Impure =
        let c = chan UnscopedArray[Int32] 1;
        spawn c <- [1, 2, 3];
        2 == (<- c)[1]

    @test
    def testBufferedChannelWithSpawn16(): Bool & Impure =
        let c1 = chan Channel[Int32] 1;
        let c2 = chan Int32 1;
        spawn c1 <- c2;
        spawn c2 <- 42;
        42 == <- <- c1

}
