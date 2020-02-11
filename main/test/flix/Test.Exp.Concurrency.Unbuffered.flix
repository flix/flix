namespace Test/Exp/Concurrency/Unbuffered {

    @test
    def testUnbufferedChannel01(): Bool @ Impure =
        let c = chan Unit 0;
        spawn c <- ();
        () == <- c

    @test
    def testUnbufferedChannel02(): Bool @ Impure =
        let c = chan Bool 0;
        spawn c <- true;
        true == <- c

    @test
    def testUnbufferedChannel03(): Bool @ Impure =
        let c = chan Float32 0;
        spawn c <- 123.456f32;
        123.456f32 == <- c

    @test
    def testUnbufferedChannel04(): Bool @ Impure =
        let c = chan Float64 0;
        spawn c <- 123.456f64;
        123.456f64 == <- c

    @test
    def testUnbufferedChannel05(): Bool @ Impure =
        let c = chan Int8 0;
        spawn c <- 42i8;
        42i8 == <- c

    @test
    def testUnbufferedChannel06(): Bool @ Impure =
        let c = chan Int16 0;
        spawn c <- 42i16;
        42i16 == <- c

    @test
    def testUnbufferedChannel07(): Bool @ Impure =
        let c = chan Int32 0;
        spawn c <- 42i32;
        42i32 == <- c

    @test
    def testUnbufferedChannel08(): Bool @ Impure =
        let c = chan Int32 0;
        spawn c <- 42i32;
        42i32 == <- c

    @test
    def testUnbufferedChannel09(): Bool @ Impure =
        let c = chan Int64 0;
        spawn c <- 42i64;
        42i64 == <- c

    @test
    def testUnbufferedChannel10(): Bool @ Impure =
        let c = chan BigInt 0;
        spawn c <- 42ii;
        42ii == <- c

    @test
    def testUnbufferedChannel11(): Bool @ Impure =
        let c = chan String 0;
        spawn c <- "Hello World!";
        "Hello World!" == <- c

    @test
    def testUnbufferedChannel12(): Bool @ Impure =
        let c = chan Option[Int] 0;
        spawn c <- None;
        None == <- c

    @test
    def testUnbufferedChannel13(): Bool @ Impure =
        let c = chan Option[Int] 0;
        spawn c <- Some(123);
        Some(123) == <- c

    @test
    def testUnbufferedChannel14(): Bool @ Impure =
        let c = chan Result[Int, String] 0;
        spawn c <- Ok(123);
        Ok(123) == <- c

    @test
    def testUnbufferedChannel15(): Bool @ Impure =
        let c = chan Result[Int, String] 0;
        spawn c <- Err("Goodbye World!");
        Err("Goodbye World!") == <- c

    @test
    def testUnbufferedChannel16(): Bool @ Impure =
        let c = chan Array[Int] 0;
        spawn c <- [1, 2, 3];
        2 == (<- c)[1]

    @test
    def testUnbufferedChannel17(): Bool @ Impure =
        let c1 = chan Channel[Int] 0;
        let c2 = chan Int 0;
        spawn c1 <- c2;
        spawn c2 <- 42;
        42 == <- <- c1

}
