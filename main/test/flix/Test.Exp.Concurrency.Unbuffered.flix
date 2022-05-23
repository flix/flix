namespace Test/Exp/Concurrency/Unbuffered {

    @test
    def testUnbufferedChannelPutGet01(): Bool & Impure =
        let c = chan Unit 0;
        spawn c <- ();
        () == <- c

    @test
    def testUnbufferedChannelPutGet02(): Bool & Impure =
        let c = chan Bool 0;
        spawn c <- true;
        true == <- c

    @test
    def testUnbufferedChannelPutGet03(): Bool & Impure =
        let c = chan Float32 0;
        spawn c <- 123.456f32;
        123.456f32 == <- c

    @test
    def testUnbufferedChannelPutGet04(): Bool & Impure =
        let c = chan Float64 0;
        spawn c <- 123.456f64;
        123.456f64 == <- c

    @test
    def testUnbufferedChannelPutGet05(): Bool & Impure =
        let c = chan Int8 0;
        spawn c <- 42i8;
        42i8 == <- c

    @test
    def testUnbufferedChannelPutGet06(): Bool & Impure =
        let c = chan Int16 0;
        spawn c <- 42i16;
        42i16 == <- c

    @test
    def testUnbufferedChannelPutGet07(): Bool & Impure =
        let c = chan Int32 0;
        spawn c <- 42i32;
        42i32 == <- c

    @test
    def testUnbufferedChannelPutGet08(): Bool & Impure =
        let c = chan Int64 0;
        spawn c <- 42i64;
        42i64 == <- c

    @test
    def testUnbufferedChannelPutGet09(): Bool & Impure =
        let c = chan BigInt 0;
        spawn c <- 42ii;
        42ii == <- c

    @test
    def testUnbufferedChannelPutGet10(): Bool & Impure =
        let c = chan String 0;
        spawn c <- "Hello World!";
        "Hello World!" == <- c

    @test
    def testUnbufferedChannelPutGet11(): Bool & Impure =
        let c = chan Option[Int32] 0;
        spawn c <- None;
        None == <- c

    @test
    def testUnbufferedChannelPutGet12(): Bool & Impure =
        let c = chan Option[Int32] 0;
        spawn c <- Some(123);
        Some(123) == <- c

    @test
    def testUnbufferedChannelPutGet13(): Bool & Impure =
        let c = chan Result[Int32, String] 0;
        spawn c <- Ok(123);
        Ok(123) == <- c

    @test
    def testUnbufferedChannelPutGet14(): Bool & Impure =
        let c = chan Result[Int32, String] 0;
        spawn c <- Err("Goodbye World!");
        Err("Goodbye World!") == <- c

    @test
    def testUnbufferedChannelPutGet15(): Bool & Impure =
        let c = chan Array[Int32] 0;
        spawn c <- [1, 2, 3];
        2 == (<- c)[1]

    @test
    def testUnbufferedChannelPutGet16(): Bool & Impure =
        let c1 = chan Channel[Int32] 0;
        let c2 = chan Int32 0;
        spawn c1 <- c2;
        spawn c2 <- 42;
        42 == <- <- c1

    @test
    def testUnbufferedChannelGetPut01(): Unit & Impure =
        let c = chan Unit 0;
        spawn <- c;
        c <- ()

    @test
    def testUnbufferedChannelGetPut02(): Unit & Impure =
        let c = chan Bool 0;
        spawn <- c;
        c <- true

    @test
    def testUnbufferedChannelGetPut03(): Unit & Impure =
        let c = chan Float32 0;
        spawn <- c;
        c <- 123.456f32

    @test
    def testUnbufferedChannelGetPut04(): Unit & Impure =
        let c = chan Float64 0;
        spawn <- c;
        c <- 123.456f64

    @test
    def testUnbufferedChannelGetPut05(): Unit & Impure =
        let c = chan Int8 0;
        spawn <- c;
        c <- 42i8
    @test

    def testUnbufferedChannelGetPut06(): Unit & Impure =
        let c = chan Int16 0;
        spawn <- c;
        c <- 42i16

    @test
    def testUnbufferedChannelGetPut07(): Unit & Impure =
        let c = chan Int32 0;
        spawn <- c;
        c <- 42i32

    @test
    def testUnbufferedChannelGetPut08(): Unit & Impure =
        let c = chan Int64 0;
        spawn <- c;
        c <- 42i64

    @test
    def testUnbufferedChannelGetPut09(): Unit & Impure =
        let c = chan BigInt 0;
        spawn <- c;
        c <- 42ii

    @test
    def testUnbufferedChannelGetPut10(): Unit & Impure =
        let c = chan String 0;
        spawn <- c;
        c <- "Hello World!"

    @test
    def testUnbufferedChannelGetPut11(): Unit & Impure =
        let c = chan Option[Int32] 0;
        spawn <- c;
        c <- None

    @test
    def testUnbufferedChannelGetPut12(): Unit & Impure =
        let c = chan Option[Int32] 0;
        spawn <- c;
        c <- Some(123)

    @test
    def testUnbufferedChannelGetPut13(): Unit & Impure =
        let c = chan Result[Int32, String] 0;
        spawn <- c;
        c <- Ok(123)

    @test
    def testUnbufferedChannelGetPut14(): Unit & Impure =
        let c = chan Result[Int32, String] 0;
        spawn <- c;
        c <- Err("Goodbye World!")

    @test
    def testUnbufferedChannelGetPut15(): Unit & Impure =
        let c = chan Array[Int32] 0;
        spawn <- c;
        c <- [1, 2, 3]

    @test
    def testUnbufferedChannelGetPut16(): Unit & Impure =
        let c1 = chan Channel[Int32] 0;
        let c2 = chan Int32 0;
        spawn <- <- c1;
        spawn c1 <- c2;
        c2 <- 42
}
