mod Test.Exp.Ascribe {

    @test
    def testAscribe01(): Unit = (() : Unit)

    @test
    def testAscribe02(): Bool = (true : Bool)

    @test
    def testAscribe03(): Bool = not (false : Bool)

    @test
    def testAscribe04(): Char = ('a' : Char)

    @test
    def testAscribe05(): Float32 = (0.0f32 : Float32)

    @test
    def testAscribe06(): Float64 = (0.0f64 : Float64)

    @test
    def testAscribe07(): Int8 = (0i8 : Int8)

    @test
    def testAscribe08(): Int16 = (0i16 : Int16)

    @test
    def testAscribe09(): Int32 = (0i32 : Int32)

    @test
    def testAscribe10(): Int64 = (0i64 : Int64)

    @test
    def testAscribe11(): BigInt = (0ii : BigInt)

    @test
    def testAscribe12(): BigDecimal = (0.0ff : BigDecimal)

    @test
    def testAscribe13(): String = ("hello world" : String)

    @test
    def testAscribe14(): (Int32, Int32, Int32) = ((1, 2, 3) : (Int32, Int32, Int32))

    @test
    def testAscribe15(): Option[Int32] = (Some(42) : Option[Int32])

}
