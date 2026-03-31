mod Test.Exp.Ascribe {

    @CompileTest
    def testAscribe01(): Unit = (() : Unit)

    @CompileTest
    def testAscribe02(): Bool = (true : Bool)

    @CompileTest
    def testAscribe03(): Bool = not (false : Bool)

    @CompileTest
    def testAscribe04(): Char = ('a' : Char)

    @CompileTest
    def testAscribe05(): Float32 = (0.0f32 : Float32)

    @CompileTest
    def testAscribe06(): Float64 = (0.0f64 : Float64)

    @CompileTest
    def testAscribe07(): Int8 = (0i8 : Int8)

    @CompileTest
    def testAscribe08(): Int16 = (0i16 : Int16)

    @CompileTest
    def testAscribe09(): Int32 = (0i32 : Int32)

    @CompileTest
    def testAscribe10(): Int64 = (0i64 : Int64)

    @CompileTest
    def testAscribe11(): BigInt = (0ii : BigInt)

    @CompileTest
    def testAscribe12(): BigDecimal = (0.0ff : BigDecimal)

    @CompileTest
    def testAscribe13(): String = ("hello world" : String)

    @CompileTest
    def testAscribe14(): (Int32, Int32, Int32) = ((1, 2, 3) : (Int32, Int32, Int32))

    @CompileTest
    def testAscribe15(): Option[Int32] = (Some(42) : Option[Int32])

}
