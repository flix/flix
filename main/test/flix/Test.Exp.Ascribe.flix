mod Test.Exp.Ascribe {

    @Test
    def testAscribe01(): Unit = () : Unit

    @Test
    def testAscribe02(): Bool = true : Bool

    @Test
    def testAscribe03(): Bool = not false : Bool

    @Test
    def testAscribe04(): Char = 'a' : Char

    @Test
    def testAscribe05(): Float32 = 0.0f32 : Float32

    @Test
    def testAscribe06(): Float64 = 0.0f64 : Float64

    @Test
    def testAscribe07(): Int8 = 0i8 : Int8

    @Test
    def testAscribe08(): Int16 = 0i16 : Int16

    @Test
    def testAscribe09(): Int32 = 0i32 : Int32

    @Test
    def testAscribe10(): Int64 = 0i64 : Int64

    @Test
    def testAscribe11(): BigInt = 0ii : BigInt

    @Test
    def testAscribe12(): BigDecimal = 0.0ff : BigDecimal

    @Test
    def testAscribe13(): String = "hello world" : String

    @Test
    def testAscribe14(): (Int32, Int32, Int32) = (1, 2, 3) : (Int32, Int32, Int32)

    @Test
    def testAscribe15(): Option[Int32] = Some(42) : Option[Int32]

}
