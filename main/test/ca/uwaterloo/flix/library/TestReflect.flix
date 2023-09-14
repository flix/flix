mod TestReflect {
    use Reflect.default

    @Test
    def testDefaultUnit(): Bool = (default(): Unit) == ()

    @Test
    def testDefaultBool(): Bool = (default(): Bool) == false

    @Test
    def testDefaultChar(): Bool = (default(): Char) == '0'

    @Test
    def testDefaultFloat32(): Bool = (default(): Float32) == 0.0f32

    @Test
    def testDefaultFloat64(): Bool = (default(): Float64) == 0.0f64

    @Test
    def testDefaultBigDecimal(): Bool = (default(): BigDecimal) == 0.0ff

    @Test
    def testDefaultInt8(): Bool = (default(): Int8) == 0i8

    @Test
    def testDefaultInt16(): Bool = (default(): Int16) == 0i16

    @Test
    def testDefaultInt32(): Bool = (default(): Int32) == 0i32

    @Test
    def testDefaultInt64(): Bool = (default(): Int64) == 0i64

    @Test
    def testDefaultBigInt(): Bool = (default(): BigInt) == 0ii

    @Test
    def testDefaultList(): Bool = Object.isNull(default(): List[String])
}
