namespace Test/Exp/Lazy {

    // TODO: make better names a la Test.Exp.Force.flix

    @test
    def testUnitLazy01(): Lazy[Unit] & Pure =
        lazy ()

    @test
    def testBoolLazy01(): Lazy[Bool] & Pure =
        lazy true

    @test
    def testCharLazy01(): Lazy[Char] & Pure =
        lazy 'a'

    @test
    def testFloat32Lazy01(): Lazy[Float32] & Pure =
        lazy 12.34f32

    @test
    def testFloat64Lazy01(): Lazy[Float64] & Pure =
        lazy 12.34f64

    @test
    def testInt8Lazy01(): Lazy[Int8] & Pure =
        lazy 12i8

    @test
    def testInt16Lazy01(): Lazy[Int16] & Pure =
        lazy 12i16

    @test
    def testInt32Lazy01(): Lazy[Int32] & Pure =
        lazy 12i32

    @test
    def testInt64Lazy01(): Lazy[Int64] & Pure =
        lazy 12i64

    @test
    def testBigIntLazy01(): Lazy[BigInt] & Pure =
        lazy 12ii

    @test
    def testStringLazy01(): Lazy[Str] & Pure =
        lazy "string"

    @test
    def testArrayOfIntLazy01(): Lazy[Array[Int]] & Impure =
        let array = [1, 2];
        lazy array

    @test
    def testArrayOfStringLazy01(): Lazy[Array[Str]] & Impure =
        let array = ["str", "str2"];
        lazy array

    @test
    def testTupleLazy01(): Lazy[(Str, Str)] & Pure =
        lazy ("str", "str2")

    pub enum Suit {
        case Hearts, Diamonds, Clubs, Spades
    }

    pub enum Rank {
        case N(Int), Jack, Queen, King, Ace
    }

    pub enum Card {
        case Card(Suit, Rank)
    }

    @test
    def testNestedLazy01(): Lazy[Card] & Pure =
        lazy Card(Hearts, N(4))

    pub enum Poly[a, b] {
        case None,
        case This(a)
        case That(b)
        case Both(a, b)
    }

    @test
    def testPolyLazy01(): Lazy[Poly[Int, Str]] & Pure =
        lazy This(123)

    @test
    def testPolyLazy02(): Lazy[Poly[Str, Str]] & Pure =
        lazy That("str")

    @test
    def testPolyLazy03(): Lazy[Poly[Str, Int]] & Pure =
        lazy Both("str", 123)
}