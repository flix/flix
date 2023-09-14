mod Test.Exp.Lazy {

    @Test
    def testUnitLazy01(): Lazy[Unit] =
        lazy ()

    @Test
    def testBoolLazy01(): Lazy[Bool] =
        lazy true

    @Test
    def testBoolLazy02(): Lazy[Bool] =
        lazy false

    @Test
    def testCharLazy01(): Lazy[Char] =
        lazy 'a'

    @Test
    def testFloat32Lazy01(): Lazy[Float32] =
        lazy 12.34f32

    @Test
    def testFloat64Lazy01(): Lazy[Float64] =
        lazy 12.34f64

    @Test
    def testBigDecimalLazy01(): Lazy[BigDecimal] =
        lazy 12.34ff

    @Test
    def testInt8Lazy01(): Lazy[Int8] =
        lazy 12i8

    @Test
    def testInt16Lazy01(): Lazy[Int16] =
        lazy 12i16

    @Test
    def testInt32Lazy01(): Lazy[Int32] =
        lazy 12i32

    @Test
    def testInt64Lazy01(): Lazy[Int64] =
        lazy 12i64

    @Test
    def testBigIntLazy01(): Lazy[BigInt] =
        lazy 12ii

    @Test
    def testStringLazy01(): Lazy[String] =
        lazy "string"

    @Test
    def testArrayOfIntLazy01(): Unit = region rc {
        discard {
            let array = Array#{1, 2} @ rc;
            lazy array
        }
    }

    @Test
    def testArrayOfStringLazy01(): Unit = region rc {
        discard {
            let array = Array#{"str", "str2"} @ rc;
            lazy array
        }
    }

    @Test
    def testTupleLazy01(): Lazy[(String, String)] =
        lazy ("str", "str2")

    @Test
    def testNestedLazy01(): Lazy[Lazy[Int32]] =
        lazy (lazy 123)

    @Test
    def testNestedLazy02(): Lazy[Int32 -> Lazy[String]] =
        lazy (x -> if (x < 0) lazy "neg" else lazy "pos")

    @Test
    def testNestedLazy03(): Poly[Lazy[Int32], Lazy[String]] =
        Poly.Both(lazy 2, lazy "2")

    @Test
    def testNestedLazy04(): Poly[Lazy[Int32], Lazy[Int32]] =
        let x = lazy 15;
        Poly.Both(x, x)

    @Test
    def testNestedLazy05(): Lazy[Poly[Lazy[Int32], String]] =
        lazy (Poly.This(lazy 5))

    @Test
    def testNestedEnumLazy01(): Lazy[Card] =
        lazy Card.Card(Suit.Hearts, Rank.N(4))

    @Test
    def testPolyLazy01(): Lazy[Poly[Int32, String]] =
        lazy Poly.This(123)

    @Test
    def testPolyLazy02(): Lazy[Poly[String, String]] =
        lazy Poly.That("str")

    @Test
    def testPolyLazy03(): Lazy[Poly[String, Int32]] =
        lazy Poly.Both("str", 123)

    pub enum Suit {
        case Hearts, Diamonds, Clubs, Spades
    }

    pub enum Rank {
        case N(Int32), Jack, Queen, King, Ace
    }

    pub enum Card {
        case Card(Suit, Rank)
    }

    pub enum Poly[a, b] {
        case None,
        case This(a)
        case That(b)
        case Both(a, b)
    }
}
