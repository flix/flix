mod Test.Exp.Tag {

    enum A {
        case OfUnit(Unit)
        case OfBool(Bool)
        case OfChar(Char)
        case OfFloat32(Float32)
        case OfFloat64(Float64)
        case OfBigDecimal(BigDecimal)
        case OfInt8(Int8)
        case OfInt16(Int16)
        case OfInt32(Int32)
        case OfInt64(Int64)
        case OfBigInt(BigInt)
        case OfString(String)
        case OfArrayOfInt(Array[Int32, Static])
        case OfArrayOfString(Array[String, Static])
        case OfTuple(Bool, Char, String)
        case OfRecord({a = Int32}, {b = Char})
        case OfRecordOfTuple({a = ({b = Int32}, {c = Char})})
    }

    @Test
    def testUnitTag01(): A = A.OfUnit(())

    @Test
    def testUnitTag02(): A = Test.Exp.Tag.A.OfUnit(())


    @Test
    def testBoolTag01(): A = A.OfBool(true)

    @Test
    def testBoolTag02(): A = Test.Exp.Tag.A.OfBool(true)


    @Test
    def testCharTag01(): A = A.OfChar('a')

    @Test
    def testCharTag02(): A = Test.Exp.Tag.A.OfChar('a')


    @Test
    def testFloat32Tag01(): A = A.OfFloat32(21.42f32)

    @Test
    def testFloat32Tag02(): A = Test.Exp.Tag.A.OfFloat32(21.42f32)


    @Test
    def testFloat64Tag01(): A = A.OfFloat64(21.42f64)

    @Test
    def testFloat64Tag02(): A = Test.Exp.Tag.A.OfFloat64(21.42f64)


    @Test
    def testBigDecimalTag01(): A = A.OfBigDecimal(21.42ff)

    @Test
    def testBigDecimalTag02(): A = Test.Exp.Tag.A.OfBigDecimal(21.42ff)


    @Test
    def testInt8Tag01(): A = A.OfInt8(42i8)

    @Test
    def testInt8Tag02(): A = Test.Exp.Tag.A.OfInt8(42i8)


    @Test
    def testInt16Tag01(): A = A.OfInt16(42i16)

    @Test
    def testInt16Tag02(): A = Test.Exp.Tag.A.OfInt16(42i16)


    @Test
    def testInt32Tag01(): A = A.OfInt32(42i32)

    @Test
    def testInt32Tag02(): A = Test.Exp.Tag.A.OfInt32(42i32)


    @Test
    def testInt64Tag01(): A = A.OfInt64(42i64)

    @Test
    def testInt64Tag02(): A = Test.Exp.Tag.A.OfInt64(42i64)


    @Test
    def testBigIntTag01(): A = A.OfBigInt(42ii)

    @Test
    def testBigIntTag02(): A = Test.Exp.Tag.A.OfBigInt(42ii)


    @Test
    def testStringTag01(): A = A.OfString("Hello World!")

    @Test
    def testStringTag02(): A = Test.Exp.Tag.A.OfString("Hello World!")


    @Test
    def testArrayOfIntTag01(): A \ IO = A.OfArrayOfInt(Array#{1, 2, 3} @ Static)

    @Test
    def testArrayOfIntTag02(): A \ IO = Test.Exp.Tag.A.OfArrayOfInt(Array#{1, 2, 3} @ Static)


    @Test
    def testArrayOfStringTag01(): A \ IO = A.OfArrayOfString(Array#{"a", "b", "c"} @ Static)

    @Test
    def testArrayOfStringTag02(): A \ IO = Test.Exp.Tag.A.OfArrayOfString(Array#{"a", "b", "c"} @ Static)


    @Test
    def testTupleTag01(): A = A.OfTuple(true, 'a', "Hello World!")

    @Test
    def testTupleTag02(): A = Test.Exp.Tag.A.OfTuple(true, 'a', "Hello World!")

    @Test
    def testRecordTag01(): A = A.OfRecord(a = 123, b = 'x')

    @Test
    def testRecordOfTupleTag01(): A = A.OfRecordOfTuple(a = (b = 123, c = 'x'))

    pub enum Suit {
        case Hearts, Diamonds, Clubs, Spades
    }

    pub enum Rank {
        case N(Int32), Jack, Queen, King, Ace
    }

    pub enum Card {
        case Card(Suit, Rank)
    }

    @Test
    def testNestedTag01(): Card = Card.Card(Suit.Hearts, Rank.N(5))

    @Test
    def testNestedTag02(): Card = Test.Exp.Tag.Card.Card(Test.Exp.Tag.Suit.Hearts, Test.Exp.Tag.Rank.N(5))

    pub enum Poly[a, b] {
        case None,
        case This(a)
        case That(b)
        case Both(a, b)
    }

    @Test
    def testPolyTag01(): Poly[Int32, String] = Poly.None

    @Test
    def testPolyTag02(): Poly[Int32, String] = Test.Exp.Tag.Poly.None

    @Test
    def testPolyTag03(): Poly[Int32, String] = Poly.This(123)

    @Test
    def testPolyTag04(): Poly[Int32, String] = Test.Exp.Tag.Poly.This(123)

    @Test
    def testPolyTag05(): Poly[Int32, String] = Poly.That("Hello World!")

    @Test
    def testPolyTag06(): Poly[Int32, String] = Test.Exp.Tag.Poly.That("Hello World!")

    @Test
    def testPolyTag07(): Poly[Int32, String] = Poly.Both(123, "Hello World!")

    @Test
    def testPolyTag08(): Poly[Int32, String] = Test.Exp.Tag.Poly.Both(123, "Hello World!")

    @Test
    def testTopLevelTag01(): Option[Int32] = Some(1)

    @Test
    def testTopLevelTag02(): Option[Int32] = Option.Some(1)

    // Local version of List to make sure that this is used instead of top level
    enum List {
        case Cons(String)
    }

    @Test
    def testTopLevelTag03(): List = List.Cons("Foo")
}
