namespace Test/Exp/Tag {

    enum A {
        case OfUnit(Unit)
        case OfBool(Bool)
        case OfChar(Char)
        case OfFloat32(Float32)
        case OfFloat64(Float64)
        case OfInt8(Int8)
        case OfInt16(Int16)
        case OfInt32(Int32)
        case OfInt64(Int64)
        case OfBigInt(BigInt)
        case OfString(String)
        case OfArrayOfInt(Array[Int])
        case OfArrayOfString(Array[String])
        case OfTuple(Bool, Char, String)
    }

    @test
    def testUnitTag01(): A = OfUnit(())

    @test
    def testUnitTag02(): A = A.OfUnit(())

    @test
    def testUnitTag03(): A = Test/Exp/Tag/A.OfUnit(())


    @test
    def testBoolTag01(): A = OfBool(true)

    @test
    def testBoolTag02(): A = A.OfBool(true)

    @test
    def testBoolTag03(): A = Test/Exp/Tag/A.OfBool(true)


    @test
    def testCharTag01(): A = OfChar('a')

    @test
    def testCharTag02(): A = A.OfChar('a')

    @test
    def testCharTag03(): A = Test/Exp/Tag/A.OfChar('a')


    @test
    def testFloat32Tag01(): A = OfFloat32(21.42f32)

    @test
    def testFloat32Tag02(): A = A.OfFloat32(21.42f32)

    @test
    def testFloat32Tag03(): A = Test/Exp/Tag/A.OfFloat32(21.42f32)


    @test
    def testFloat64Tag01(): A = OfFloat64(21.42f64)

    @test
    def testFloat64Tag02(): A = A.OfFloat64(21.42f64)

    @test
    def testFloat64Tag03(): A = Test/Exp/Tag/A.OfFloat64(21.42f64)


    @test
    def testInt8Tag01(): A = OfInt8(42i8)

    @test
    def testInt8Tag02(): A = A.OfInt8(42i8)

    @test
    def testInt8Tag03(): A = Test/Exp/Tag/A.OfInt8(42i8)


    @test
    def testInt16Tag01(): A = OfInt16(42i16)

    @test
    def testInt16Tag02(): A = A.OfInt16(42i16)

    @test
    def testInt16Tag03(): A = Test/Exp/Tag/A.OfInt16(42i16)


    @test
    def testInt32Tag01(): A = OfInt32(42i32)

    @test
    def testInt32Tag02(): A = A.OfInt32(42i32)

    @test
    def testInt32Tag03(): A = Test/Exp/Tag/A.OfInt32(42i32)


    @test
    def testInt64Tag01(): A = OfInt64(42i64)

    @test
    def testInt64Tag02(): A = A.OfInt64(42i64)

    @test
    def testInt64Tag03(): A = Test/Exp/Tag/A.OfInt64(42i64)


    @test
    def testBigIntTag01(): A = OfBigInt(42ii)

    @test
    def testBigIntTag02(): A = A.OfBigInt(42ii)

    @test
    def testBigIntTag03(): A = Test/Exp/Tag/A.OfBigInt(42ii)


    @test
    def testStringTag01(): A = OfString("Hello World!")

    @test
    def testStringTag02(): A = A.OfString("Hello World!")

    @test
    def testStringTag03(): A = Test/Exp/Tag/A.OfString("Hello World!")


    @test
    def testArrayOfIntTag01(): A & Impure = OfArrayOfInt([1, 2, 3])

    @test
    def testArrayOfIntTag02(): A & Impure = A.OfArrayOfInt([1, 2, 3])

    @test
    def testArrayOfIntTag03(): A & Impure = Test/Exp/Tag/A.OfArrayOfInt([1, 2, 3])


    @test
    def testArrayOfStringTag01(): A & Impure = OfArrayOfString(["a", "b", "c"])

    @test
    def testArrayOfStringTag02(): A & Impure = A.OfArrayOfString(["a", "b", "c"])

    @test
    def testArrayOfStringTag03(): A & Impure = Test/Exp/Tag/A.OfArrayOfString(["a", "b", "c"])


    @test
    def testTupleTag01(): A = OfTuple(true, 'a', "Hello World!")

    @test
    def testTupleTag02(): A = A.OfTuple(true, 'a', "Hello World!")

    @test
    def testTupleTag03(): A = Test/Exp/Tag/A.OfTuple(true, 'a', "Hello World!")

}
