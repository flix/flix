mod Test.Exp.Tuple {

    @CompileTest
    def testUnitTuple01(): Unit = ()

    @CompileTest
    def testBoolTuple01(): (Bool, Bool) = (true, false)

    @CompileTest
    def testBoolTuple02(): (Bool, Bool, Bool) = (true, false, true)

    @CompileTest
    def testCharTuple01(): (Char, Char) = ('a', 'z')

    @CompileTest
    def testCharTuple02(): (Char, Char, Char) = ('a', 'k', 'z')

    @CompileTest
    def testFloat32Tuple01(): (Float32, Float32) = (123.456f32, 456.789f32)

    @CompileTest
    def testFloat32Tuple02(): (Float32, Float32, Float32) = (123.456f32, 456.789f32, 789.123f32)

    @CompileTest
    def testFloat64Tuple01(): (Float64, Float64) = (123.456f64, 456.789f64)

    @CompileTest
    def testFloat64Tuple02(): (Float64, Float64, Float64) = (123.456f64, 456.789f64, 789.123f64)

    @CompileTest
    def testBigDecimalTuple01(): (BigDecimal, BigDecimal) = (123.456ff, 456.789ff)

    @CompileTest
    def testBigDecimalTuple02(): (BigDecimal, BigDecimal, BigDecimal) = (123.456ff, 456.789ff, 789.123ff)

    @CompileTest
    def testInt8Tuple01(): (Int8, Int8) = (1i8, 2i8)

    @CompileTest
    def testInt8Tuple02(): (Int8, Int8, Int8) = (1i8, 2i8, 3i8)

    @CompileTest
    def testInt16Tuple01(): (Int16, Int16) = (1i16, 2i16)

    @CompileTest
    def testInt16Tuple02(): (Int16, Int16, Int16) = (1i16, 2i16, 3i16)

    @CompileTest
    def testInt32Tuple01(): (Int32, Int32) = (1i32, 2i32)

    @CompileTest
    def testInt32Tuple02(): (Int32, Int32, Int32) = (1i32, 2i32, 3i32)

    @CompileTest
    def testInt64Tuple01(): (Int64, Int64) = (1i64, 2i64)

    @CompileTest
    def testInt64Tuple02(): (Int64, Int64, Int64) = (1i64, 2i64, 3i64)

    @CompileTest
    def testBigIntTuple01(): (BigInt, BigInt) = (1ii, 2ii)

    @CompileTest
    def testBigIntTuple02(): (BigInt, BigInt, BigInt) = (1ii, 2ii, 3ii)

    @CompileTest
    def testStringTuple01(): (String, String) = ("abc", "def")

    @CompileTest
    def testStringTuple02(): (String, String, String) = ("abc", "def", "geh")

    @CompileTest
    def testRecordTuple01(): ({x = Int32}, {x = Int32}) = ({x = 1}, {x = 2})

    @CompileTest
    def testRecordTuple02(): ({x = Int32}, {x = Int32}, {x = Int32}) = ({x = 1}, {x = 2}, {x = 3})

    @CompileTest
    def testTupleTuple01(): (Int32, (Int32, Int32)) = (1, (2, 3))

    @CompileTest
    def testTupleTuple02(): ((Int32, Int32), Int32) = ((1, 2), 3)

    @CompileTest
    def testTupleTuple03(): ((Int32, Int32), (Int32, Int32)) = ((1, 2), (3, 4))

    @CompileTest
    def testTupleTuple04(): (Int32, (Int32, (Int32, Int32))) = (1, (2, (3, 4)))

    @CompileTest
    def testTupleTuple05(): (((Int32, Int32), Int32), Int32) = (((1, 2), 3), 4)

    @CompileTest
    def testTupleTuple06(): ((Int32, Int32), (Int32, Int32), (Int32, Int32)) = ((1, 2), (3, 4), (5, 6))

    @CompileTest
    def testOptionTuple01(): (Option[Int32], Option[Int32]) = (None, None)

    @CompileTest
    def testOptionTuple02(): (Option[Int32], Option[Int32]) = (None, Some(2))

    @CompileTest
    def testOptionTuple03(): (Option[Int32], Option[Int32]) = (Some(1), None)

    @CompileTest
    def testOptionTuple04(): (Option[Int32], Option[Int32]) = (Some(1), Some(2))

    @CompileTest
    def testOptionTuple05(): Option[(Int32, Int32)] = None

    @CompileTest
    def testOptionTuple06(): Option[(Int32, Int32)] = Some((1, 2))

    @CompileTest
    def testResultTuple01(): (Result[Int32, Int32], Result[Int32, Int32]) = (Ok(1), Ok(2))

    @CompileTest
    def testResultTuple02(): (Result[Int32, Int32], Result[Int32, Int32]) = (Ok(1), Err(2))

    @CompileTest
    def testResultTuple03(): (Result[Int32, Int32], Result[Int32, Int32]) = (Err(1), Ok(2))

    @CompileTest
    def testResultTuple04(): (Result[Int32, Int32], Result[Int32, Int32]) = (Err(1), Err(2))

    @CompileTest
    def testResultTuple05(): Result[(Int32, Int32), Int32] = Ok(1)

    @CompileTest
    def testResultTuple06(): Result[(Int32, Int32), Int32] = Err((1, 2))

    @CompileTest
    def testListTuple01(): (List[Int32], List[Int32]) = (Nil, Nil)

    @CompileTest
    def testListTuple02(): (List[Int32], List[Int32]) = (1 :: Nil, 2 :: Nil)

    @CompileTest
    def testListTuple03(): List[(Int32, Int32)] = Nil

    @CompileTest
    def testListTuple04(): List[(Int32, Int32)] = (1, 2) :: Nil

    @CompileTest
    def testListTuple05(): List[(Int32, Int32)] = (1, 2) :: (3, 4) :: Nil

    @CompileTest
    def testNamedTuple01(): {a = Int32} = (a = 123)

    @CompileTest
    def testNamedTuple02(): ({a = Int32}, {b = Char}) = (a = 123, b = 'x')

}
