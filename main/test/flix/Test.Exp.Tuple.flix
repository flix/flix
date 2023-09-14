mod Test.Exp.Tuple {

    @Test
    def testUnitTuple01(): Unit = ()

    @Test
    def testBoolTuple01(): (Bool, Bool) = (true, false)

    @Test
    def testBoolTuple02(): (Bool, Bool, Bool) = (true, false, true)

    @Test
    def testCharTuple01(): (Char, Char) = ('a', 'z')

    @Test
    def testCharTuple02(): (Char, Char, Char) = ('a', 'k', 'z')

    @Test
    def testFloat32Tuple01(): (Float32, Float32) = (123.456f32, 456.789f32)

    @Test
    def testFloat32Tuple02(): (Float32, Float32, Float32) = (123.456f32, 456.789f32, 789.123f32)

    @Test
    def testFloat64Tuple01(): (Float64, Float64) = (123.456f64, 456.789f64)

    @Test
    def testFloat64Tuple02(): (Float64, Float64, Float64) = (123.456f64, 456.789f64, 789.123f64)

    @Test
    def testBigDecimalTuple01(): (BigDecimal, BigDecimal) = (123.456ff, 456.789ff)

    @Test
    def testBigDecimalTuple02(): (BigDecimal, BigDecimal, BigDecimal) = (123.456ff, 456.789ff, 789.123ff)

    @Test
    def testInt8Tuple01(): (Int8, Int8) = (1i8, 2i8)

    @Test
    def testInt8Tuple02(): (Int8, Int8, Int8) = (1i8, 2i8, 3i8)

    @Test
    def testInt16Tuple01(): (Int16, Int16) = (1i16, 2i16)

    @Test
    def testInt16Tuple02(): (Int16, Int16, Int16) = (1i16, 2i16, 3i16)

    @Test
    def testInt32Tuple01(): (Int32, Int32) = (1i32, 2i32)

    @Test
    def testInt32Tuple02(): (Int32, Int32, Int32) = (1i32, 2i32, 3i32)

    @Test
    def testInt64Tuple01(): (Int64, Int64) = (1i64, 2i64)

    @Test
    def testInt64Tuple02(): (Int64, Int64, Int64) = (1i64, 2i64, 3i64)

    @Test
    def testBigIntTuple01(): (BigInt, BigInt) = (1ii, 2ii)

    @Test
    def testBigIntTuple02(): (BigInt, BigInt, BigInt) = (1ii, 2ii, 3ii)

    @Test
    def testStringTuple01(): (String, String) = ("abc", "def")

    @Test
    def testStringTuple02(): (String, String, String) = ("abc", "def", "geh")

    @Test
    def testRecordTuple01(): ({x = Int32}, {x = Int32}) = ({x = 1}, {x = 2})

    @Test
    def testRecordTuple02(): ({x = Int32}, {x = Int32}, {x = Int32}) = ({x = 1}, {x = 2}, {x = 3})

    @Test
    def testTupleTuple01(): (Int32, (Int32, Int32)) = (1, (2, 3))

    @Test
    def testTupleTuple02(): ((Int32, Int32), Int32) = ((1, 2), 3)

    @Test
    def testTupleTuple03(): ((Int32, Int32), (Int32, Int32)) = ((1, 2), (3, 4))

    @Test
    def testTupleTuple04(): (Int32, (Int32, (Int32, Int32))) = (1, (2, (3, 4)))

    @Test
    def testTupleTuple05(): (((Int32, Int32), Int32), Int32) = (((1, 2), 3), 4)

    @Test
    def testTupleTuple06(): ((Int32, Int32), (Int32, Int32), (Int32, Int32)) = ((1, 2), (3, 4), (5, 6))

    @Test
    def testOptionTuple01(): (Option[Int32], Option[Int32]) = (None, None)

    @Test
    def testOptionTuple02(): (Option[Int32], Option[Int32]) = (None, Some(2))

    @Test
    def testOptionTuple03(): (Option[Int32], Option[Int32]) = (Some(1), None)

    @Test
    def testOptionTuple04(): (Option[Int32], Option[Int32]) = (Some(1), Some(2))

    @Test
    def testOptionTuple05(): Option[(Int32, Int32)] = None

    @Test
    def testOptionTuple06(): Option[(Int32, Int32)] = Some((1, 2))

    @Test
    def testResultTuple01(): (Result[Int32, Int32], Result[Int32, Int32]) = (Ok(1), Ok(2))

    @Test
    def testResultTuple02(): (Result[Int32, Int32], Result[Int32, Int32]) = (Ok(1), Err(2))

    @Test
    def testResultTuple03(): (Result[Int32, Int32], Result[Int32, Int32]) = (Err(1), Ok(2))

    @Test
    def testResultTuple04(): (Result[Int32, Int32], Result[Int32, Int32]) = (Err(1), Err(2))

    @Test
    def testResultTuple05(): Result[(Int32, Int32), Int32] = Ok(1)

    @Test
    def testResultTuple06(): Result[(Int32, Int32), Int32] = Err((1, 2))

    @Test
    def testListTuple01(): (List[Int32], List[Int32]) = (Nil, Nil)

    @Test
    def testListTuple02(): (List[Int32], List[Int32]) = (1 :: Nil, 2 :: Nil)

    @Test
    def testListTuple03(): List[(Int32, Int32)] = Nil

    @Test
    def testListTuple04(): List[(Int32, Int32)] = (1, 2) :: Nil

    @Test
    def testListTuple05(): List[(Int32, Int32)] = (1, 2) :: (3, 4) :: Nil

    @Test
    def testNamedTuple01(): {a = Int32} = (a = 123)

    @Test
    def testNamedTuple02(): ({a = Int32}, {b = Char}) = (a = 123, b = 'x')

}
