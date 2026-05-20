mod TestSemiGroup {

    use Assert.{assertEq}
    use SemiGroup.{combine, combineN}

    /////////////////////////////////////////////////////////////////////////////
    // Unit                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def combineUnit01(): Unit \ Assert =
        assertEq(expected = (), combine((), ()))

    /////////////////////////////////////////////////////////////////////////////
    // Int8                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def combineInt801(): Unit \ Assert =
        assertEq(expected = 0i8, combine(0i8, 0i8))

    @Test
    def combineInt802(): Unit \ Assert =
        assertEq(expected = 1i8, combine(1i8, 0i8))

    @Test
    def combineInt803(): Unit \ Assert =
        assertEq(expected = 2i8, combine(0i8, 2i8))

    @Test
    def combineInt804(): Unit \ Assert =
        assertEq(expected = 3i8, combine(1i8, 2i8))

    /////////////////////////////////////////////////////////////////////////////
    // Int16                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def combineInt1601(): Unit \ Assert =
        assertEq(expected = 0i16, combine(0i16, 0i16))

    @Test
    def combineInt1602(): Unit \ Assert =
        assertEq(expected = 1i16, combine(1i16, 0i16))

    @Test
    def combineInt1603(): Unit \ Assert =
        assertEq(expected = 2i16, combine(0i16, 2i16))

    @Test
    def combineInt1604(): Unit \ Assert =
        assertEq(expected = 3i16, combine(1i16, 2i16))

    /////////////////////////////////////////////////////////////////////////////
    // Int32                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def combineInt3201(): Unit \ Assert =
        assertEq(expected = 0i32, combine(0i32, 0i32))

    @Test
    def combineInt3202(): Unit \ Assert =
        assertEq(expected = 1i32, combine(1i32, 0i32))

    @Test
    def combineInt3203(): Unit \ Assert =
        assertEq(expected = 2i32, combine(0i32, 2i32))

    @Test
    def combineInt3204(): Unit \ Assert =
        assertEq(expected = 3i32, combine(1i32, 2i32))

    /////////////////////////////////////////////////////////////////////////////
    // Int64                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def combineInt6401(): Unit \ Assert =
        assertEq(expected = 0i64, combine(0i64, 0i64))

    @Test
    def combineInt6402(): Unit \ Assert =
        assertEq(expected = 1i64, combine(1i64, 0i64))

    @Test
    def combineInt6403(): Unit \ Assert =
        assertEq(expected = 2i64, combine(0i64, 2i64))

    @Test
    def combineInt6404(): Unit \ Assert =
        assertEq(expected = 3i64, combine(1i64, 2i64))

    /////////////////////////////////////////////////////////////////////////////
    // BigInt                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def combineBigInt01(): Unit \ Assert =
        assertEq(expected = 0ii, combine(0ii, 0ii))

    @Test
    def combineBigInt02(): Unit \ Assert =
        assertEq(expected = 1ii, combine(1ii, 0ii))

    @Test
    def combineBigInt03(): Unit \ Assert =
        assertEq(expected = 2ii, combine(0ii, 2ii))

    @Test
    def combineBigInt04(): Unit \ Assert =
        assertEq(expected = 3ii, combine(1ii, 2ii))

    /////////////////////////////////////////////////////////////////////////////
    // Float32                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def combineFloat3201(): Unit \ Assert =
        assertEq(expected = 0.0f32, combine(0.0f32, 0.0f32))

    @Test
    def combineFloat3202(): Unit \ Assert =
        assertEq(expected = 1.0f32, combine(1.0f32, 0.0f32))

    @Test
    def combineFloat3203(): Unit \ Assert =
        assertEq(expected = 2.0f32, combine(0.0f32, 2.0f32))

    @Test
    def combineFloat3204(): Unit \ Assert =
        assertEq(expected = 3.0f32, combine(1.0f32, 2.0f32))

    /////////////////////////////////////////////////////////////////////////////
    // Float64                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def combineFloat6401(): Unit \ Assert =
        assertEq(expected = 0.0f64, combine(0.0f64, 0.0f64))

    @Test
    def combineFloat6402(): Unit \ Assert =
        assertEq(expected = 1.0f64, combine(1.0f64, 0.0f64))

    @Test
    def combineFloat6403(): Unit \ Assert =
        assertEq(expected = 2.0f64, combine(0.0f64, 2.0f64))

    @Test
    def combineFloat6404(): Unit \ Assert =
        assertEq(expected = 3.0f64, combine(1.0f64, 2.0f64))

    /////////////////////////////////////////////////////////////////////////////
    // BigDecimal                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def combineBigDecimal01(): Unit \ Assert =
        assertEq(expected = 0.0ff, combine(0.0ff, 0.0ff))

    @Test
    def combineBigDecimal02(): Unit \ Assert =
        assertEq(expected = 1.0ff, combine(1.0ff, 0.0ff))

    @Test
    def combineBigDecimal03(): Unit \ Assert =
        assertEq(expected = 2.0ff, combine(0.0ff, 2.0ff))

    @Test
    def combineBigDecimal4(): Unit \ Assert =
        assertEq(expected = 3.0ff, combine(1.0ff, 2.0ff))

    /////////////////////////////////////////////////////////////////////////////
    // String                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def combineString01(): Unit \ Assert =
        assertEq(expected = "", combine("", ""))

    @Test
    def combineString02(): Unit \ Assert =
        assertEq(expected = "a", combine("a", ""))

    @Test
    def combineString03(): Unit \ Assert =
        assertEq(expected = "b", combine("", "b"))

    @Test
    def combineString04(): Unit \ Assert =
        assertEq(expected = "ab", combine("a", "b"))

    /////////////////////////////////////////////////////////////////////////////
    // Option                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def combineOption01(): Unit \ Assert =
        assertEq(expected = (None: Option[String]), combine(None, None))

    @Test
    def combineOption02(): Unit \ Assert =
        assertEq(expected = Some("a"), combine(Some("a"), None))

    @Test
    def combineOption03(): Unit \ Assert =
        assertEq(expected = Some("b"), combine(None, Some("b")))

    @Test
    def combineOption04(): Unit \ Assert =
        assertEq(expected = Some("ab"), combine(Some("a"), Some("b")))

    /////////////////////////////////////////////////////////////////////////////
    // List                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def combineList01(): Unit \ Assert =
        assertEq(expected = (Nil: List[Int32]), combine(Nil, Nil))

    @Test
    def combineList02(): Unit \ Assert =
        assertEq(expected = 1 :: Nil, combine(1 :: Nil, Nil))

    @Test
    def combineList03(): Unit \ Assert =
        assertEq(expected = 2 :: Nil, combine(Nil, 2 :: Nil))

    @Test
    def combineList04(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: Nil, combine(1 :: Nil, 2 :: Nil))

    @Test
    def combineList05(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: 1 :: 2 :: Nil, combine(1 :: 2 :: Nil, 1 :: 2 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // Map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def combineMap01(): Unit \ Assert =
        assertEq(expected = (Map#{}: Map[Int32, String]), combine(Map#{}, Map#{}))

    @Test
    def combineMap02(): Unit \ Assert =
        assertEq(expected = Map#{1 => "A"}, combine(Map#{1 => "A"}, Map#{}))

    @Test
    def combineMap03(): Unit \ Assert =
        assertEq(expected = Map#{1 => "a"}, combine(Map#{}, Map#{1 => "a"}))

    @Test
    def combineMap04(): Unit \ Assert =
        assertEq(expected = Map#{1 => "Aa"}, combine(Map#{1 => "A"}, Map#{1 => "a"}))

    @Test
    def combineMap05(): Unit \ Assert =
        assertEq(expected = Map#{1 => "A", 2 => "Bb", 3 => "Cc", 4 => "d"}, combine(Map#{1 => "A", 2 => "B", 3 => "C"}, Map#{2 => "b", 3 => "c", 4 => "d"}))

    /////////////////////////////////////////////////////////////////////////////
    // Validation                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def combineValidation01(): Unit \ Assert =
        assertEq(expected = Validation.Failure(Nec.append(Nec.singleton("e1"), Nec.singleton("e2"))), (combine(Validation.Failure(Nec.singleton("e1")), Validation.Failure(Nec.singleton("e2"))): Validation[String, String]))

    @Test
    def combineValidation02(): Unit \ Assert =
        assertEq(expected = Validation.Failure(Nec.singleton("e1")), (combine(Validation.Success("a"), Validation.Failure(Nec.singleton("e1"))): Validation[String, String]))

    @Test
    def combineValidation03(): Unit \ Assert =
        assertEq(expected = Validation.Failure(Nec.singleton("e1")), (combine(Validation.Failure(Nec.singleton("e1")), Validation.Success("1")): Validation[String, String]))

    @Test
    def combineValidation04(): Unit \ Assert =
        assertEq(expected = Validation.Success("a1"), (combine(Validation.Success("a"), Validation.Success("1")): Validation[String, String]))

    /////////////////////////////////////////////////////////////////////////////
    // 2-Tuple                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def combine2Tuple01(): Unit \ Assert =
        assertEq(expected = ("", ""), combine(("", ""), ("", "")))

    @Test
    def combine2Tuple02(): Unit \ Assert =
        assertEq(expected = ("a", "b"), combine(("a", "b"), ("", "")))

    @Test
    def combine2Tuple03(): Unit \ Assert =
        assertEq(expected = ("1", "2"), combine(("", ""), ("1", "2")))

    @Test
    def combine2Tuple04(): Unit \ Assert =
        assertEq(expected = ("a1", "b2"), combine(("a", "b"), ("1", "2")))

    @Test
    def combine2Tuple05(): Unit \ Assert =
        assertEq(expected = ("a1", ()), combine(("a", ()), ("1", ())))

    /////////////////////////////////////////////////////////////////////////////
    // 3-Tuple                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def combine3Tuple01(): Unit \ Assert =
        assertEq(expected = ("", "", ""), combine(("", "", ""), ("", "", "")))

    @Test
    def combine3Tuple02(): Unit \ Assert =
        assertEq(expected = ("a", "b", "c"), combine(("a", "b", "c"), ("", "", "")))

    @Test
    def combine3Tuple03(): Unit \ Assert =
        assertEq(expected = ("1", "2", "3"), combine(("", "", ""), ("1", "2", "3")))

    @Test
    def combine3Tuple04(): Unit \ Assert =
        assertEq(expected = ("a1", "b2", "c3"), combine(("a", "b", "c"), ("1", "2", "3")))

    @Test
    def combine3Tuple05(): Unit \ Assert =
        assertEq(expected = ("a1", (), "b2"), combine(("a", (), "b"), ("1", (), "2")))

    /////////////////////////////////////////////////////////////////////////////
    // 4-Tuple                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def combine4Tuple01(): Unit \ Assert =
        assertEq(expected = ("", "", "", ""), combine(("", "", "", ""), ("", "", "", "")))

    @Test
    def combine4Tuple02(): Unit \ Assert =
        assertEq(expected = ("a", "b", "c", "d"), combine(("a", "b", "c", "d"), ("", "", "", "")))

    @Test
    def combine4Tuple03(): Unit \ Assert =
        assertEq(expected = ("1", "2", "3", "4"), combine(("", "", "", ""), ("1", "2", "3", "4")))

    @Test
    def combine4Tuple04(): Unit \ Assert =
        assertEq(expected = ("a1", "b2", "c3", "d4"), combine(("a", "b", "c", "d"), ("1", "2", "3", "4")))

    @Test
    def combine4Tuple05(): Unit \ Assert =
        assertEq(expected = ("a1", (), "b2", ()), combine(("a", (), "b", ()), ("1", (), "2", ())))

    /////////////////////////////////////////////////////////////////////////////
    // 5-Tuple                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def combine5Tuple01(): Unit \ Assert =
        assertEq(expected = ("", "", "", "", ""), combine(("", "", "", "", ""), ("", "", "", "", "")))

    @Test
    def combine5Tuple02(): Unit \ Assert =
        assertEq(expected = ("a", "b", "c", "d", "e"), combine(("a", "b", "c", "d", "e"), ("", "", "", "", "")))

    @Test
    def combine5Tuple03(): Unit \ Assert =
        assertEq(expected = ("1", "2", "3", "4", "5"), combine(("", "", "", "", ""), ("1", "2", "3", "4", "5")))

    @Test
    def combine5Tuple04(): Unit \ Assert =
        assertEq(expected = ("a1", "b2", "c3", "d4", "e5"), combine(("a", "b", "c", "d", "e"), ("1", "2", "3", "4", "5")))

    @Test
    def combine5Tuple05(): Unit \ Assert =
        assertEq(expected = ("a1", (), "b2", (), "c3"), combine(("a", (), "b", (), "c"), ("1", (), "2", (), "3")))

    /////////////////////////////////////////////////////////////////////////////
    // 6-Tuple                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def combine6Tuple01(): Unit \ Assert =
        assertEq(expected = ("", "", "", "", "", ""), combine(("", "", "", "", "", ""), ("", "", "", "", "", "")))

    @Test
    def combine6Tuple02(): Unit \ Assert =
        assertEq(expected = ("a", "b", "c", "d", "e", "f"), combine(("a", "b", "c", "d", "e", "f"), ("", "", "", "", "", "")))

    @Test
    def combine6Tuple03(): Unit \ Assert =
        assertEq(expected = ("1", "2", "3", "4", "5", "6"), combine(("", "", "", "", "", ""), ("1", "2", "3", "4", "5", "6")))

    @Test
    def combine6Tuple04(): Unit \ Assert =
        assertEq(expected = ("a1", "b2", "c3", "d4", "e5", "f6"), combine(("a", "b", "c", "d", "e", "f"), ("1", "2", "3", "4", "5", "6")))

    @Test
    def combine6Tuple05(): Unit \ Assert =
        assertEq(expected = ("a1", (), "b2", (), "c3", ()), combine(("a", (), "b", (), "c", ()), ("1", (), "2", (), "3", ())))

    /////////////////////////////////////////////////////////////////////////////
    // 7-Tuple                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def combine7Tuple01(): Unit \ Assert =
        assertEq(expected = ("", "", "", "", "", "", ""), combine(("", "", "", "", "", "", ""), ("", "", "", "", "", "", "")))

    @Test
    def combine7Tuple02(): Unit \ Assert =
        assertEq(expected = ("a", "b", "c", "d", "e", "f", "g"), combine(("a", "b", "c", "d", "e", "f", "g"), ("", "", "", "", "", "", "")))

    @Test
    def combine7Tuple03(): Unit \ Assert =
        assertEq(expected = ("1", "2", "3", "4", "5", "6", "7"), combine(("", "", "", "", "", "", ""), ("1", "2", "3", "4", "5", "6", "7")))

    @Test
    def combine7Tuple04(): Unit \ Assert =
        assertEq(expected = ("a1", "b2", "c3", "d4", "e5", "f6", "g7"), combine(("a", "b", "c", "d", "e", "f", "g"), ("1", "2", "3", "4", "5", "6", "7")))

    @Test
    def combine7Tuple05(): Unit \ Assert =
        assertEq(expected = ("a1", (), "b2", (), "c3", (), "d4"), combine(("a", (), "b", (), "c", (), "d"), ("1", (), "2", (), "3", (), "4")))

    /////////////////////////////////////////////////////////////////////////////
    // 8-Tuple                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def combine8Tuple01(): Unit \ Assert =
        assertEq(expected = ("", "", "", "", "", "", "", ""), combine(("", "", "", "", "", "", "", ""), ("", "", "", "", "", "", "", "")))

    @Test
    def combine8Tuple02(): Unit \ Assert =
        assertEq(expected = ("a", "b", "c", "d", "e", "f", "g", "h"), combine(("a", "b", "c", "d", "e", "f", "g", "h"), ("", "", "", "", "", "", "", "")))

    @Test
    def combine8Tuple03(): Unit \ Assert =
        assertEq(expected = ("1", "2", "3", "4", "5", "6", "7", "8"), combine(("", "", "", "", "", "", "", ""), ("1", "2", "3", "4", "5", "6", "7", "8")))

    @Test
    def combine8Tuple04(): Unit \ Assert =
        assertEq(expected = ("a1", "b2", "c3", "d4", "e5", "f6", "g7", "h8"), combine(("a", "b", "c", "d", "e", "f", "g", "h"), ("1", "2", "3", "4", "5", "6", "7", "8")))

    @Test
    def combine8Tuple05(): Unit \ Assert =
        assertEq(expected = ("a1", (), "b2", (), "c3", (), "d4", ()), combine(("a", (), "b", (), "c", (), "d", ()), ("1", (), "2", (), "3", (), "4", ())))

    /////////////////////////////////////////////////////////////////////////////
    // 9-Tuple                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def combine9Tuple01(): Unit \ Assert =
        assertEq(expected = ("", "", "", "", "", "", "", "", ""), combine(("", "", "", "", "", "", "", "", ""), ("", "", "", "", "", "", "", "", "")))

    @Test
    def combine9Tuple02(): Unit \ Assert =
        assertEq(expected = ("a", "b", "c", "d", "e", "f", "g", "h", "i"), combine(("a", "b", "c", "d", "e", "f", "g", "h", "i"), ("", "", "", "", "", "", "", "", "")))

    @Test
    def combine9Tuple03(): Unit \ Assert =
        assertEq(expected = ("1", "2", "3", "4", "5", "6", "7", "8", "9"), combine(("", "", "", "", "", "", "", "", ""), ("1", "2", "3", "4", "5", "6", "7", "8", "9")))

    @Test
    def combine9Tuple04(): Unit \ Assert =
        assertEq(expected = ("a1", "b2", "c3", "d4", "e5", "f6", "g7", "h8", "i9"), combine(("a", "b", "c", "d", "e", "f", "g", "h", "i"), ("1", "2", "3", "4", "5", "6", "7", "8", "9")))

    @Test
    def combine9Tuple05(): Unit \ Assert =
        assertEq(expected = ("a1", (), "b2", (), "c3", (), "d4", (), "e5"), combine(("a", (), "b", (), "c", (), "d", (), "e"), ("1", (), "2", (), "3", (), "4", (), "5")))

    /////////////////////////////////////////////////////////////////////////////
    // 10-Tuple                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def combine10Tuple01(): Unit \ Assert =
        assertEq(expected = ("", "", "", "", "", "", "", "", "", ""), combine(("", "", "", "", "", "", "", "", "", ""), ("", "", "", "", "", "", "", "", "", "")))

    @Test
    def combine10Tuple02(): Unit \ Assert =
        assertEq(expected = ("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"), combine(("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"), ("", "", "", "", "", "", "", "", "", "")))

    @Test
    def combine10Tuple03(): Unit \ Assert =
        assertEq(expected = ("1", "2", "3", "4", "5", "6", "7", "8", "9", "0"), combine(("", "", "", "", "", "", "", "", "", ""), ("1", "2", "3", "4", "5", "6", "7", "8", "9", "0")))

    @Test
    def combine10Tuple04(): Unit \ Assert =
        assertEq(expected = ("a1", "b2", "c3", "d4", "e5", "f6", "g7", "h8", "i9", "j0"), combine(("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"), ("1", "2", "3", "4", "5", "6", "7", "8", "9", "0")))

    @Test
    def combine10Tuple05(): Unit \ Assert =
        assertEq(expected = ("a1", (), "b2", (), "c3", (), "d4", (), "e5", ()), combine(("a", (), "b", (), "c", (), "d", (), "e", ()), ("1", (), "2", (), "3", (), "4", (), "5", ())))

    /////////////////////////////////////////////////////////////////////////////
    // combineN                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def combineN01(): Unit \ Assert =
        assertEq(expected = (), combineN((), 0))

    @Test
    def combineN02(): Unit \ Assert =
        assertEq(expected = (), combineN((), 3))

    @Test
    def combineN03(): Unit \ Assert =
        assertEq(expected = "", combineN("", -1))

    @Test
    def combineN04(): Unit \ Assert =
        assertEq(expected = "", combineN("", 5))

    @Test
    def combineN05(): Unit \ Assert =
        assertEq(expected = "aaa", combineN("a", 3))

    @Test
    def combineN06(): Unit \ Assert =
        assertEq(expected = Some("ababababab"), combineN(Some("ab"), 5))

    @Test
    def combineN07(): Unit \ Assert =
        assertEq(expected = 1 :: 1 :: Nil, combineN(1 :: Nil, 2))

    @Test
    def combineN08(): Unit \ Assert =
        assertEq(expected = Map#{1 => "aaaa"}, combineN(Map#{1 => "a"}, 4))

    /////////////////////////////////////////////////////////////////////////////
    // associative law                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def associativeTest01(): Unit \ Assert =
        assertEq(expected = combine("a", combine("b", "c")), combine(combine("a", "b"), "c"))

    /////////////////////////////////////////////////////////////////////////////
    // Syntax                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def operator01(): Unit \ Assert =
        assertEq(expected = 2, (1 ++ 1))

    @Test
    def operator02(): Unit \ Assert =
        assertEq(expected = "abcdef", ("abc" ++ "def"))

    @Test
    def operator03(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: 3 :: 4 :: Nil, ((1 :: 2 :: Nil) ++ (3 :: 4 :: Nil)))

}