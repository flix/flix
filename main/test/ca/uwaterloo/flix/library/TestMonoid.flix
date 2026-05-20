mod TestMonoid {

    use Assert.assertEq;
    use Monoid.{empty, combine}

    /////////////////////////////////////////////////////////////////////////////
    // Unit                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unitEmpty01(): Unit \ Assert =
        assertEq(expected = (), empty())

    @Test
    def unitCombine01(): Unit \ Assert =
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
    def combineBigDecimal04(): Unit \ Assert =
        assertEq(expected = 3.0ff, combine(1.0ff, 2.0ff))

    /////////////////////////////////////////////////////////////////////////////
    // String                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def stringEmpty01(): Unit \ Assert =
        assertEq(expected = "", empty())

    @Test
    def stringCombine01(): Unit \ Assert =
        assertEq(expected = "", combine("", ""))

    @Test
    def stringCombine02(): Unit \ Assert =
        assertEq(expected = "a", combine("a", ""))

    @Test
    def stringCombine03(): Unit \ Assert =
        assertEq(expected = "b", combine("", "b"))

    @Test
    def stringCombine04(): Unit \ Assert =
        assertEq(expected = "ab", combine("a", "b"))

    /////////////////////////////////////////////////////////////////////////////
    // Option                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def optionEmpty01(): Unit \ Assert =
        assertEq(expected = None, (empty(): Option[Unit]))

    @Test
    def optionCombine01(): Unit \ Assert =
        assertEq(expected = None, (combine(None, None): Option[Unit]))

    @Test
    def optionCombine02(): Unit \ Assert =
        assertEq(expected = Some("a"), combine(Some("a"), None))

    @Test
    def optionCombine03(): Unit \ Assert =
        assertEq(expected = Some("1"), combine(None, Some("1")))

    @Test
    def optionCombine04(): Unit \ Assert =
        assertEq(expected = Some("a1"), combine(Some("a"), Some("1")))

    /////////////////////////////////////////////////////////////////////////////
    // Tuple 2                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple2Empty01(): Unit \ Assert =
        assertEq(expected = ("", ""), empty())

    @Test
    def tuple2Empty02(): Unit \ Assert =
        assertEq(expected = ("", ()), empty())

    @Test
    def tuple2Combine01(): Unit \ Assert =
        assertEq(expected = ("", ""), combine(("", ""), ("", "")))

    @Test
    def tuple2Combine02(): Unit \ Assert =
        assertEq(expected = ("a", "b"), combine(("a", "b"), ("", "")))

    @Test
    def tuple2Combine03(): Unit \ Assert =
        assertEq(expected = ("1", "2"), combine(("", ""), ("1", "2")))

    @Test
    def tuple2Combine04(): Unit \ Assert =
        assertEq(expected = ("a1", "b2"), combine(("a", "b"), ("1", "2")))

    @Test
    def tuple2Combine05(): Unit \ Assert =
        assertEq(expected = ("a1", ()), combine(("a", ()), ("1", ())))

    /////////////////////////////////////////////////////////////////////////////
    // Tuple 3                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple3Empty01(): Unit \ Assert =
        assertEq(expected = ("", "", ""), empty())

    @Test
    def tuple3Empty02(): Unit \ Assert =
        assertEq(expected = ("", (), ""), empty())

    @Test
    def tuple3Combine01(): Unit \ Assert =
        assertEq(expected = ("", "", ""), combine(("", "", ""), ("", "", "")))

    @Test
    def tuple3Combine02(): Unit \ Assert =
        assertEq(expected = ("a", "b", "c"), combine(("a", "b", "c"), ("", "", "")))

    @Test
    def tuple3Combine03(): Unit \ Assert =
        assertEq(expected = ("1", "2", "3"), combine(("", "", ""), ("1", "2", "3")))

    @Test
    def tuple3Combine04(): Unit \ Assert =
        assertEq(expected = ("a1", "b2", "c3"), combine(("a", "b", "c"), ("1", "2", "3")))

    @Test
    def tuple3Combine05(): Unit \ Assert =
        assertEq(expected = ("a1", (), "b2"), combine(("a", (), "b"), ("1", (), "2")))

    /////////////////////////////////////////////////////////////////////////////
    // Tuple 4                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple4Empty01(): Unit \ Assert =
        assertEq(expected = ("", "", "", ""), empty())

    @Test
    def tuple4Empty02(): Unit \ Assert =
        assertEq(expected = ("", (), "", ()), empty())

    @Test
    def tuple4Combine01(): Unit \ Assert =
        assertEq(expected = ("", "", "", ""), combine(("", "", "", ""), ("", "", "", "")))

    @Test
    def tuple4Combine02(): Unit \ Assert =
        assertEq(expected = ("a", "b", "c", "d"), combine(("a", "b", "c", "d"), ("", "", "", "")))

    @Test
    def tuple4Combine03(): Unit \ Assert =
        assertEq(expected = ("1", "2", "3", "4"), combine(("", "", "", ""), ("1", "2", "3", "4")))

    @Test
    def tuple4Combine04(): Unit \ Assert =
        assertEq(expected = ("a1", "b2", "c3", "d4"), combine(("a", "b", "c", "d"), ("1", "2", "3", "4")))

    @Test
    def tuple4Combine05(): Unit \ Assert =
        assertEq(expected = ("a1", (), "b2", ()), combine(("a", (), "b", ()), ("1", (), "2", ())))

    /////////////////////////////////////////////////////////////////////////////
    // Tuple 5                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple5Empty01(): Unit \ Assert =
        assertEq(expected = ("", "", "", "", ""), empty())

    @Test
    def tuple5Empty02(): Unit \ Assert =
        assertEq(expected = ("", (), "", (), ""), empty())

    @Test
    def tuple5Combine01(): Unit \ Assert =
        assertEq(expected = ("", "", "", "", ""), combine(("", "", "", "", ""), ("", "", "", "", "")))

    @Test
    def tuple5Combine02(): Unit \ Assert =
        assertEq(expected = ("a", "b", "c", "d", "e"), combine(("a", "b", "c", "d", "e"), ("", "", "", "", "")))

    @Test
    def tuple5Combine03(): Unit \ Assert =
        assertEq(expected = ("1", "2", "3", "4", "5"), combine(("", "", "", "", ""), ("1", "2", "3", "4", "5")))

    @Test
    def tuple5Combine04(): Unit \ Assert =
        assertEq(expected = ("a1", "b2", "c3", "d4", "e5"), combine(("a", "b", "c", "d", "e"), ("1", "2", "3", "4", "5")))

    @Test
    def tuple5Combine05(): Unit \ Assert =
        assertEq(expected = ("a1", (), "b2", (), "c3"), combine(("a", (), "b", (), "c"), ("1", (), "2", (), "3")))

    /////////////////////////////////////////////////////////////////////////////
    // Tuple 6                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple6Empty01(): Unit \ Assert =
        assertEq(expected = ("", "", "", "", "", ""), empty())

    @Test
    def tuple6Empty02(): Unit \ Assert =
        assertEq(expected = ("", (), "", (), "", ()), empty())

    @Test
    def tuple6Combine01(): Unit \ Assert =
        assertEq(expected = ("", "", "", "", "", ""), combine(("", "", "", "", "", ""), ("", "", "", "", "", "")))

    @Test
    def tuple6Combine02(): Unit \ Assert =
        assertEq(expected = ("a", "b", "c", "d", "e", "f"), combine(("a", "b", "c", "d", "e", "f"), ("", "", "", "", "", "")))

    @Test
    def tuple6Combine03(): Unit \ Assert =
        assertEq(expected = ("1", "2", "3", "4", "5", "6"), combine(("", "", "", "", "", ""), ("1", "2", "3", "4", "5", "6")))

    @Test
    def tuple6Combine04(): Unit \ Assert =
        assertEq(expected = ("a1", "b2", "c3", "d4", "e5", "f6"), combine(("a", "b", "c", "d", "e", "f"), ("1", "2", "3", "4", "5", "6")))

    @Test
    def tuple6Combine05(): Unit \ Assert =
        assertEq(expected = ("a1", (), "b2", (), "c3", ()), combine(("a", (), "b", (), "c", ()), ("1", (), "2", (), "3", ())))

    /////////////////////////////////////////////////////////////////////////////
    // Tuple 7                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple7Empty01(): Unit \ Assert =
        assertEq(expected = ("", "", "", "", "", "", ""), empty())

    @Test
    def tuple7Empty02(): Unit \ Assert =
        assertEq(expected = ("", (), "", (), "", (), ""), empty())

    @Test
    def tuple7Combine01(): Unit \ Assert =
        assertEq(expected = ("", "", "", "", "", "", ""), combine(("", "", "", "", "", "", ""), ("", "", "", "", "", "", "")))

    @Test
    def tuple7Combine02(): Unit \ Assert =
        assertEq(expected = ("a", "b", "c", "d", "e", "f", "g"), combine(("a", "b", "c", "d", "e", "f", "g"), ("", "", "", "", "", "", "")))

    @Test
    def tuple7Combine03(): Unit \ Assert =
        assertEq(expected = ("1", "2", "3", "4", "5", "6", "7"), combine(("", "", "", "", "", "", ""), ("1", "2", "3", "4", "5", "6", "7")))

    @Test
    def tuple7Combine04(): Unit \ Assert =
        assertEq(expected = ("a1", "b2", "c3", "d4", "e5", "f6", "g7"), combine(("a", "b", "c", "d", "e", "f", "g"), ("1", "2", "3", "4", "5", "6", "7")))

    @Test
    def tuple7Combine05(): Unit \ Assert =
        assertEq(expected = ("a1", (), "b2", (), "c3", (), "d4"), combine(("a", (), "b", (), "c", (), "d"), ("1", (), "2", (), "3", (), "4")))

    /////////////////////////////////////////////////////////////////////////////
    // Tuple 8                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple8Empty01(): Unit \ Assert =
        assertEq(expected = ("", "", "", "", "", "", "", ""), empty())

    @Test
    def tuple8Empty02(): Unit \ Assert =
        assertEq(expected = ("", (), "", (), "", (), "", ()), empty())

    @Test
    def tuple8Combine01(): Unit \ Assert =
        assertEq(expected = ("", "", "", "", "", "", "", ""), combine(("", "", "", "", "", "", "", ""), ("", "", "", "", "", "", "", "")))

    @Test
    def tuple8Combine02(): Unit \ Assert =
        assertEq(expected = ("a", "b", "c", "d", "e", "f", "g", "h"), combine(("a", "b", "c", "d", "e", "f", "g", "h"), ("", "", "", "", "", "", "", "")))

    @Test
    def tuple8Combine03(): Unit \ Assert =
        assertEq(expected = ("1", "2", "3", "4", "5", "6", "7", "8"), combine(("", "", "", "", "", "", "", ""), ("1", "2", "3", "4", "5", "6", "7", "8")))

    @Test
    def tuple8Combine04(): Unit \ Assert =
        assertEq(expected = ("a1", "b2", "c3", "d4", "e5", "f6", "g7", "h8"), combine(("a", "b", "c", "d", "e", "f", "g", "h"), ("1", "2", "3", "4", "5", "6", "7", "8")))

    @Test
    def tuple8Combine05(): Unit \ Assert =
        assertEq(expected = ("a1", (), "b2", (), "c3", (), "d4", ()), combine(("a", (), "b", (), "c", (), "d", ()), ("1", (), "2", (), "3", (), "4", ())))

    /////////////////////////////////////////////////////////////////////////////
    // Tuple 9                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple9Empty01(): Unit \ Assert =
        assertEq(expected = ("", "", "", "", "", "", "", "", ""), empty())

    @Test
    def tuple9Empty02(): Unit \ Assert =
        assertEq(expected = ("", (), "", (), "", (), "", (), ""), empty())

    @Test
    def tuple9Combine01(): Unit \ Assert =
        assertEq(expected = ("", "", "", "", "", "", "", "", ""), combine(("", "", "", "", "", "", "", "", ""), ("", "", "", "", "", "", "", "", "")))

    @Test
    def tuple9Combine02(): Unit \ Assert =
        assertEq(expected = ("a", "b", "c", "d", "e", "f", "g", "h", "i"), combine(("a", "b", "c", "d", "e", "f", "g", "h", "i"), ("", "", "", "", "", "", "", "", "")))

    @Test
    def tuple9Combine03(): Unit \ Assert =
        assertEq(expected = ("1", "2", "3", "4", "5", "6", "7", "8", "9"), combine(("", "", "", "", "", "", "", "", ""), ("1", "2", "3", "4", "5", "6", "7", "8", "9")))

    @Test
    def tuple9Combine04(): Unit \ Assert =
        assertEq(expected = ("a1", "b2", "c3", "d4", "e5", "f6", "g7", "h8", "i9"), combine(("a", "b", "c", "d", "e", "f", "g", "h", "i"), ("1", "2", "3", "4", "5", "6", "7", "8", "9")))

    @Test
    def tuple9Combine05(): Unit \ Assert =
        assertEq(expected = ("a1", (), "b2", (), "c3", (), "d4", (), "e5"), combine(("a", (), "b", (), "c", (), "d", (), "e"), ("1", (), "2", (), "3", (), "4", (), "5")))

    /////////////////////////////////////////////////////////////////////////////
    // Tuple 10                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tuple10Empty01(): Unit \ Assert =
        assertEq(expected = ("", "", "", "", "", "", "", "", "", ""), empty())

    @Test
    def tuple10Empty02(): Unit \ Assert =
        assertEq(expected = ("", (), "", (), "", (), "", (), "", ()), empty())

    @Test
    def tuple10Combine01(): Unit \ Assert =
        assertEq(expected = ("", "", "", "", "", "", "", "", "", ""), combine(("", "", "", "", "", "", "", "", "", ""), ("", "", "", "", "", "", "", "", "", "")))

    @Test
    def tuple10Combine02(): Unit \ Assert =
        assertEq(expected = ("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"), combine(("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"), ("", "", "", "", "", "", "", "", "", "")))

    @Test
    def tuple10Combine03(): Unit \ Assert =
        assertEq(expected = ("1", "2", "3", "4", "5", "6", "7", "8", "9", "0"), combine(("", "", "", "", "", "", "", "", "", ""), ("1", "2", "3", "4", "5", "6", "7", "8", "9", "0")))

    @Test
    def tuple10Combine04(): Unit \ Assert =
        assertEq(expected = ("a1", "b2", "c3", "d4", "e5", "f6", "g7", "h8", "i9", "j0"), combine(("a", "b", "c", "d", "e", "f", "g", "h", "i", "j"), ("1", "2", "3", "4", "5", "6", "7", "8", "9", "0")))

    @Test
    def tuple10Combine05(): Unit \ Assert =
        assertEq(expected = ("a1", (), "b2", (), "c3", (), "d4", (), "e5", ()), combine(("a", (), "b", (), "c", (), "d", (), "e", ()), ("1", (), "2", (), "3", (), "4", (), "5", ())))

    /////////////////////////////////////////////////////////////////////////////
    // Validation                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def validationEmpty01(): Unit \ Assert =
        assertEq(expected = Validation.Success(""), (empty(): Validation[String, String]))

    @Test
    def validationCombine01(): Unit \ Assert =
        assertEq(expected = Validation.Failure(Nec.append(Nec.singleton("e1"), Nec.singleton("e2"))), (combine(Validation.Failure(Nec.singleton("e1")), Validation.Failure(Nec.singleton("e2"))): Validation[String, String]))

    @Test
    def validationCombine02(): Unit \ Assert =
        assertEq(expected = Validation.Failure(Nec.singleton("e1")), (combine(Validation.Success("a"), Validation.Failure(Nec.singleton("e1"))): Validation[String, String]))

    @Test
    def validationCombine03(): Unit \ Assert =
        assertEq(expected = Validation.Failure(Nec.singleton("e1")), (combine(Validation.Failure(Nec.singleton("e1")), Validation.Success("1")): Validation[String, String]))

    @Test
    def validationCombine04(): Unit \ Assert =
        assertEq(expected = Validation.Success("a1"), (combine(Validation.Success("a"), Validation.Success("1")): Validation[String, String]))

    /////////////////////////////////////////////////////////////////////////////
    // List                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def listEmpty01(): Unit \ Assert =
        assertEq(expected = Nil, (empty(): List[Int32]))

    @Test
    def listCombine01(): Unit \ Assert =
        assertEq(expected = Nil, (combine(Nil, Nil): List[Int32]))

    @Test
    def listCombine02(): Unit \ Assert =
        assertEq(expected = 1 :: Nil, combine(1 :: Nil, Nil))

    @Test
    def listCombine03(): Unit \ Assert =
        assertEq(expected = 2 :: Nil, combine(Nil, 2 :: Nil))

    @Test
    def listCombine04(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: Nil, combine(1 :: Nil, 2 :: Nil))

    @Test
    def listCombine05(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: 1 :: 2 :: Nil, combine(1 :: 2 :: Nil, 1 :: 2 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // Set                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def setEmpty01(): Unit \ Assert =
        assertEq(expected = Set#{}, (empty(): Set[Int32]))

    @Test
    def setCombine01(): Unit \ Assert =
        assertEq(expected = Set#{}, (combine(Set#{}, Set#{}): Set[Int32]))

    @Test
    def setCombine02(): Unit \ Assert =
        assertEq(expected = Set#{1}, combine(Set#{1}, Set#{}))

    @Test
    def setCombine03(): Unit \ Assert =
        assertEq(expected = Set#{2}, combine(Set#{}, Set#{2}))

    @Test
    def setCombine04(): Unit \ Assert =
        assertEq(expected = Set#{1, 2}, combine(Set#{1}, Set#{2}))

    @Test
    def setCombine05(): Unit \ Assert =
        assertEq(expected = Set#{1, 2}, combine(Set#{1, 2}, Set#{1, 2}))

    /////////////////////////////////////////////////////////////////////////////
    // Map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapEmpty01(): Unit \ Assert =
        assertEq(expected = Map#{}, (empty(): Map[Int32, String]))

    @Test
    def mapCombine01(): Unit \ Assert =
        assertEq(expected = Map#{}, (combine(Map#{}, Map#{}): Map[Int32, String]))

    @Test
    def mapCombine02(): Unit \ Assert =
        assertEq(expected = Map#{1 => "A"}, combine(Map#{1 => "A"}, Map#{}))

    @Test
    def mapCombine03(): Unit \ Assert =
        assertEq(expected = Map#{1 => "a"}, combine(Map#{}, Map#{1 => "a"}))

    @Test
    def mapCombine04(): Unit \ Assert =
        assertEq(expected = Map#{1 => "Aa"}, combine(Map#{1 => "A"}, Map#{1 => "a"}))

    @Test
    def mapCombine05(): Unit \ Assert =
        assertEq(expected = Map#{1 => "A", 2 => "Bb", 3 => "Cc", 4 => "d"}, combine(Map#{1 => "A", 2 => "B", 3 => "C"}, Map#{2 => "b", 3 => "c", 4 => "d"}))

}

