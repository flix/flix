mod TestFromString {

    use Assert.{assertEq};
    use FromString.fromString;

    @Test
    def fromStringUnit01(): Unit \ Assert =
        assertEq(expected = Some(()), fromString("()"))

    @Test
    def fromStringUnit02(): Unit \ Assert =
        assertEq(expected = (None: Option[Unit]), fromString(")("))

    @Test
    def fromStringTrue01(): Unit \ Assert =
        assertEq(expected = Some(true), fromString("true"))

    @Test
    def fromStringTrue02(): Unit \ Assert =
        assertEq(expected = (None: Option[Bool]), fromString("fue"))

    @Test
    def fromStringFalse01(): Unit \ Assert =
        assertEq(expected = Some(false), fromString("false"))

    @Test
    def fromStringFalse02(): Unit \ Assert =
        assertEq(expected = (None: Option[Bool]), fromString("tralse"))

    @Test
    def fromStringChar01(): Unit \ Assert =
        assertEq(expected = Some('Q'), fromString("Q"))

    @Test
    def fromStringChar02(): Unit \ Assert =
        assertEq(expected = (None: Option[Char]), fromString("ABC"))

    @Test
    def fromStringFloat3201(): Unit \ Assert =
        assertEq(expected = Some(123.0f32), fromString("123.0"))

    @Test
    def fromStringFloat3202(): Unit \ Assert =
        assertEq(expected = (None: Option[Float32]), fromString("ABC"))

    @Test
    def fromStringFloat6401(): Unit \ Assert =
        assertEq(expected = Some(123.0f64), fromString("123.0"))

    @Test
    def fromStringFloat6402(): Unit \ Assert =
        assertEq(expected = (None: Option[Float64]), fromString("ABC"))

    @Test
    def fromStringBigDecimal01(): Unit \ Assert =
        assertEq(expected = Some(123.0ff), fromString("123.0"))

    @Test
    def fromStringBigDecimal02(): Unit \ Assert =
        assertEq(expected = (None: Option[BigDecimal]), fromString("ABC"))

    @Test
    def fromStringInt801(): Unit \ Assert =
        assertEq(expected = Some(123i8), fromString("123"))

    @Test
    def fromStringInt802(): Unit \ Assert =
        assertEq(expected = (None: Option[Int8]), fromString("ABC"))

    @Test
    def fromStringInt1601(): Unit \ Assert =
        assertEq(expected = Some(123i16), fromString("123"))

    @Test
    def fromStringInt1602(): Unit \ Assert =
        assertEq(expected = (None: Option[Int16]), fromString("ABC"))

    @Test
    def fromStringInt3201(): Unit \ Assert =
        assertEq(expected = Some(123i32), fromString("123"))

    @Test
    def fromStringInt3202(): Unit \ Assert =
        assertEq(expected = (None: Option[Int32]), fromString("ABC"))

    @Test
    def fromStringInt6401(): Unit \ Assert =
        assertEq(expected = Some(123i64), fromString("123"))

    @Test
    def fromStringInt6402(): Unit \ Assert =
        assertEq(expected = (None: Option[Int64]), fromString("ABC"))

    @Test
    def fromStringString01(): Unit \ Assert =
        assertEq(expected = Some("abcd"), fromString("abcd"))

    @Test
    def fromStringBigInt01(): Unit \ Assert =
        assertEq(expected = Some(123ii), fromString("123"))

    @Test
    def fromStringBigInt02(): Unit \ Assert =
        assertEq(expected = (None: Option[BigInt]), fromString("ABC"))

}
