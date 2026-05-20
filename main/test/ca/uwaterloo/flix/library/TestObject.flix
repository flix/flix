mod TestObject {

    use Assert.{assertEq, assertTrue, assertFalse}

    /////////////////////////////////////////////////////////////////////////////
    // isNull                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def isNullUnit01(): Unit \ Assert = assertFalse(Object.isNull(()))

    @Test
    def isNullUnit02(): Unit \ Assert = assertTrue(Object.isNull(unchecked_cast(null as Unit)))

    @Test
    def isNullBool01(): Unit \ Assert = assertFalse(Object.isNull(true))

    @Test
    def isNullChar01(): Unit \ Assert = assertFalse(Object.isNull('a'))

    @Test
    def isNullFloat32_01(): Unit \ Assert = assertFalse(Object.isNull(123.0f32))

    @Test
    def isNullFloat64_01(): Unit \ Assert = assertFalse(Object.isNull(123.0f64))

    @Test
    def isNullInt8_01(): Unit \ Assert = assertFalse(Object.isNull(123i8))

    @Test
    def isNullInt16_01(): Unit \ Assert = assertFalse(Object.isNull(123i16))

    @Test
    def isNullInt32_01(): Unit \ Assert = assertFalse(Object.isNull(123i32))

    @Test
    def isNullInt64_01(): Unit \ Assert = assertFalse(Object.isNull(123i64))

    @Test
    def isNullBigInt01(): Unit \ Assert = assertFalse(Object.isNull(123ii))

    @Test
    def isNullBigInt02(): Unit \ Assert = assertTrue(Object.isNull((checked_cast(null): BigInt)))

    @Test
    def isNullString01(): Unit \ Assert = assertFalse(Object.isNull("hello"))

    @Test
    def isNullString02(): Unit \ Assert = assertTrue(Object.isNull((checked_cast(null): String)))

    @Test
    def isNullArray01(): Unit \ {Assert, IO} = assertFalse(Object.isNull(Array#{} @ Static))

    @Test
    def isNullArray02(): Unit \ Assert = region rc {
        let _ = rc;
        assertTrue(Object.isNull((checked_cast(null): Array[Int32, rc])))
    }

    @Test
    def isNullOther01(): Unit \ Assert = assertFalse(Object.isNull(1 :: 2 :: Nil))

    @Test
    def isNullOther02(): Unit \ Assert = assertTrue(Object.isNull(unchecked_cast(null as List[Int32])))

    /////////////////////////////////////////////////////////////////////////////
    // toOption                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toOption01(): Unit \ Assert = assertEq(expected = None, Object.toOption((checked_cast(null): String)))

    @Test
    def toOption02(): Unit \ Assert = assertEq(expected = Some(""), Object.toOption(""))
}
