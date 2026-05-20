mod TestReflect {

    use Assert.{assertEq, assertTrue};

    @Test
    def defaultUnit01(): Unit \ Assert =
        assertEq(expected = (), (Reflect.default(): Unit))

    @Test
    def defaultBool01(): Unit \ Assert =
        assertEq(expected = false, (Reflect.default(): Bool))

    @Test
    def defaultChar01(): Unit \ Assert =
        assertEq(expected = '0', (Reflect.default(): Char))

    @Test
    def defaultFloat3201(): Unit \ Assert =
        assertEq(expected = 0.0f32, (Reflect.default(): Float32))

    @Test
    def defaultFloat6401(): Unit \ Assert =
        assertEq(expected = 0.0f64, (Reflect.default(): Float64))

    @Test
    def defaultBigDecimal01(): Unit \ Assert =
        assertTrue(Object.isNull((Reflect.default(): BigDecimal)))

    @Test
    def defaultInt801(): Unit \ Assert =
        assertEq(expected = 0i8, (Reflect.default(): Int8))

    @Test
    def defaultInt1601(): Unit \ Assert =
        assertEq(expected = 0i16, (Reflect.default(): Int16))

    @Test
    def defaultInt3201(): Unit \ Assert =
        assertEq(expected = 0i32, (Reflect.default(): Int32))

    @Test
    def defaultInt6401(): Unit \ Assert =
        assertEq(expected = 0i64, (Reflect.default(): Int64))

    @Test
    def defaultBigInt01(): Unit \ Assert =
        assertTrue(Object.isNull((Reflect.default(): BigInt)))

    @Test
    def defaultList01(): Unit \ Assert =
        assertTrue(Object.isNull((Reflect.default(): List[String])))

}
