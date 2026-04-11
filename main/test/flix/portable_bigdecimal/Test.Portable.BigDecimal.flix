mod Test.Portable.BigDecimal {

    def assertTrue(cond: Bool, msg: String): Unit =
        if (cond) () else bug!(msg)

    @Test
    def parseAndFormatting01(): Unit =
        match BigDecimal.fromString(" 1.23e5 ") {
            case Some(v) =>
                let _ = assertTrue(ToString.toString(v) == "1.23E+5", "parseAndFormatting01 failed: canonical toString.");
                assertTrue(BigDecimal.toPlainString(v) == "123000", "parseAndFormatting01 failed: toPlainString.")
            case None => bug!("parseAndFormatting01 failed: expected parse success.")
        }

    @Test
    def equalityAndHash01(): Unit = {
        let a = 1.0ff;
        let b = 1.00ff;
        let _ = assertTrue(a == b, "equalityAndHash01 failed: expected numeric equality.");
        let _ = assertTrue(BigDecimal.numericEquals(a, b), "equalityAndHash01 failed: expected numericEquals.");
        let _ = assertTrue(Hash.hash(a) != Hash.hash(b), "equalityAndHash01 failed: hash should remain scale-sensitive.");
        let _ = assertTrue(ToString.toString(a) == "1.0", "equalityAndHash01 failed: toString(1.0).");
        assertTrue(ToString.toString(b) == "1.00", "equalityAndHash01 failed: toString(1.00).")
    }

    @Test
    def arithmetic01(): Unit = {
        let _ = assertTrue(ToString.toString(1.20ff + 3.4ff) == "4.60", "arithmetic01 failed: add scale.");
        let _ = assertTrue(ToString.toString(1.20ff - 3.4ff) == "-2.20", "arithmetic01 failed: sub scale.");
        let _ = assertTrue(ToString.toString(1.20ff * 3.4ff) == "4.080", "arithmetic01 failed: mul scale.");
        assertTrue(ToString.toString(1.20ff / 0.3ff) == "4.0", "arithmetic01 failed: exact div scale.")
    }

    @Test
    def scalePrecisionAndRounding01(): Unit = {
        let _ = assertTrue(BigDecimal.scale(1.00ff) == 2i32, "scalePrecisionAndRounding01 failed: scale.");
        let _ = assertTrue(BigDecimal.precision(1.00ff) == 3i32, "scalePrecisionAndRounding01 failed: precision(1.00).");
        let _ = assertTrue(BigDecimal.precision(0.0ff) == 1i32, "scalePrecisionAndRounding01 failed: precision(0.0).");
        let _ = assertTrue(BigDecimal.numericEquals(BigDecimal.ceil(-0.1ff), 0.0ff), "scalePrecisionAndRounding01 failed: ceil.");
        let _ = assertTrue(BigDecimal.numericEquals(BigDecimal.floor(-0.1ff), -1.0ff), "scalePrecisionAndRounding01 failed: floor.");
        assertTrue(BigDecimal.numericEquals(BigDecimal.round(2.5ff), 2.0ff), "scalePrecisionAndRounding01 failed: round half-even.")
    }

    @Test
    def integerConversion01(): Unit = {
        let _ = assertTrue(BigDecimal.toBigInt(-12.9ff) == -12ii, "integerConversion01 failed: toBigInt truncation.");
        let _ = assertTrue(BigDecimal.tryToInt32(-12.9ff) == Some(-12i32), "integerConversion01 failed: tryToInt32 truncation.");
        assertTrue(BigDecimal.tryToInt32(2147483648.0ff) == None, "integerConversion01 failed: out of range.")
    }

    @Test
    def compatConversion01(): Unit = {
        let _ = assertTrue(ToString.toString(Int32.toBigDecimal(42i32)) == "42", "compatConversion01 failed: Int32.toBigDecimal.");
        let _ = assertTrue(ToString.toString(BigInt.toBigDecimal(123ii)) == "123", "compatConversion01 failed: BigInt.toBigDecimal.");
        let _ = assertTrue(Float64.tryToBigDecimal(1.5f64) == Some(1.5ff), "compatConversion01 failed: Float64.tryToBigDecimal.");
        assertTrue(Float32.tryToBigDecimal(Float32.positiveInfinity()) == None, "compatConversion01 failed: Float32 infinity.")
    }

    pub def runAll(): Unit = {
        let _ = parseAndFormatting01();
        let _ = equalityAndHash01();
        let _ = arithmetic01();
        let _ = scalePrecisionAndRounding01();
        let _ = integerConversion01();
        let _ = compatConversion01();
        ()
    }

}
