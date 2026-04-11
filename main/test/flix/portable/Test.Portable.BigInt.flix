mod Test.Portable.BigInt {

    def assertTrue(cond: Bool, msg: String): Unit =
        if (cond) () else bug!(msg)

    @Test
    def parseAndRoundtrip01(): Unit = {
        let _ = match BigInt.fromString("  +123  ") {
            case Some(v) =>
                let _ = assertTrue(v == 123ii, "parseAndRoundtrip01 failed: expected trimmed parse.");
                ()
            case None => bug!("parseAndRoundtrip01 failed: expected parse success.")
        };

        let _ = match BigInt.fromString("1_000") {
            case None => ()
            case _ => bug!("parseAndRoundtrip01 failed: expected underscore parse failure.")
        };

        let x = BigInt.leftShift(1ii, 64i32);
        let _ = assertTrue(BigInt.toString(x) == "18446744073709551616", "parseAndRoundtrip01 failed: unexpected toString.");
        match BigInt.fromString(BigInt.toString(x)) {
            case Some(y) => assertTrue(y == x, "parseAndRoundtrip01 failed: expected roundtrip.")
            case None => bug!("parseAndRoundtrip01 failed: expected roundtrip parse success.")
        }
    }

    @Test
    def arithmetic01(): Unit = {
        let _ = assertTrue(1ii + 2ii == 3ii, "arithmetic01 failed: add.");
        let _ = assertTrue(10ii - 3ii == 7ii, "arithmetic01 failed: sub.");
        let _ = assertTrue(7ii * 6ii == 42ii, "arithmetic01 failed: mul.");
        let _ = assertTrue(10ii / 3ii == 3ii, "arithmetic01 failed: div.");
        let _ = assertTrue(BigInt.remainder(10ii, 3ii) == 1ii, "arithmetic01 failed: rem.");
        let _ = assertTrue(-7ii / 3ii == -2ii, "arithmetic01 failed: neg div.");
        assertTrue(BigInt.remainder(-7ii, 3ii) == -1ii, "arithmetic01 failed: neg rem.")
    }

    @Test
    def shiftAndBitwise01(): Unit = {
        let _ = assertTrue(BigInt.leftShift(1ii, 32i32) == 4294967296ii, "shiftAndBitwise01 failed: shl.");
        let _ = assertTrue(BigInt.leftShift(1ii, -1i32) == 0ii, "shiftAndBitwise01 failed: shl negative.");
        let _ = assertTrue(BigInt.rightShift(1ii, -1i32) == 2ii, "shiftAndBitwise01 failed: shr negative.");
        let _ = assertTrue(BigInt.rightShift(-1ii, 1i32) == -1ii, "shiftAndBitwise01 failed: shr sign-ext.");
        let _ = assertTrue(BigInt.bitwiseNot(0ii) == -1ii, "shiftAndBitwise01 failed: not.");
        let _ = assertTrue(BigInt.bitwiseAnd(-1ii, 255ii) == 255ii, "shiftAndBitwise01 failed: and.");
        let _ = assertTrue(BigInt.bitwiseOr(1ii, 2ii) == 3ii, "shiftAndBitwise01 failed: or.");
        assertTrue(BigInt.bitwiseXor(1ii, 3ii) == 2ii, "shiftAndBitwise01 failed: xor.")
    }

    @Test
    def bitLengthAndHash01(): Unit = {
        let _ = assertTrue(BigInt.log2(0ii) == 0i32, "bitLengthAndHash01 failed: bitLength(0).");
        let _ = assertTrue(BigInt.log2(-1ii) == 0i32, "bitLengthAndHash01 failed: bitLength(-1).");
        let _ = assertTrue(BigInt.log2(-2ii) == 1i32, "bitLengthAndHash01 failed: bitLength(-2).");
        let _ = assertTrue(BigInt.log2(BigInt.leftShift(1ii, 32i32)) == 33i32, "bitLengthAndHash01 failed: bitLength(2^32).");
        let _ = assertTrue(Hash.hash(0ii) == 0i32, "bitLengthAndHash01 failed: hash(0).");
        let _ = assertTrue(Hash.hash(-1ii) == -1i32, "bitLengthAndHash01 failed: hash(-1).");
        assertTrue(Hash.hash(BigInt.leftShift(1ii, 64i32)) == 961i32, "bitLengthAndHash01 failed: hash(2^64).")
    }

    pub def runAll(): Unit = {
        let _ = parseAndRoundtrip01();
        let _ = arithmetic01();
        let _ = shiftAndBitwise01();
        let _ = bitLengthAndHash01();
        ()
    }

}
