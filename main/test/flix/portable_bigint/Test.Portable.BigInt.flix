mod Test.Portable.BigInt {

    def assertTrue(cond: Bool, msg: String): Unit =
        if (cond) () else bug!(msg)

    @Test
    def fromStringOk01(): Unit =
        match BigInt.fromString("  +123  ") {
            case Some(v) if v == 123ii => ()
            case _ => bug!("expected BigInt.fromString to succeed.")
        }

    @Test
    def fromStringErr01(): Unit =
        match BigInt.fromString("abc") {
            case None => ()
            case _ => bug!("expected BigInt.fromString to fail.")
        }

    @Test
    def fromStringErr02(): Unit =
        match BigInt.fromString("1_000") {
            case None => ()
            case _ => bug!("expected BigInt.fromString to reject underscores.")
        }

    @Test
    def toStringRoundtrip01(): Unit = {
        let x = BigInt.leftShift(1ii, 64i32);
        let _ = assertTrue(BigInt.toString(x) == "18446744073709551616", "unexpected BigInt.toString output.");
        match BigInt.fromString(BigInt.toString(x)) {
            case Some(y) if y == x => ()
            case _ => bug!("expected BigInt roundtrip to succeed.")
        }
    }

    @Test
    def arith01(): Unit = {
        let _ = assertTrue(1ii + 2ii == 3ii, "add failed.");
        let _ = assertTrue(10ii - 3ii == 7ii, "sub failed.");
        let _ = assertTrue(7ii * 6ii == 42ii, "mul failed.");
        ()
    }

    @Test
    def divRem01(): Unit = {
        let _ = assertTrue(10ii / 3ii == 3ii, "div failed.");
        let _ = assertTrue(BigInt.remainder(10ii, 3ii) == 1ii, "rem failed.");

        let _ = assertTrue(-7ii / 3ii == -2ii, "neg div failed.");
        let _ = assertTrue(BigInt.remainder(-7ii, 3ii) == -1ii, "neg rem failed.");

        let _ = assertTrue(7ii / -3ii == -2ii, "neg divisor div failed.");
        let _ = assertTrue(BigInt.remainder(7ii, -3ii) == 1ii, "neg divisor rem failed.");
        ()
    }

    @Test
    def shift01(): Unit = {
        let _ = assertTrue(BigInt.leftShift(1ii, 32i32) == 4294967296ii, "shl failed.");
        let _ = assertTrue(BigInt.leftShift(1ii, -1i32) == 0ii, "shl negative failed.");
        let _ = assertTrue(BigInt.rightShift(1ii, -1i32) == 2ii, "shr negative failed.");
        let _ = assertTrue(BigInt.rightShift(-1ii, 1i32) == -1ii, "shr sign-ext failed.");
        ()
    }

    @Test
    def bitwise01(): Unit = {
        let _ = assertTrue(BigInt.bitwiseNot(0ii) == -1ii, "not failed.");
        let _ = assertTrue(BigInt.bitwiseNot(-1ii) == 0ii, "not -1 failed.");
        let _ = assertTrue(BigInt.bitwiseAnd(-1ii, 255ii) == 255ii, "and failed.");
        let _ = assertTrue(BigInt.bitwiseOr(1ii, 2ii) == 3ii, "or failed.");
        let _ = assertTrue(BigInt.bitwiseXor(1ii, 3ii) == 2ii, "xor failed.");
        ()
    }

    @Test
    def bitLength01(): Unit = {
        let _ = assertTrue(BigInt.log2(0ii) == 0i32, "bitLength(0) failed.");
        let _ = assertTrue(BigInt.log2(1ii) == 1i32, "bitLength(1) failed.");
        let _ = assertTrue(BigInt.log2(2ii) == 2i32, "bitLength(2) failed.");
        let _ = assertTrue(BigInt.log2(-1ii) == 0i32, "bitLength(-1) failed.");
        let _ = assertTrue(BigInt.log2(-2ii) == 1i32, "bitLength(-2) failed.");
        let _ = assertTrue(BigInt.log2(-3ii) == 2i32, "bitLength(-3) failed.");
        let _ = assertTrue(BigInt.log2(BigInt.leftShift(1ii, 32i32)) == 33i32, "bitLength(2^32) failed.");
        ()
    }

    @Test
    def hash01(): Unit = {
        let _ = assertTrue(Hash.hash(0ii) == 0i32, "hash(0) failed.");
        let _ = assertTrue(Hash.hash(1ii) == 1i32, "hash(1) failed.");
        let _ = assertTrue(Hash.hash(-1ii) == -1i32, "hash(-1) failed.");
        let _ = assertTrue(Hash.hash(BigInt.leftShift(1ii, 32i32)) == 31i32, "hash(2^32) failed.");
        let _ = assertTrue(Hash.hash(BigInt.leftShift(1ii, 64i32)) == 961i32, "hash(2^64) failed.");
        ()
    }

    pub def runAll(): Unit = {
        let _ = fromStringOk01();
        let _ = fromStringErr01();
        let _ = fromStringErr02();
        let _ = toStringRoundtrip01();
        let _ = arith01();
        let _ = divRem01();
        let _ = shift01();
        let _ = bitwise01();
        let _ = bitLength01();
        let _ = hash01();
        ()
    }

}
