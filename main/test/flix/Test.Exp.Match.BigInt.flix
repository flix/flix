mod Test.Exp.Match.BigInt {

    use Assert.{assertEq, success, fail}

    @Test
    def matchBigInt01(): Unit \ Assert = match 0ii {
        case 0ii => success("matched 0ii")
        case 1ii => fail("unexpected match on 1ii")
        case 2ii => fail("unexpected match on 2ii")
        case _   => fail("unexpected match on wildcard")
    }

    @Test
    def matchBigInt02(): Unit \ Assert = match 1ii {
        case 0ii => fail("unexpected match on 0ii")
        case 1ii => success("matched 1ii")
        case 2ii => fail("unexpected match on 2ii")
        case _   => fail("unexpected match on wildcard")
    }

    @Test
    def matchBigInt03(): Unit \ Assert = match 2ii {
        case 0ii => fail("unexpected match on 0ii")
        case 1ii => fail("unexpected match on 1ii")
        case 2ii => success("matched 2ii")
        case _   => fail("unexpected match on wildcard")
    }

    @Test
    def matchBigInt04(): Unit \ Assert = match 3ii {
        case 0ii => fail("unexpected match on 0ii")
        case 1ii => fail("unexpected match on 1ii")
        case 2ii => fail("unexpected match on 2ii")
        case _   => success("matched wildcard")
    }

    @Test
    def matchBigIntVar01(): Unit \ Assert = match 0ii {
        case x => assertEq(expected = 0ii, x)
    }

    @Test
    def matchBigIntVar02(): Unit \ Assert = match 2ii {
        case 0ii => fail("unexpected match on 0ii")
        case 1ii => fail("unexpected match on 1ii")
        case x   => assertEq(expected = 2ii, x)
    }

}
