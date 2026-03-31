mod Test.Exp.Match.Int64 {

    use Assert.{assertEq, success, fail}

    @Test
    def matchInt64_01(): Unit \ Assert = match 0i64 {
        case 0i64 => success("matched 0i64")
        case 1i64 => fail("unexpected match on 1i64")
        case 2i64 => fail("unexpected match on 2i64")
        case _    => fail("unexpected match on wildcard")
    }

    @Test
    def matchInt64_02(): Unit \ Assert = match 1i64 {
        case 0i64 => fail("unexpected match on 0i64")
        case 1i64 => success("matched 1i64")
        case 2i64 => fail("unexpected match on 2i64")
        case _    => fail("unexpected match on wildcard")
    }

    @Test
    def matchInt64_03(): Unit \ Assert = match 2i64 {
        case 0i64 => fail("unexpected match on 0i64")
        case 1i64 => fail("unexpected match on 1i64")
        case 2i64 => success("matched 2i64")
        case _    => fail("unexpected match on wildcard")
    }

    @Test
    def matchInt64_04(): Unit \ Assert = match 3i64 {
        case 0i64 => fail("unexpected match on 0i64")
        case 1i64 => fail("unexpected match on 1i64")
        case 2i64 => fail("unexpected match on 2i64")
        case _    => success("matched wildcard")
    }

    @Test
    def matchInt64Var01(): Unit \ Assert = match 0i64 {
        case x => assertEq(expected = 0i64, x)
    }

    @Test
    def matchInt64Var02(): Unit \ Assert = match 2i64 {
        case 0i64 => fail("unexpected match on 0i64")
        case 1i64 => fail("unexpected match on 1i64")
        case x    => assertEq(expected = 2i64, x)
    }

}
