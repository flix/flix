mod Test.Exp.Match.Int16 {

    use Assert.{assertEq, success, fail}

    @Test
    def matchInt16_01(): Unit \ Assert = match 0i16 {
        case 0i16 => success("matched 0i16")
        case 1i16 => fail("unexpected match on 1i16")
        case 2i16 => fail("unexpected match on 2i16")
        case _    => fail("unexpected match on wildcard")
    }

    @Test
    def matchInt16_02(): Unit \ Assert = match 1i16 {
        case 0i16 => fail("unexpected match on 0i16")
        case 1i16 => success("matched 1i16")
        case 2i16 => fail("unexpected match on 2i16")
        case _    => fail("unexpected match on wildcard")
    }

    @Test
    def matchInt16_03(): Unit \ Assert = match 2i16 {
        case 0i16 => fail("unexpected match on 0i16")
        case 1i16 => fail("unexpected match on 1i16")
        case 2i16 => success("matched 2i16")
        case _    => fail("unexpected match on wildcard")
    }

    @Test
    def matchInt16_04(): Unit \ Assert = match 3i16 {
        case 0i16 => fail("unexpected match on 0i16")
        case 1i16 => fail("unexpected match on 1i16")
        case 2i16 => fail("unexpected match on 2i16")
        case _    => success("matched wildcard")
    }

    @Test
    def matchInt16Var01(): Unit \ Assert = match 0i16 {
        case x => assertEq(expected = 0i16, x)
    }

    @Test
    def matchInt16Var02(): Unit \ Assert = match 2i16 {
        case 0i16 => fail("unexpected match on 0i16")
        case 1i16 => fail("unexpected match on 1i16")
        case x    => assertEq(expected = 2i16, x)
    }

}
