mod Test.Exp.Match.Int8 {

    use Assert.{assertEq, success, fail}

    @Test
    def matchInt8_01(): Unit \ Assert = match 0i8 {
        case 0i8 => success("matched 0i8")
        case 1i8 => fail("unexpected match on 1i8")
        case 2i8 => fail("unexpected match on 2i8")
        case _   => fail("unexpected match on wildcard")
    }

    @Test
    def matchInt8_02(): Unit \ Assert = match 1i8 {
        case 0i8 => fail("unexpected match on 0i8")
        case 1i8 => success("matched 1i8")
        case 2i8 => fail("unexpected match on 2i8")
        case _   => fail("unexpected match on wildcard")
    }

    @Test
    def matchInt8_03(): Unit \ Assert = match 2i8 {
        case 0i8 => fail("unexpected match on 0i8")
        case 1i8 => fail("unexpected match on 1i8")
        case 2i8 => success("matched 2i8")
        case _   => fail("unexpected match on wildcard")
    }

    @Test
    def matchInt8_04(): Unit \ Assert = match 3i8 {
        case 0i8 => fail("unexpected match on 0i8")
        case 1i8 => fail("unexpected match on 1i8")
        case 2i8 => fail("unexpected match on 2i8")
        case _   => success("matched wildcard")
    }

    @Test
    def matchInt8Var01(): Unit \ Assert = match 0i8 {
        case x => assertEq(expected = 0i8, x)
    }

    @Test
    def matchInt8Var02(): Unit \ Assert = match 2i8 {
        case 0i8 => fail("unexpected match on 0i8")
        case 1i8 => fail("unexpected match on 1i8")
        case x   => assertEq(expected = 2i8, x)
    }

}
