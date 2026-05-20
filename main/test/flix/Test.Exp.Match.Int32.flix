mod Test.Exp.Match.Int32 {

    use Assert.{assertEq, success, fail}

    @Test
    def matchInt32_01(): Unit \ Assert = match 0i32 {
        case 0i32 => success("matched 0i32")
        case 1i32 => fail("unexpected match on 1i32")
        case 2i32 => fail("unexpected match on 2i32")
        case _    => fail("unexpected match on wildcard")
    }

    @Test
    def matchInt32_02(): Unit \ Assert = match 1i32 {
        case 0i32 => fail("unexpected match on 0i32")
        case 1i32 => success("matched 1i32")
        case 2i32 => fail("unexpected match on 2i32")
        case _    => fail("unexpected match on wildcard")
    }

    @Test
    def matchInt32_03(): Unit \ Assert = match 2i32 {
        case 0i32 => fail("unexpected match on 0i32")
        case 1i32 => fail("unexpected match on 1i32")
        case 2i32 => success("matched 2i32")
        case _    => fail("unexpected match on wildcard")
    }

    @Test
    def matchInt32_04(): Unit \ Assert = match 3i32 {
        case 0i32 => fail("unexpected match on 0i32")
        case 1i32 => fail("unexpected match on 1i32")
        case 2i32 => fail("unexpected match on 2i32")
        case _    => success("matched wildcard")
    }

    @Test
    def matchInt32Var01(): Unit \ Assert = match 0i32 {
        case x => assertEq(expected = 0i32, x)
    }

    @Test
    def matchInt32Var02(): Unit \ Assert = match 2i32 {
        case 0i32 => fail("unexpected match on 0i32")
        case 1i32 => fail("unexpected match on 1i32")
        case x    => assertEq(expected = 2i32, x)
    }

}
