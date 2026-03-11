mod Test.Exp.Match.Float64 {

    use Assert.{assertEq, success, fail}

    @Test
    def matchFloat64_01(): Unit \ Assert = match 0.0f64 {
        case 0.0f64 => success("matched 0.0f64")
        case 1.0f64 => fail("unexpected match on 1.0f64")
        case 2.0f64 => fail("unexpected match on 2.0f64")
        case _      => fail("unexpected match on wildcard")
    }

    @Test
    def matchFloat64_02(): Unit \ Assert = match 1.0f64 {
        case 0.0f64 => fail("unexpected match on 0.0f64")
        case 1.0f64 => success("matched 1.0f64")
        case 2.0f64 => fail("unexpected match on 2.0f64")
        case _      => fail("unexpected match on wildcard")
    }

    @Test
    def matchFloat64_03(): Unit \ Assert = match 2.0f64 {
        case 0.0f64 => fail("unexpected match on 0.0f64")
        case 1.0f64 => fail("unexpected match on 1.0f64")
        case 2.0f64 => success("matched 2.0f64")
        case _      => fail("unexpected match on wildcard")
    }

    @Test
    def matchFloat64_04(): Unit \ Assert = match 3.0f64 {
        case 0.0f64 => fail("unexpected match on 0.0f64")
        case 1.0f64 => fail("unexpected match on 1.0f64")
        case 2.0f64 => fail("unexpected match on 2.0f64")
        case _      => success("matched wildcard")
    }

    @Test
    def matchFloat64Var01(): Unit \ Assert = match 0.0f64 {
        case x => assertEq(expected = 0.0f64, x)
    }

    @Test
    def matchFloat64Var02(): Unit \ Assert = match 2.0f64 {
        case 0.0f64 => fail("unexpected match on 0.0f64")
        case 1.0f64 => fail("unexpected match on 1.0f64")
        case x      => assertEq(expected = 2.0f64, x)
    }

}
