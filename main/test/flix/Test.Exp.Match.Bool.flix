mod Test.Exp.Match.Bool {

    use Assert.{assertEq, success, fail}

    @Test
    def matchBool01(): Unit \ Assert = match true {
        case true  => success("matched true")
        case false => fail("unexpected match on false")
    }

    @Test
    def matchBool02(): Unit \ Assert = match true {
        case false => fail("unexpected match on false")
        case true  => success("matched true")
    }

    @Test
    def matchBool03(): Unit \ Assert = match false {
        case false => success("matched false")
        case true  => fail("unexpected match on true")
    }

    @Test
    def matchBool04(): Unit \ Assert = match false {
        case true  => fail("unexpected match on true")
        case false => success("matched false")
    }

    @Test
    def matchBoolVar01(): Unit \ Assert = match true {
        case x => assertEq(expected = true, x)
    }

    @Test
    def matchBoolVar02(): Unit \ Assert = match false {
        case x => assertEq(expected = false, x)
    }

    @Test
    def matchBoolVar03(): Unit \ Assert = match false {
        case true => fail("unexpected match on true")
        case x    => assertEq(expected = false, x)
    }

    @Test
    def matchBoolVar04(): Unit \ Assert = match true {
        case false => fail("unexpected match on false")
        case x     => assertEq(expected = true, x)
    }

    @Test
    def matchBoolTuple01(): Unit \ Assert = match (true, false) {
        case (false, false) => fail("unexpected match on (false, false)")
        case (false, true)  => fail("unexpected match on (false, true)")
        case (true, false)  => success("matched (true, false)")
        case (true, true)   => fail("unexpected match on (true, true)")
    }

}
