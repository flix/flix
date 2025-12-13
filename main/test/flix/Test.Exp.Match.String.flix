mod Test.Exp.Match.String {

    use Assert.{assertEq, success, fail}

    @Test
    def matchString01(): Unit \ Assert = match "" {
        case ""    => success("matched empty string")
        case "a"   => fail("unexpected match on 'a'")
        case "abc" => fail("unexpected match on 'abc'")
        case _     => fail("unexpected match on wildcard")
    }

    @Test
    def matchString02(): Unit \ Assert = match "a" {
        case ""    => fail("unexpected match on empty string")
        case "a"   => success("matched 'a'")
        case "abc" => fail("unexpected match on 'abc'")
        case _     => fail("unexpected match on wildcard")
    }

    @Test
    def matchString03(): Unit \ Assert = match "abc" {
        case ""    => fail("unexpected match on empty string")
        case "a"   => fail("unexpected match on 'a'")
        case "abc" => success("matched 'abc'")
        case _     => fail("unexpected match on wildcard")
    }

    @Test
    def matchString04(): Unit \ Assert = match "def" {
        case ""    => fail("unexpected match on empty string")
        case "a"   => fail("unexpected match on 'a'")
        case "abc" => fail("unexpected match on 'abc'")
        case _     => success("matched wildcard")
    }

    @Test
    def matchString05(): Unit \ Assert = match "\"" {
        case "\"" => success("matched quote")
        case _    => fail("unexpected match on wildcard")
    }

    @Test
    def matchString06(): Unit \ Assert = match "\'\u1234\'" {
        case "\'\u1234\'" => success("matched unicode string")
        case _            => fail("unexpected match on wildcard")
    }

    @Test
    def matchString07(): Unit \ Assert = match "\r\n" {
        case "\r\n" => success("matched CRLF")
        case _      => fail("unexpected match on wildcard")
    }

    @Test
    def matchStringVar01(): Unit \ Assert = match "abc" {
        case x => assertEq(expected = "abc", x)
    }

    @Test
    def matchStringVar02(): Unit \ Assert = match "def" {
        case ""    => fail("unexpected match on empty string")
        case "a"   => fail("unexpected match on 'a'")
        case "abc" => fail("unexpected match on 'abc'")
        case x     => assertEq(expected = "def", x)
    }

}
