mod Test.Exp.Match.Char {

    use Assert.{assertEq, success, fail}

    @Test
    def matchChar01(): Unit \ Assert = match 'a' {
        case 'a' => success("matched 'a'")
        case 'b' => fail("unexpected match on 'b'")
        case 'c' => fail("unexpected match on 'c'")
        case _   => fail("unexpected match on wildcard")
    }

    @Test
    def matchChar02(): Unit \ Assert = match 'b' {
        case 'a' => fail("unexpected match on 'a'")
        case 'b' => success("matched 'b'")
        case 'c' => fail("unexpected match on 'c'")
        case _   => fail("unexpected match on wildcard")
    }

    @Test
    def matchChar03(): Unit \ Assert = match 'c' {
        case 'a' => fail("unexpected match on 'a'")
        case 'b' => fail("unexpected match on 'b'")
        case 'c' => success("matched 'c'")
        case _   => fail("unexpected match on wildcard")
    }

    @Test
    def matchChar04(): Unit \ Assert = match 'd' {
        case 'a' => fail("unexpected match on 'a'")
        case 'b' => fail("unexpected match on 'b'")
        case 'c' => fail("unexpected match on 'c'")
        case _   => success("matched wildcard")
    }

    @Test
    def matchChar05(): Unit \ Assert = match '\n' {
        case '\n' => success("matched newline")
        case '\r' => fail("unexpected match on carriage return")
        case '\t' => fail("unexpected match on tab")
        case _    => fail("unexpected match on wildcard")
    }

    @Test
    def matchChar06(): Unit \ Assert = match '\r' {
        case '\n' => fail("unexpected match on newline")
        case '\r' => success("matched carriage return")
        case '\t' => fail("unexpected match on tab")
        case _    => fail("unexpected match on wildcard")
    }

    @Test
    def matchChar07(): Unit \ Assert = match '\t' {
        case '\n' => fail("unexpected match on newline")
        case '\r' => fail("unexpected match on carriage return")
        case '\t' => success("matched tab")
        case _    => fail("unexpected match on wildcard")
    }

    @Test
    def matchChar08(): Unit \ Assert = match '\u0040' {
        case '\n' => fail("unexpected match on newline")
        case '\r' => fail("unexpected match on carriage return")
        case '\t' => fail("unexpected match on tab")
        case _    => success("matched wildcard")
    }

    @Test
    def matchChar09(): Unit \ Assert = match '\u0040' {
        case '\u0040' => success("matched unicode 0040")
        case '\u0041' => success("matched unicode 0041")
        case '\u0042' => success("matched unicode 0042")
        case _        => fail("unexpected match on wildcard")
    }

    @Test
    def matchChar10(): Unit \ Assert = match '\u0041' {
        case '\u0040' => fail("unexpected match on unicode 0040")
        case '\u0041' => success("matched unicode 0041")
        case '\u0042' => fail("unexpected match on unicode 0042")
        case _        => fail("unexpected match on wildcard")
    }

    @Test
    def matchChar11(): Unit \ Assert = match '\u0042' {
        case '\u0040' => fail("unexpected match on unicode 0040")
        case '\u0041' => fail("unexpected match on unicode 0041")
        case '\u0042' => success("matched unicode 0042")
        case _        => fail("unexpected match on wildcard")
    }

    @Test
    def matchChar12(): Unit \ Assert = match '\u0043' {
        case '\u0040' => fail("unexpected match on unicode 0040")
        case '\u0041' => fail("unexpected match on unicode 0041")
        case '\u0042' => fail("unexpected match on unicode 0042")
        case _        => success("matched wildcard")
    }

    @Test
    def matchCharVar01(): Unit \ Assert = match 'a' {
        case x => assertEq(expected = 'a', x)
    }

    @Test
    def matchCharVar02(): Unit \ Assert = match 'c' {
        case 'a' => fail("unexpected match on 'a'")
        case 'b' => fail("unexpected match on 'b'")
        case x   => assertEq(expected = 'c', x)
    }

}
