mod Test.Exp.Match.Char {

    @Test
    def testMatchChar01(): Bool = match 'a' {
        case 'a' => true
        case 'b' => false
        case 'c' => false
        case _   => false
    }

    @Test
    def testMatchChar02(): Bool = match 'b' {
        case 'a' => false
        case 'b' => true
        case 'c' => false
        case _   => false
    }

    @Test
    def testMatchChar03(): Bool = match 'c' {
        case 'a' => false
        case 'b' => false
        case 'c' => true
        case _   => false
    }

    @Test
    def testMatchChar04(): Bool = match 'd' {
        case 'a' => false
        case 'b' => false
        case 'c' => false
        case _   => true
    }

    @Test
    def testMatchChar05(): Bool = match '\n' {
        case '\n' => true
        case '\r' => false
        case '\t' => false
        case _    => false
    }

    @Test
    def testMatchChar06(): Bool = match '\r' {
        case '\n' => false
        case '\r' => true
        case '\t' => false
        case _    => false
    }

    @Test
    def testMatchChar07(): Bool = match '\t' {
        case '\n' => false
        case '\r' => false
        case '\t' => true
        case _    => false
    }

    @Test
    def testMatchChar08(): Bool = match '\u0040' {
        case '\n' => false
        case '\r' => false
        case '\t' => false
        case _    => true
    }

    @Test
    def testMatchChar09(): Bool = match '\u0040' {
        case '\u0040' => true
        case '\u0041' => true
        case '\u0042' => true
        case _        => false
    }

    @Test
    def testMatchChar10(): Bool = match '\u0041' {
        case '\u0040' => false
        case '\u0041' => true
        case '\u0042' => false
        case _        => false
    }

    @Test
    def testMatchChar11(): Bool = match '\u0042' {
        case '\u0040' => false
        case '\u0041' => false
        case '\u0042' => true
        case _        => false
    }

    @Test
    def testMatchChar12(): Bool = match '\u0043' {
        case '\u0040' => false
        case '\u0041' => false
        case '\u0042' => false
        case _        => true
    }

    @Test
    def testMatchCharVar01(): Bool = match 'a' {
        case x => x == 'a'
    }

    @Test
    def testMatchCharVar02(): Bool = match 'c' {
        case 'a' => false
        case 'b' => false
        case x   => x == 'c'
    }

}
