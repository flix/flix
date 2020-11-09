namespace Test/Exp/Match/Char {

    @test
    def testMatchChar01(): Bool = match 'a' {
        case 'a' => true
        case 'b' => false
        case 'c' => false
        case _   => false
    }

    @test
    def testMatchChar02(): Bool = match 'b' {
        case 'a' => false
        case 'b' => true
        case 'c' => false
        case _   => false
    }

    @test
    def testMatchChar03(): Bool = match 'c' {
        case 'a' => false
        case 'b' => false
        case 'c' => true
        case _   => false
    }

    @test
    def testMatchChar04(): Bool = match 'd' {
        case 'a' => false
        case 'b' => false
        case 'c' => false
        case _   => true
    }

    @test
    def testMatchChar05(): Bool = match '\n' {
        case '\n' => true
        case '\r' => false
        case '\t' => false
        case _    => false
    }

    @test
    def testMatchChar06(): Bool = match '\r' {
        case '\n' => false
        case '\r' => true
        case '\t' => false
        case _    => false
    }

    @test
    def testMatchChar07(): Bool = match '\t' {
        case '\n' => false
        case '\r' => false
        case '\t' => true
        case _    => false
    }

    @test
    def testMatchChar08(): Bool = match '\u0040' {
        case '\n' => false
        case '\r' => false
        case '\t' => true
        case _    => false
    }

}
