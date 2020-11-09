namespace Test/Exp/Match/Bool {

    @test
    def testMatchBool01(): Bool = match true {
        case true  => true
        case false => false
    }

    @test
    def testMatchBool02(): Bool = match true {
        case false => false
        case true  => true
    }

    @test
    def testMatchBool03(): Bool = match false {
        case false => true
        case true  => false
    }

    @test
    def testMatchBool04(): Bool = match false {
        case true  => false
        case false => true
    }

}
