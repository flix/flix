mod Test.Exp.Match.Bool {

    @Test
    def testMatchBool01(): Bool = match true {
        case true  => true
        case false => false
    }

    @Test
    def testMatchBool02(): Bool = match true {
        case false => false
        case true  => true
    }

    @Test
    def testMatchBool03(): Bool = match false {
        case false => true
        case true  => false
    }

    @Test
    def testMatchBool04(): Bool = match false {
        case true  => false
        case false => true
    }

    @Test
    def testMatchBoolVar01(): Bool = match true {
        case x  => x == true
    }

    @Test
    def testMatchBoolVar02(): Bool = match false {
        case x  => x == false
    }

    @Test
    def testMatchBoolVar03(): Bool = match false {
        case true => false
        case x    => x == false
    }

    @Test
    def testMatchBoolVar04(): Bool = match true {
        case false => false
        case x     => x == true
    }

}
