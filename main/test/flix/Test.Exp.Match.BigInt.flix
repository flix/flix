mod Test.Exp.Match.BigInt {

    @test
    def testMatchBigInt01(): Bool = match 0ii {
        case 0ii => true
        case 1ii => false
        case 2ii => false
        case _    => false
    }

    @test
    def testMatchBigInt02(): Bool = match 1ii {
        case 0ii => false
        case 1ii => true
        case 2ii => false
        case _    => false
    }

    @test
    def testMatchBigInt03(): Bool = match 2ii {
        case 0ii => false
        case 1ii => false
        case 2ii => true
        case _    => false
    }

    @test
    def testMatchBigInt04(): Bool = match 3ii {
        case 0ii => false
        case 1ii => false
        case 2ii => false
        case _    => true
    }

    @test
    def testMatchBigIntVar01(): Bool = match 0ii {
        case x => x == 0ii
    }

    @test
    def testMatchBigIntVar02(): Bool = match 2ii {
        case 0ii => false
        case 1ii => false
        case x   => x == 2ii
    }

}
