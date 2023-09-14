mod Test.Exp.Match.Int64 {

    @Test
    def testMatchInt64_01(): Bool = match 0i64 {
        case 0i64 => true
        case 1i64 => false
        case 2i64 => false
        case _    => false
    }

    @Test
    def testMatchInt64_02(): Bool = match 1i64 {
        case 0i64 => false
        case 1i64 => true
        case 2i64 => false
        case _    => false
    }

    @Test
    def testMatchInt64_03(): Bool = match 2i64 {
        case 0i64 => false
        case 1i64 => false
        case 2i64 => true
        case _    => false
    }

    @Test
    def testMatchInt64_04(): Bool = match 3i64 {
        case 0i64 => false
        case 1i64 => false
        case 2i64 => false
        case _    => true
    }

    @Test
    def testMatchInt64Var01(): Bool = match 0i64 {
        case x => x == 0i64
    }

    @Test
    def testMatchInt64Var02(): Bool = match 2i64 {
        case 0i64 => false
        case 1i64 => false
        case x   => x == 2i64
    }

}
