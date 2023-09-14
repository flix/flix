mod Test.Exp.Match.Int8 {

    @Test
    def testMatchInt8_01(): Bool = match 0i8 {
        case 0i8 => true
        case 1i8 => false
        case 2i8 => false
        case _   => false
    }

    @Test
    def testMatchInt8_02(): Bool = match 1i8 {
        case 0i8 => false
        case 1i8 => true
        case 2i8 => false
        case _   => false
    }

    @Test
    def testMatchInt8_03(): Bool = match 2i8 {
        case 0i8 => false
        case 1i8 => false
        case 2i8 => true
        case _   => false
    }

    @Test
    def testMatchInt8_04(): Bool = match 3i8 {
        case 0i8 => false
        case 1i8 => false
        case 2i8 => false
        case _   => true
    }

    @Test
    def testMatchInt8Var01(): Bool = match 0i8 {
        case x => x == 0i8
    }

    @Test
    def testMatchInt8Var02(): Bool = match 2i8 {
        case 0i8 => false
        case 1i8 => false
        case x   => x == 2i8
    }

}
