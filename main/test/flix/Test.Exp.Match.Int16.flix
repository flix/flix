mod Test.Exp.Match.Int16 {

    @Test
    def testMatchInt16_01(): Bool = match 0i16 {
        case 0i16 => true
        case 1i16 => false
        case 2i16 => false
        case _    => false
    }

    @Test
    def testMatchInt16_02(): Bool = match 1i16 {
        case 0i16 => false
        case 1i16 => true
        case 2i16 => false
        case _    => false
    }

    @Test
    def testMatchInt16_03(): Bool = match 2i16 {
        case 0i16 => false
        case 1i16 => false
        case 2i16 => true
        case _    => false
    }

    @Test
    def testMatchInt16_04(): Bool = match 3i16 {
        case 0i16 => false
        case 1i16 => false
        case 2i16 => false
        case _    => true
    }

    @Test
    def testMatchInt16Var01(): Bool = match 0i16 {
        case x => x == 0i16
    }

    @Test
    def testMatchInt16Var02(): Bool = match 2i16 {
        case 0i16 => false
        case 1i16 => false
        case x   => x == 2i16
    }

}
