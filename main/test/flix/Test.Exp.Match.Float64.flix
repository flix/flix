mod Test.Exp.Match.Float64 {

    @Test
    def testMatchFloat64_01(): Bool = match 0.0f64 {
        case 0.0f64 => true
        case 1.0f64 => false
        case 2.0f64 => false
        case _      => false
    }

    @Test
    def testMatchFloat64_02(): Bool = match 1.0f64 {
        case 0.0f64 => false
        case 1.0f64 => true
        case 2.0f64 => false
        case _      => false
    }

    @Test
    def testMatchFloat64_03(): Bool = match 2.0f64 {
        case 0.0f64 => false
        case 1.0f64 => false
        case 2.0f64 => true
        case _      => false
    }

    @Test
    def testMatchFloat64_04(): Bool = match 3.0f64 {
        case 0.0f64 => false
        case 1.0f64 => false
        case 2.0f64 => false
        case _      => true
    }

    @Test
    def testMatchFloat64Var01(): Bool = match 0.0f64 {
        case x => x == 0.0f64
    }

    @Test
    def testMatchFloat64Var02(): Bool = match 2.0f64 {
        case 0.0f64 => false
        case 1.0f64 => false
        case x   => x == 2.0f64
    }

}
