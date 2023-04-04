mod Test.Exp.Match.Int32 {

    @test
    def testMatchInt32_01(): Bool = match 0i32 {
        case 0i32 => true
        case 1i32 => false
        case 2i32 => false
        case _    => false
    }

    @test
    def testMatchInt32_02(): Bool = match 1i32 {
        case 0i32 => false
        case 1i32 => true
        case 2i32 => false
        case _    => false
    }

    @test
    def testMatchInt32_03(): Bool = match 2i32 {
        case 0i32 => false
        case 1i32 => false
        case 2i32 => true
        case _    => false
    }

    @test
    def testMatchInt32_04(): Bool = match 3i32 {
        case 0i32 => false
        case 1i32 => false
        case 2i32 => false
        case _    => true
    }

    @test
    def testMatchInt32Var01(): Bool = match 0i32 {
        case x => x == 0i32
    }

    @test
    def testMatchInt32Var02(): Bool = match 2i32 {
        case 0i32 => false
        case 1i32 => false
        case x   => x == 2i32
    }

}
