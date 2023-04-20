mod Test.Exp.Match.Float32 {

    @test
    def testMatchFloat32_01(): Bool = match 0.0f32 {
        case 0.0f32 => true
        case 1.0f32 => false
        case 2.0f32 => false
        case _      => false
    }

    @test
    def testMatchFloat32_02(): Bool = match 1.0f32 {
        case 0.0f32 => false
        case 1.0f32 => true
        case 2.0f32 => false
        case _      => false
    }

    @test
    def testMatchFloat32_03(): Bool = match 2.0f32 {
        case 0.0f32 => false
        case 1.0f32 => false
        case 2.0f32 => true
        case _      => false
    }

    @test
    def testMatchFloat32_04(): Bool = match 3.0f32 {
        case 0.0f32 => false
        case 1.0f32 => false
        case 2.0f32 => false
        case _      => true
    }

    @test
    def testMatchFloat32Var01(): Bool = match 0.0f32 {
        case x => x == 0.0f32
    }

    @test
    def testMatchFloat32Var02(): Bool = match 2.0f32 {
        case 0.0f32 => false
        case 1.0f32 => false
        case x   => x == 2.0f32
    }

}
