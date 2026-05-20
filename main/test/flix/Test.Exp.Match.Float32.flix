mod Test.Exp.Match.Float32 {

    use Assert.{assertEq, success, fail}

    @Test
    def matchFloat32_01(): Unit \ Assert = match 0.0f32 {
        case 0.0f32 => success("matched 0.0f32")
        case 1.0f32 => fail("unexpected match on 1.0f32")
        case 2.0f32 => fail("unexpected match on 2.0f32")
        case _      => fail("unexpected match on wildcard")
    }

    @Test
    def matchFloat32_02(): Unit \ Assert = match 1.0f32 {
        case 0.0f32 => fail("unexpected match on 0.0f32")
        case 1.0f32 => success("matched 1.0f32")
        case 2.0f32 => fail("unexpected match on 2.0f32")
        case _      => fail("unexpected match on wildcard")
    }

    @Test
    def matchFloat32_03(): Unit \ Assert = match 2.0f32 {
        case 0.0f32 => fail("unexpected match on 0.0f32")
        case 1.0f32 => fail("unexpected match on 1.0f32")
        case 2.0f32 => success("matched 2.0f32")
        case _      => fail("unexpected match on wildcard")
    }

    @Test
    def matchFloat32_04(): Unit \ Assert = match 3.0f32 {
        case 0.0f32 => fail("unexpected match on 0.0f32")
        case 1.0f32 => fail("unexpected match on 1.0f32")
        case 2.0f32 => fail("unexpected match on 2.0f32")
        case _      => success("matched wildcard")
    }

    @Test
    def matchFloat32Var01(): Unit \ Assert = match 0.0f32 {
        case x => assertEq(expected = 0.0f32, x)
    }

    @Test
    def matchFloat32Var02(): Unit \ Assert = match 2.0f32 {
        case 0.0f32 => fail("unexpected match on 0.0f32")
        case 1.0f32 => fail("unexpected match on 1.0f32")
        case x      => assertEq(expected = 2.0f32, x)
    }

}
