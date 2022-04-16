namespace Test/Exp/ArraySlice {

    @test
    def testArraySlice01(): Unit = region r {
        let x = [1, 2, 3] @ r;
        let _ = x[0..2];
        ()
    }

    @test
    def testArraySlice02(): Unit = region r {
        let x = [1, 2, 3, 4] @ r;
        let _ = x[2..3];
        ()
    }

    @test
    def testArraySlice03(): Unit = region r {
        let x = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20] @ r;
        let _ = x[10..15];
        ()
    }

    @test
    def testArraySlice04(): Unit = region r {
        let x = [1, 2] @ r;
        let _ = x[1..2];
        ()
    }

    @test
    def testArraySlice05(): Unit = region r {
        let x = [true, false] @ r;
        let _ = x[0..1];
        ()
    }

    @test
    def testArraySlice06(): Unit = region r {
        let x = [false, true, false] @ r;
        let _ = x[2..3];
        ()
    }

    @test
    def testArraySlice07(): Unit = region r {
        let x = [false, false, false, false, true] @ r;
        let _ = x[1..3];
        ()
    }

    @test
    def testArraySlice08(): Unit = region r {
        let x = ['a', 'b'] @ r;
        let _ = x[0..1];
        ()
    }

    @test
    def testArraySlice09(): Unit = region r {
        let x = ['a', 'b', 'c'] @ r;
        let _ = x[0..2];
        ()
    }

    @test
    def testArraySlice10(): Unit = region r {
        let x = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n'] @ r;
        let _ = x[2..6];
        ()
    }

    @test
    def testArraySlice11(): Unit = region r {
        let x = [1i8, 2i8, 3i8] @ r;
        let _ = x[1..2];
        ()
    }

    @test
    def testArraySlice12(): Unit = region r {
        let x = [1i8, 2i8, 3i8, 4i8] @ r;
        let _ = x[2..4];
        ()
    }

    @test
    def testArraySlice13(): Unit = region r {
        let x = [1i16, 2i16, 3i16] @ r;
        let _ = x[1..2];
        ()
    }

    @test
    def testArraySlice14(): Unit = region r {
        let x = [1i16, 2i16, 3i16, 4i16] @ r;
        let _ = x[2..4];
        ()
    }

    @test
    def testArraySlice15(): Unit = region r {
        let x = [1i64, 2i64, 3i64] @ r;
        let _ = x[1..2];
        ()
    }

    @test
    def testArraySlice16(): Unit = region r {
        let x = [1i64, 2i64, 3i64, 4i64] @ r;
        let _ = x[2..4];
        ()
    }

    @test
    def testArraySlice17(): Unit = region r {
        let x = [1.0f32, 2.0f32, 3.0f32] @ r;
        let _ = x[1..2];
        ()
    }

    @test
    def testArraySlice18(): Unit = region r {
        let x = [1.0f32, 2.0f32, 3.0f32, 4.0f32] @ r;
        let _ = x[2..4];
        ()
    }

    @test
    def testArraySlice19(): Unit = region r {
        let x = [1.0f64, 2.0f64, 3.0f64] @ r;
        let _ = x[1..2];
        ()
    }

    @test
    def testArraySlice20(): Unit = region r {
        let x = [1.0f64, 2.0f64, 3.0f64, 4.0f64] @ r;
        let _ = x[3..4];
        ()
    }

    @test
    def testArraySlice21(): Unit = region r {
        let x = ["Hello", "World"] @ r;
        let _ = x[1..2];
        ()
    }

    @test
    def testArraySlice22(): Unit = region r {
        let x = ["World", "Hello", "World"] @ r;
        let _ = x[2..3];
        ()
    }

    @test
    def testArraySlice23(): Unit = region r {
        let x = [[1, 2] @ r, [1, 2] @ r] @ r;
        let _ = x[1..2];
        ()
    }

    @test
    def testArraySlice24(): Unit = region r {
        let x = [[1] @ r, [1, 2, 3, 4, 5, 6, 7, 8] @ r] @ r;
        let _ = x[0..2];
        ()
    }

    @test
    def testArraySlice25(): Unit = region r {
        let x = [[1, 2, 3, 4, 5] @ r, [2] @ r] @ r;
        let _ = x[0..1];
        ()
    }

    @test
    def testArraySlice26(): Unit = region r {
        let x = [[1, 2, 3] @ r, [1, 2, 3] @ r, [1, 2, 3] @ r] @ r;
        let _ = x[0..3];
        ()
    }

    @test
    def testArraySlice27(): Unit = region r {
        let x = [(1 :: Nil), (2 :: Nil)] @ r;
        let _ = x[1..2];
        ()
    }

    @test
    def testArraySlice28(): Unit = region r {
        let x = [(true, true), (true, false), (true, true)] @ r;
        let _ = x[1..2];
        ()
    }

    @test
    def testArraySlice29(): Unit = region r {
        let x = [Some(1), Some(2), Some(3), Some(4), None] @ r;
        let _ = x[1..2];
        ()
    }

}
