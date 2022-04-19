namespace Test/Exp/ArraySliceCopy {

    @test
    def testArraySliceCopy01(): Unit = region r {
        let x = [1, 2, 3] @ r;
        let _ = x[..];
        ()
    }

    @test
    def testArraySliceCopy02(): Unit = region r {
        let x = [1, 2, 3, 4] @ r;
        let _ = x[..];
        ()
    }

    @test
    def testArraySliceCopy03(): Unit = region r {
        let x = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20] @ r;
        let _ = x[..];
        ()
    }

    @test
    def testArraySliceCopy04(): Unit = region r {
        let x = [1, 2] @ r;
        let _ = x[..];
        ()
    }

    @test
    def testArraySliceCopy05(): Unit = region r {
        let x = [true, false] @ r;
        let _ = x[..];
        ()
    }

    @test
    def testArraySliceCopy06(): Unit = region r {
        let x = [false, true, false] @ r;
        let _ = x[..];
        ()
    }

    @test
    def testArraySliceCopy07(): Unit = region r {
        let x = [false, false, false, false, true] @ r;
        let _ = x[..];
        ()
    }

    @test
    def testArraySliceCopy08(): Unit = region r {
        let x = ['a', 'b'] @ r;
        let _ = x[..];
        ()
    }

    @test
    def testArraySliceCopy09(): Unit = region r {
        let x = ['a', 'b', 'c'] @ r;
        let _ = x[..];
        ()
    }

    @test
    def testArraySliceCopy10(): Unit = region r {
        let x = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n'] @ r;
        let _ = x[..];
        ()
    }

    @test
    def testArraySliceCopy11(): Unit = region r {
        let x = [1i8, 2i8, 3i8] @ r;
        let _ = x[..];
        ()
    }

    @test
    def testArraySliceCopy12(): Unit = region r {
        let x = [1i8, 2i8, 3i8, 4i8] @ r;
        let _ = x[..];
        ()
    }

    @test
    def testArraySliceCopy13(): Unit = region r {
        let x = [1i16, 2i16, 3i16] @ r;
        let _ = x[..];
        ()
    }

    @test
    def testArraySliceCopy14(): Unit = region r {
        let x = [1i16, 2i16, 3i16, 4i16] @ r;
        let _ = x[..];
        ()
    }

    @test
    def testArraySliceCopy15(): Unit = region r {
        let x = [1i64, 2i64, 3i64] @ r;
        let _ = x[..];
        ()
    }

    @test
    def testArraySliceCopy16(): Unit = region r {
        let x = [1i64, 2i64, 3i64, 4i64] @ r;
        let _ = x[..];
        ()
    }

    @test
    def testArraySliceCopy17(): Unit = region r {
        let x = [1.0f32, 2.0f32, 3.0f32] @ r;
        let _ = x[..];
        ()
    }

    @test
    def testArraySliceCopy18(): Unit = region r {
        let x = [1.0f32, 2.0f32, 3.0f32, 4.0f32] @ r;
        let _ = x[..];
        ()
    }

    @test
    def testArraySliceCopy19(): Unit = region r {
        let x = [1.0f64, 2.0f64, 3.0f64] @ r;
        let _ = x[..];
        ()
    }

    @test
    def testArraySliceCopy20(): Unit = region r {
        let x = [1.0f64, 2.0f64, 3.0f64, 4.0f64] @ r;
        let _ = x[..];
        ()
    }

    @test
    def testArraySliceCopy21(): Unit = region r {
        let x = ["Hello", "World"] @ r;
        let _ = x[..];
        ()
    }

    @test
    def testArraySliceCopy22(): Unit = region r {
        let x = ["World", "Hello", "World"] @ r;
        let _ = x[..];
        ()
    }

    @test
    def testArraySliceCopy23(): Unit = region r {
        let x = [[1, 2] @ r, [1, 2] @ r] @ r;
        let _ = x[..];
        ()
    }

    @test
    def testArraySliceCopy24(): Unit = region r {
        let x = [[1] @ r, [1, 2, 3, 4, 5, 6, 7, 8] @ r] @ r;
        let _ = x[..];
        ()
    }

    @test
    def testArraySliceCopy25(): Unit = region r {
        let x = [[1, 2, 3, 4, 5] @ r, [2] @ r] @ r;
        let _ = x[..];
        ()
    }

    @test
    def testArraySliceCopy26(): Unit = region r {
        let x = [[1, 2, 3] @ r, [1, 2, 3] @ r, [1, 2, 3] @ r] @ r;
        let _ = x[..];
        ()
    }

    @test
    def testArraySliceCopy27(): Unit = region r {
        let x = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] @ r;
        let _ = x[..];
        ()
    }

    @test
    def testArraySliceCopy28(): Unit = region r {
        let x = [(1 :: Nil), (2 :: Nil)] @ r;
        let _ = x[..];
        ()
    }

    @test
    def testArraySliceCopy29(): Unit = region r {
        let x = [(true, true), (true, false), (true, true)] @ r;
        let _ = x[..];
        ()
    }

    @test
    def testArraySliceCopy30(): Unit = region r {
        let x = [None, None, Some(2), Some(2)] @ r;
        let _ = x[..];
        ()
    }

}
