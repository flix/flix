namespace Test/Exp/ArraySliceNoStartIndex {

    @test
    def testArraySliceNoStartIndex01(): Unit = region r {
        let x = [1, 2, 3] @ r;
        discard x[..2];
        ()
    }

    @test
    def testArraySliceNoStartIndex02(): Unit = region r {
        let x = [1, 2, 3, 4] @ r;
        discard x[..3];
        ()
    }

    @test
    def testArraySliceNoStartIndex03(): Unit = region r {
        let x = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20] @ r;
        discard x[..15];
        ()
    }

    @test
    def testArraySliceNoStartIndex04(): Unit = region r {
        let x = [1, 2] @ r;
        discard x[..2];
        ()
    }

    @test
    def testArraySliceNoStartIndex05(): Unit = region r {
        let x = [true, false] @ r;
        discard x[..1];
        ()
    }

    @test
    def testArraySliceNoStartIndex06(): Unit = region r {
        let x = [false, true, false] @ r;
        discard x[..3];
        ()
    }

    @test
    def testArraySliceNoStartIndex07(): Unit = region r {
        let x = [false, false, false, false, true] @ r;
        discard x[..3];
        ()
    }

    @test
    def testArraySliceNoStartIndex08(): Unit = region r {
        let x = ['a', 'b'] @ r;
        discard x[..1];
        ()
    }

    @test
    def testArraySliceNoStartIndex09(): Unit = region r {
        let x = ['a', 'b', 'c'] @ r;
        discard x[..2];
        ()
    }

    @test
    def testArraySliceNoStartIndex10(): Unit = region r {
        let x = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'] @ r;
        discard x[..6];
        ()
    }

    @test
    def testArraySliceNoStartIndex11(): Unit = region r {
        let x = [1i8, 2i8, 3i8] @ r;
        discard x[..2];
        ()
    }

    @test
    def testArraySliceNoStartIndex12(): Unit = region r {
        let x = [1i8, 2i8, 3i8, 4i8] @ r;
        discard x[..4];
        ()
    }

    @test
    def testArraySliceNoStartIndex13(): Unit = region r {
        let x = [1i16, 2i16, 3i16] @ r;
        discard x[..2];
        ()
    }

    @test
    def testArraySliceNoStartIndex14(): Unit = region r {
        let x = [1i16, 2i16, 3i16, 4i16] @ r;
        discard x[..4];
        ()
    }

    @test
    def testArraySliceNoStartIndex15(): Unit = region r {
        let x = [1i64, 2i64, 3i64] @ r;
        discard x[..2];
        ()
    }

    @test
    def testArraySliceNoStartIndex16(): Unit = region r {
        let x = [1i64, 2i64, 3i64, 4i64] @ r;
        discard x[..4];
        ()
    }

    @test
    def testArraySliceNoStartIndex17(): Unit = region r {
        let x = [1.0f32, 2.0f32, 3.0f32] @ r;
        discard x[..2];
        ()
    }

    @test
    def testArraySliceNoStartIndex18(): Unit = region r {
        let x = [1.0f32, 2.0f32, 3.0f32, 4.0f32] @ r;
        discard x[..4];
        ()
    }

    @test
    def testArraySliceNoStartIndex19(): Unit = region r {
        let x = [1.0f64, 2.0f64, 3.0f64] @ r;
        discard x[..2];
        ()
    }

    @test
    def testArraySliceNoStartIndex20(): Unit = region r {
        let x = [1.0f64, 2.0f64, 3.0f64, 4.0f64] @ r;
        discard x[..4];
        ()
    }

    @test
    def testArraySliceNoStartIndex21(): Unit = region r {
        let x = ["Hello", "World"] @ r;
        discard x[..2];
        ()
    }

    @test
    def testArraySliceNoStartIndex22(): Unit = region r {
        let x = ["World", "Hello", "World"] @ r;
        discard x[..3];
        ()
    }

    @test
    def testArraySliceNoStartIndex23(): Unit = region r {
        let x = [[1, 2] @ r, [1, 2] @ r] @ r;
        discard x[..2];
        ()
    }

    @test
    def testArraySliceNoStartIndex24(): Unit = region r {
        let x = [[1] @ r, [1, 2, 3, 4, 5, 6, 7, 8] @ r] @ r;
        discard x[..2];
        ()
    }

    @test
    def testArraySliceNoStartIndex25(): Unit = region r {
        let x = [[1, 2, 3, 4, 5] @ r, [2] @ r] @ r;
        discard x[..1];
        ()
    }

    @test
    def testArraySliceNoStartIndex26(): Unit = region r {
        let x = [[1, 2, 3] @ r, [1, 2, 3] @ r, [1, 2, 3] @ r] @ r;
        discard x[..3];
        ()
    }

    @test
    def testArraySliceNoStartIndex27(): Unit = region r {
        let x = [(1 :: Nil), (2 :: Nil)] @ r;
        discard x[..1];
        ()
    }

    @test
    def testArraySliceNoStartIndex28(): Unit = region r {
        let x = [(true, true), (true, false), (true, true)] @ r;
        discard x[..2];
        ()
    }

    @test
    def testArraySliceNoStartIndex29(): Unit = region r {
        let x = [None, None, Some(1)] @ r;
        discard x[..2];
        ()
    }

}
