namespace Test/Exp/ArraySliceNoEndIndex {

    @test
    def testArraySliceNoEndIndex01(): Unit = region r {
        let x = [1, 2, 3] @ r;
        discard x[0..];
        ()
    }

    @test
    def testArraySliceNoEndIndex02(): Unit = region r {
        let x = [1, 2, 3, 4] @ r;
        discard x[2..];
        ()
    }

    @test
    def testArraySliceNoEndIndex03(): Unit = region r {
        let x = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20] @ r;
        discard x[10..];
        ()
    }

    @test
    def testArraySliceNoEndIndex04(): Unit = region r {
        let x = [1, 2] @ r;
        discard x[1..];
        ()
    }

    @test
    def testArraySliceNoEndIndex05(): Unit = region r {
        let x = [true, false] @ r;
        discard x[0..];
        ()
    }

    @test
    def testArraySliceNoEndIndex06(): Unit = region r {
        let x = [false, true, false] @ r;
        discard x[2..];
        ()
    }

    @test
    def testArraySliceNoEndIndex07(): Unit = region r {
        let x = [false, false, false, false, true] @ r;
        discard x[1..];
        ()
    }

    @test
    def testArraySliceNoEndIndex08(): Unit = region r {
        let x = ['a', 'b'] @ r;
        discard x[0..];
        ()
    }

    @test
    def testArraySliceNoEndIndex09(): Unit = region r {
        let x = ['a', 'b', 'c'] @ r;
        discard x[0..];
        ()
    }

    @test
    def testArraySliceNoEndIndex10(): Unit = region r {
        let x = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n'] @ r;
        discard x[2..];
        ()
    }

    @test
    def testArraySliceNoEndIndex11(): Unit = region r {
        let x = [1i8, 2i8, 3i8] @ r;
        discard x[1..];
        ()
    }

    @test
    def testArraySliceNoEndIndex12(): Unit = region r {
        let x = [1i8, 2i8, 3i8, 4i8] @ r;
        discard x[2..];
        ()
    }

    @test
    def testArraySliceNoEndIndex13(): Unit = region r {
        let x = [1i16, 2i16, 3i16] @ r;
        discard x[1..];
        ()
    }

    @test
    def testArraySliceNoEndIndex14(): Unit = region r {
        let x = [1i16, 2i16, 3i16, 4i16] @ r;
        discard x[2..];
        ()
    }

    @test
    def testArraySliceNoEndIndex15(): Unit = region r {
        let x = [1i64, 2i64, 3i64] @ r;
        discard x[1..];
        ()
    }

    @test
    def testArraySliceNoEndIndex16(): Unit = region r {
        let x = [1i64, 2i64, 3i64, 4i64] @ r;
        discard x[2..];
        ()
    }

    @test
    def testArraySliceNoEndIndex17(): Unit = region r {
        let x = [1.0f32, 2.0f32, 3.0f32] @ r;
        discard x[1..];
        ()
    }

    @test
    def testArraySliceNoEndIndex18(): Unit = region r {
        let x = [1.0f32, 2.0f32, 3.0f32, 4.0f32] @ r;
        discard x[2..];
        ()
    }

    @test
    def testArraySliceNoEndIndex19(): Unit = region r {
        let x = [1.0f64, 2.0f64, 3.0f64] @ r;
        discard x[1..];
        ()
    }

    @test
    def testArraySliceNoEndIndex20(): Unit = region r {
        let x = [1.0f64, 2.0f64, 3.0f64, 4.0f64] @ r;
        discard x[3..];
        ()
    }

    @test
    def testArraySliceNoEndIndex21(): Unit = region r {
        let x = ["Hello", "World"] @ r;
        discard x[1..];
        ()
    }

    @test
    def testArraySliceNoEndIndex22(): Unit = region r {
        let x = ["World", "Hello", "World"] @ r;
        discard x[2..];
        ()
    }

    @test
    def testArraySliceNoEndIndex23(): Unit = region r {
        let x = [[1, 2] @ r, [1, 2] @ r] @ r;
        discard x[1..];
        ()
    }

    @test
    def testArraySliceNoEndIndex24(): Unit = region r {
        let x = [[1] @ r, [1, 2, 3, 4, 5, 6, 7, 8] @ r] @ r;
        discard x[0..];
        ()
    }

    @test
    def testArraySliceNoEndIndex25(): Unit = region r {
        let x = [[1, 2, 3, 4, 5] @ r, [2] @ r] @ r;
        discard x[0..];
        ()
    }

    @test
    def testArraySliceNoEndIndex26(): Unit = region r {
        let x = [[1, 2, 3] @ r, [1, 2, 3] @ r, [1, 2, 3] @ r] @ r;
        discard x[0..];
        ()
    }

    @test
    def testArraySliceNoEndIndex27(): Unit = region r {
        let x = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] @ r;
        discard x[0..];
        ()
    }

    @test
    def testArraySliceNoEndIndex28(): Unit = region r {
        let x = [(1 :: Nil), (2 :: Nil)] @ r;
        discard x[0..];
        ()
    }

    @test
    def testArraySliceNoEndIndex29(): Unit = region r {
        let x = [(true, true), (true, false), (true, true)] @ r;
        discard x[0..];
        ()
    }

    @test
    def testArraySliceNoEndIndex30(): Unit = region r {
        let x = [None, None, None, None, Some(1)] @ r;
        discard x[0..];
        ()
    }

}
