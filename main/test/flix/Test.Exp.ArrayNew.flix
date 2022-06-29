namespace Test/Exp/ArrayNew {

    @test
    def testArrayNew01(): Unit = region r {
        discard [10; 0] @ r;
        ()
    }

    @test
    def testArrayNew02(): Unit = region r {
        discard [1; 1] @ r;
        ()
    }

    @test
    def testArrayNew03(): Unit = region r {
        discard [0; 2] @ r;
        ()
    }

    @test
    def testArrayNew04(): Unit = region r {
        discard [0; 113] @ r;
        ()
    }

    @test
    def testArrayNew05(): Unit = region r {
        discard [0; 500] @ r;
        ()
    }

    @test
    def testArrayNew06(): Unit = region r {
        discard [(); 5] @ r;
        ()
    }

    @test
    def testArrayNew07(): Unit = region r {
        discard [true; 5] @ r;
        ()
    }

    @test
    def testArrayNew08(): Unit = region r {
        discard [false; 5] @ r;
        ()
    }

    @test
    def testArrayNew09(): Unit = region r {
        discard ['a'; 5] @ r;
        ()
    }

    @test
    def testArrayNew10(): Unit = region r {
        discard [10i8; 5] @ r;
        ()
    }

    @test
    def testArrayNew11(): Unit = region r {
        discard [10i16; 5] @ r;
        ()
    }

    @test
    def testArrayNew12(): Unit = region r {
        discard [10; 5] @ r;
        ()
    }

    @test
    def testArrayNew13(): Unit = region r {
        discard [10i64; 5] @ r;
        ()
    }

    @test
    def testArrayNew14(): Unit = region r {
        discard [10ii; 5] @ r;
        ()
    }

    @test
    def testArrayNew15(): Unit = region r {
        discard [3.0f32; 5] @ r;
        ()
    }

    @test
    def testArrayNew16(): Unit = region r {
        discard [3.0f64; 5] @ r;
        ()
    }

    @test
    def testArrayNew17(): Unit = region r {
        discard ["foo"; 5] @ r;
        ()
    }

    @test
    def testArrayNew18(): Unit = region r {
        discard [[10; 10] @ r; 5] @ r;
        ()
    }

    @test
    def testArrayNew19(): Unit = region r {
        discard [(1, 2); 5] @ r;
        ()
    }

    @test
    def testArrayNew20(): Unit = region r {
        discard [(1 :: Nil); 5] @ r;
        ()
    }

    @test
    def testArrayNew21(): Unit = region r {
        discard [None; 1] @ r;
        ()
    }
}
