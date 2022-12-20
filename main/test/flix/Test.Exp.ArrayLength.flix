namespace Test/Exp/ArrayLength {

    @test
    def testArrayLength01(): Int32 = region r {
        let x = [] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength02(): Int32 = region r {
        let x = [1] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength03(): Int32 = region r {
        let x = [1, 2, 3] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength04(): Int32 = region r {
        let x = [1, 2, 3, 4] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength05(): Int32 = region r {
        let x = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength06(): Int32 = region r {
        let x = [1, 2] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength07(): Int32 = region r {
        let x = [true] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength08(): Int32 = region r {
        let x = [false] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength09(): Int32 = region r {
        let x = [true, false] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength10(): Int32 = region r {
        let x = [false, true, false] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength11(): Int32 = region r {
        let x = [false, false, false, false, true] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength12(): Int32 = region r {
        let x = ['c'] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength13(): Int32 = region r {
        let x = ['a', 'b'] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength14(): Int32 = region r {
        let x = ['a', 'b', 'c'] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength15(): Int32 = region r {
        let x = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n'] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength16(): Int32 = region r {
        let x = [1i8] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength17(): Int32 = region r {
        let x = [1i8, 2i8, 3i8] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength18(): Int32 = region r {
        let x = [1i8, 2i8, 3i8, 4i8] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength19(): Int32 = region r {
        let x = [1i16] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength20(): Int32 = region r {
        let x = [1i16, 2i16, 3i16] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength21(): Int32 = region r {
        let x = [1i16, 2i16, 3i16, 4i16] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength22(): Int32 = region r {
        let x = [1i64] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength23(): Int32 = region r {
        let x = [1i64, 2i64, 3i64] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength24(): Int32 = region r {
        let x = [1i64, 2i64, 3i64, 4i64] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength25(): Int32 = region r {
        let x = [1.0f32] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength26(): Int32 = region r {
        let x = [1.0f32, 2.0f32, 3.0f32] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength27(): Int32 = region r {
        let x = [1.0f32, 2.0f32, 3.0f32, 4.0f32] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength28(): Int32 = region r {
        let x = [1.0f64] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength29(): Int32 = region r {
        let x = [1.0f64, 2.0f64, 3.0f64] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength30(): Int32 = region r {
        let x = [1.0f64, 2.0f64, 3.0f64, 4.0f64] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength31(): Int32 = region r {
        let x = ["Hello"] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength32(): Int32 = region r {
        let x = ["Hello", "World"] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength33(): Int32 = region r {
        let x = ["World", "Hello", "World"] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength34(): Int32 = region r {
        let x = [[1, 2, 3]] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength35(): Int32 = region r {
        let x = [[1, 2], [1, 2]] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength36(): Int32 = region r {
        let x = [[1], [1, 2, 3, 4, 5, 6, 7, 8]] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength37(): Int32 = region r {
        let x = [[1, 2, 3, 4, 5], [2]] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength38(): Int32 = region r {
        let x = [[1, 2, 3], [1, 2, 3], [1, 2, 3]] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength39(): Int32 = region r {
        let x = [[None, Some(1), None], [Some(2), Some(45), Some(9), None]] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength40(): Int32 = region r {
        let x = [(1, 2), (3, 4)] @ r;
        $ARRAY_LENGTH$(x)
    }

    @test
    def testArrayLength41(): Int32 = region r {
        let x = [[1 :: Nil], [3 :: 4 :: 5 :: Nil]] @ r;
        $ARRAY_LENGTH$(x)
    }
}
