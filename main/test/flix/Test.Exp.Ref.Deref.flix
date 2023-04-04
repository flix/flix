mod Test.Exp.Ref.Deref {

    @test
    def testDeref01(): Bool = region r {
        let l = ref () @ r;
        deref l == ()
    }

    @test
    def testDeref02(): Bool = region r {
        let l = ref true @ r;
        deref l == true
    }

    @test
    def testDeref03(): Bool = region r {
        let l = ref false @ r;
        deref l == false
    }

    @test
    def testDeref04(): Bool = region r {
        let l = ref 'a' @ r;
        deref l == 'a'
    }

    @test
    def testDeref05(): Bool = region r {
        let l = ref 123.456f32 @ r;
        deref l == 123.456f32
    }

    @test
    def testDeref06(): Bool = region r {
        let l = ref 123.456f64 @ r;
        deref l == 123.456f64
    }

    @test
    def testDeref07(): Bool = region r {
        let l = ref 42i8 @ r;
        deref l == 42i8
    }

    @test
    def testDeref08(): Bool = region r {
        let l = ref 42i16 @ r;
        deref l == 42i16
    }

    @test
    def testDeref09(): Bool = region r {
        let l = ref 42i32 @ r;
        deref l == 42i32
    }

    @test
    def testDeref10(): Bool = region r {
        let l = ref 42i64 @ r;
        deref l == 42i64
    }

    @test
    def testDeref11(): Bool = region r {
        let l = ref 42ii @ r;
        deref l == 42ii
    }

    @test
    def testDeref12(): Bool = region r {
        let l = ref "Hello World!" @ r;
        deref l == "Hello World!"
    }

    @test
    def testDeref13(): Bool = region r {
        let l = ref (1, 2, 3) @ r;
        deref l == (1, 2, 3)
    }

    @test
    def testDeref14(): Bool = region r {
        let l = ref None: Option[Int32] @ r;
        deref l == None
    }

    @test
    def testDeref15(): Bool = region r {
        let l = ref Some(42) @ r;
        deref l == Some(42)
    }

    @test
    def testDeref16(): Bool = region r {
        let l = ref Ok(42) @ r;
        deref l: Ref[Result[Unit, _], _] == Ok(42)
    }

    @test
    def testDeref17(): Bool = region r {
        let l = ref Err("Goodbye World!") @ r;
        deref l: Ref[Result[_, Unit], _] == Err("Goodbye World!")
    }

    @test
    def testDeref18(): Bool = region r {
        let a = Array#{ref 1 @ r} @ r;
        deref $ARRAY_LOAD$(a, 0) == 1
    }
}
