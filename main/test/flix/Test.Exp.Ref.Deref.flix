mod Test.Exp.Ref.Deref {

    @test
    def testDeref01(): Bool = region rc {
        let l = ref () @ rc;
        deref l == ()
    }

    @test
    def testDeref02(): Bool = region rc {
        let l = ref true @ rc;
        deref l == true
    }

    @test
    def testDeref03(): Bool = region rc {
        let l = ref false @ rc;
        deref l == false
    }

    @test
    def testDeref04(): Bool = region rc {
        let l = ref 'a' @ rc;
        deref l == 'a'
    }

    @test
    def testDeref05(): Bool = region rc {
        let l = ref 123.456f32 @ rc;
        deref l == 123.456f32
    }

    @test
    def testDeref06(): Bool = region rc {
        let l = ref 123.456f64 @ rc;
        deref l == 123.456f64
    }

    @test
    def testDeref07(): Bool = region rc {
        let l = ref 42i8 @ rc;
        deref l == 42i8
    }

    @test
    def testDeref08(): Bool = region rc {
        let l = ref 42i16 @ rc;
        deref l == 42i16
    }

    @test
    def testDeref09(): Bool = region rc {
        let l = ref 42i32 @ rc;
        deref l == 42i32
    }

    @test
    def testDeref10(): Bool = region rc {
        let l = ref 42i64 @ rc;
        deref l == 42i64
    }

    @test
    def testDeref11(): Bool = region rc {
        let l = ref 42ii @ rc;
        deref l == 42ii
    }

    @test
    def testDeref12(): Bool = region rc {
        let l = ref "Hello World!" @ rc;
        deref l == "Hello World!"
    }

    @test
    def testDeref13(): Bool = region rc {
        let l = ref (1, 2, 3) @ rc;
        deref l == (1, 2, 3)
    }

    @test
    def testDeref14(): Bool = region rc {
        let l = ref None: Option[Int32] @ rc;
        deref l == None
    }

    @test
    def testDeref15(): Bool = region rc {
        let l = ref Some(42) @ rc;
        deref l == Some(42)
    }

    @test
    def testDeref16(): Bool = region rc {
        let l = ref Ok(42) @ rc;
        deref l: Ref[Result[Unit, _], _] == Ok(42)
    }

    @test
    def testDeref17(): Bool = region rc {
        let l = ref Err("Goodbye World!") @ rc;
        deref l: Ref[Result[_, Unit], _] == Err("Goodbye World!")
    }

    @test
    def testDeref18(): Bool = region rc {
        let a = Array#{ref 1 @ rc} @ rc;
        deref $ARRAY_LOAD$(a, 0) == 1
    }
}
