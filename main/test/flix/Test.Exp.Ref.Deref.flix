mod Test.Exp.Ref.Deref {

    @Test
    def testDeref01(): Bool = region rc {
        let l = ref () @ rc;
        deref l == ()
    }

    @Test
    def testDeref02(): Bool = region rc {
        let l = ref true @ rc;
        deref l == true
    }

    @Test
    def testDeref03(): Bool = region rc {
        let l = ref false @ rc;
        deref l == false
    }

    @Test
    def testDeref04(): Bool = region rc {
        let l = ref 'a' @ rc;
        deref l == 'a'
    }

    @Test
    def testDeref05(): Bool = region rc {
        let l = ref 123.456f32 @ rc;
        deref l == 123.456f32
    }

    @Test
    def testDeref06(): Bool = region rc {
        let l = ref 123.456f64 @ rc;
        deref l == 123.456f64
    }

    @Test
    def testDeref07(): Bool = region rc {
        let l = ref 42i8 @ rc;
        deref l == 42i8
    }

    @Test
    def testDeref08(): Bool = region rc {
        let l = ref 42i16 @ rc;
        deref l == 42i16
    }

    @Test
    def testDeref09(): Bool = region rc {
        let l = ref 42i32 @ rc;
        deref l == 42i32
    }

    @Test
    def testDeref10(): Bool = region rc {
        let l = ref 42i64 @ rc;
        deref l == 42i64
    }

    @Test
    def testDeref11(): Bool = region rc {
        let l = ref 42ii @ rc;
        deref l == 42ii
    }

    @Test
    def testDeref12(): Bool = region rc {
        let l = ref "Hello World!" @ rc;
        deref l == "Hello World!"
    }

    @Test
    def testDeref13(): Bool = region rc {
        let l = ref (1, 2, 3) @ rc;
        deref l == (1, 2, 3)
    }

    @Test
    def testDeref14(): Bool = region rc {
        let l = ref None: Option[Int32] @ rc;
        deref l == None
    }

    @Test
    def testDeref15(): Bool = region rc {
        let l = ref Some(42) @ rc;
        deref l == Some(42)
    }

    @Test
    def testDeref16(): Bool = region rc {
        let l = ref Ok(42) @ rc;
        deref l: Ref[Result[Unit, _], _] == Ok(42)
    }

    @Test
    def testDeref17(): Bool = region rc {
        let l = ref Err("Goodbye World!") @ rc;
        deref l: Ref[Result[_, Unit], _] == Err("Goodbye World!")
    }

    @Test
    def testDeref18(): Bool = region rc {
        let a = Array#{ref 1 @ rc} @ rc;
        deref $ARRAY_LOAD$(a, 0) == 1
    }
}
