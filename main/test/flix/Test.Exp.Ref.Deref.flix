mod Test.Exp.Ref.Deref {

    @test
    def testDeref01(): Bool = region rc {
        let l = Ref.fresh(rc, ());
        Ref.get(l) == ()
    }

    @test
    def testDeref02(): Bool = region rc {
        let l = Ref.fresh(rc, true);
        Ref.get(l) == true
    }

    @test
    def testDeref03(): Bool = region rc {
        let l = Ref.fresh(rc, false);
        Ref.get(l) == false
    }

    @test
    def testDeref04(): Bool = region rc {
        let l = Ref.fresh(rc, 'a');
        Ref.get(l) == 'a'
    }

    @test
    def testDeref05(): Bool = region rc {
        let l = Ref.fresh(rc, 123.456f32);
        Ref.get(l) == 123.456f32
    }

    @test
    def testDeref06(): Bool = region rc {
        let l = Ref.fresh(rc, 123.456f64);
        Ref.get(l) == 123.456f64
    }

    @test
    def testDeref07(): Bool = region rc {
        let l = Ref.fresh(rc, 42i8);
        Ref.get(l) == 42i8
    }

    @test
    def testDeref08(): Bool = region rc {
        let l = Ref.fresh(rc, 42i16);
        Ref.get(l) == 42i16
    }

    @test
    def testDeref09(): Bool = region rc {
        let l = Ref.fresh(rc, 42i32);
        Ref.get(l) == 42i32
    }

    @test
    def testDeref10(): Bool = region rc {
        let l = Ref.fresh(rc, 42i64);
        Ref.get(l) == 42i64
    }

    @test
    def testDeref11(): Bool = region rc {
        let l = Ref.fresh(rc, 42ii);
        Ref.get(l) == 42ii
    }

    @test
    def testDeref12(): Bool = region rc {
        let l = Ref.fresh(rc, "Hello World!");
        Ref.get(l) == "Hello World!"
    }

    @test
    def testDeref13(): Bool = region rc {
        let l = Ref.fresh(rc, (1, 2, 3));
        Ref.get(l) == (1, 2, 3)
    }

    @test
    def testDeref14(): Bool = region rc {
        let l = Ref.fresh(rc, (None: Option[Int32]));
        Ref.get(l) == None
    }

    @test
    def testDeref15(): Bool = region rc {
        let l = Ref.fresh(rc, Some(42));
        Ref.get(l) == Some(42)
    }

    @test
    def testDeref16(): Bool = region rc {
        let l = Ref.fresh(rc, Ok(42));
        Ref.get((l: Ref[Result[Unit, _], _])) == Ok(42)
    }

    @test
    def testDeref17(): Bool = region rc {
        let l = Ref.fresh(rc, Err("Goodbye World!"));
        Ref.get((l: Ref[Result[_, Unit], _])) == Err("Goodbye World!")
    }

    @test
    def testDeref18(): Bool = region rc {
        let a = Array#{Ref.fresh(rc, 1)} @ rc;
        Ref.get($ARRAY_LOAD$(a, 0)) == 1
    }
}
