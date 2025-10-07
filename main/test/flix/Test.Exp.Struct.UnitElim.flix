mod Test.Exp.Struct.UnitElim {

    //
    // Tests for structs with Unit fields (which are subject to elimination.)
    //

    struct Mixed1[r] {
        mut a: Unit,
        mut b: Int32
    }

    struct Mixed2[r] {
        mut x: Int32,
        mut y: Unit,
        mut z: String
    }

    struct AllUnit[r] {
        mut u: Unit,
        mut v: Unit,
        mut w: Unit
    }

    mod Mixed1 {

        @test
        def testNew01(): Unit =
            region rc {
                new Mixed1 @ rc { a = (), b = 42 };
                ()
            }

        @test
        def testNew02(): Unit =
            region rc {
                new Mixed1 @ rc { b = 99, a = () };
                ()
            }

        @test
        def testGet01(): Unit =
            region rc {
                let s = new Mixed1 @ rc { a = (), b = 42 };
                s->a
            }

        @test
        def testGet02(): Bool =
            region rc {
                let s = new Mixed1 @ rc { a = (), b = 42 };
                s->b == 42
            }

        @test
        def testGet03(): Bool =
            region rc {
                let s = new Mixed1 @ rc { b = 123, a = () };
                s->a == () and s->b == 123
            }

        @test
        def testPut01(): Unit =
            region rc {
                let s = new Mixed1 @ rc { a = (), b = 42 };
                s->a = ()
            }

        @test
        def testPut02(): Bool =
            region rc {
                let s = new Mixed1 @ rc { a = (), b = 42 };
                s->b = 99;
                s->b == 99
            }

        @test
        def testPut03(): Bool =
            region rc {
                let s = new Mixed1 @ rc { a = (), b = 10 };
                s->a = ();
                s->b = 20;
                s->a == () and s->b == 20
            }

    }

    mod Mixed2 {

        @test
        def testNew01(): Unit =
            region rc {
                new Mixed2 @ rc { x = 123, y = (), z = "hello" };
                ()
            }

        @test
        def testNew02(): Unit =
            region rc {
                new Mixed2 @ rc { z = "world", y = (), x = 456 };
                ()
            }

        @test
        def testGet01(): Unit =
            region rc {
                let s = new Mixed2 @ rc { x = 42, y = (), z = "test" };
                s->y
            }

        @test
        def testGet02(): Bool =
            region rc {
                let s = new Mixed2 @ rc { x = 42, y = (), z = "test" };
                s->x == 42 and s->z == "test"
            }

        @test
        def testGet03(): Bool =
            region rc {
                let s = new Mixed2 @ rc { x = 99, y = (), z = "hello" };
                let intVal = s->x;
                let unitVal = s->y;
                let strVal = s->z;
                intVal == 99 and unitVal == () and strVal == "hello"
            }

        @test
        def testGet04(): Bool =
            region rc {
                let s = new Mixed2 @ rc { z = "world", y = (), x = 123 };
                s->y == () and s->x == 123 and s->z == "world"
            }

        @test
        def testPut01(): Unit =
            region rc {
                let s = new Mixed2 @ rc { x = 42, y = (), z = "test" };
                s->y = ()
            }

        @test
        def testPut02(): Bool =
            region rc {
                let s = new Mixed2 @ rc { x = 42, y = (), z = "test" };
                s->x = 99;
                s->z = "updated";
                s->x == 99 and s->z == "updated"
            }

        @test
        def testPut03(): Bool =
            region rc {
                let s = new Mixed2 @ rc { x = 10, y = (), z = "hello" };
                s->y = ();
                s->x = 20;
                s->z = "world";
                s->y == () and s->x == 20 and s->z == "world"
            }

        @test
        def testPut04(): Bool =
            region rc {
                let s = new Mixed2 @ rc { z = "initial", y = (), x = 100 };
                s->x = 200;
                s->y = ();
                s->z = "final";
                s->y == () and s->x == 200 and s->z == "final"
            }

    }

    mod AllUnit {

        @test
        def testNew01(): Unit =
            region rc {
                new AllUnit @ rc { u = (), v = (), w = () };
                ()
            }

        @test
        def testNew02(): Unit =
            region rc {
                new AllUnit @ rc { w = (), u = (), v = () };
                ()
            }

        @test
        def testGet01(): Unit =
            region rc {
                let s = new AllUnit @ rc { u = (), v = (), w = () };
                s->u
            }

        @test
        def testGet02(): Bool =
            region rc {
                let s = new AllUnit @ rc { u = (), v = (), w = () };
                s->u == () and s->v == () and s->w == ()
            }

        @test
        def testPut01(): Unit =
            region rc {
                let s = new AllUnit @ rc { u = (), v = (), w = () };
                s->u = ();
                s->v = ();
                s->w = ()
            }

        @test
        def testPut02(): Bool =
            region rc {
                let s = new AllUnit @ rc { u = (), v = (), w = () };
                s->u = ();
                s->v = ();
                s->w = ();
                s->u == () and s->v == () and s->w == ()
            }

        @test
        def testPut03(): Bool =
            region rc {
                let s = new AllUnit @ rc { w = (), u = (), v = () };
                s->w = ();
                s->v = ();
                s->u = ();
                s->u == () and s->v == () and s->w == ()
            }

    }

}
