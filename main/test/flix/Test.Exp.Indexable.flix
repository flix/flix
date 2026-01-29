mod Test.Exp.Indexable {

    use Assert.assertTrue

    @Test
    def array01(): Unit \ Assert = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        assertTrue(run a[0] == 1 with handle)
    }

    @Test
    def array02(): Unit \ Assert = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        assertTrue(run a[1] == 2 with handle)
    }

    @Test
    def array03(): Unit \ Assert = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        assertTrue(run a[2] == 3 with handle)
    }

    @Test
    def array04(): Unit \ Assert = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        assertTrue(run a[3] == 4 with handle)
    }

    @Test
    def array05(): Unit \ Assert = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        assertTrue(not run a[4] == -1 with handle)
    }

    @Test
    def array06(): Unit \ Assert = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        assertTrue(not run a[-1] == -1 with handle)
    }

    @Test
    def vector01(): Unit \ Assert =
        let v = Vector#{1, 2, 3, 4};
        assertTrue(run v[0] == 1 with handle)

    @Test
    def vector02(): Unit \ Assert =
        let v = Vector#{1, 2, 3, 4};
        assertTrue(run v[1] == 2 with handle)

    @Test
    def vector03(): Unit \ Assert =
        let v = Vector#{1, 2, 3, 4};
        assertTrue(run v[2] == 3 with handle)

    @Test
    def vector04(): Unit \ Assert =
        let v = Vector#{1, 2, 3, 4};
        assertTrue(run v[3] == 4 with handle)

    @Test
    def vector05(): Unit \ Assert =
        let v = Vector#{1, 2, 3, 4};
        assertTrue(not run v[4] == -1 with handle)

    @Test
    def vector06(): Unit \ Assert =
        let v = Vector#{1, 2, 3, 4};
        assertTrue(not run v[-1] == -1 with handle)

    @Test
    def map01(): Unit \ Assert =
        let m = Map#{0 => 1, 1 => 2, 2 => 3, 3 => 4};
        assertTrue(run m[0] == 1 with handle)

    @Test
    def map02(): Unit \ Assert =
        let m = Map#{0 => 1, 1 => 2, 2 => 3, 3 => 4};
        assertTrue(run m[1] == 2 with handle)

    @Test
    def map03(): Unit \ Assert =
        let m = Map#{0 => 1, 1 => 2, 2 => 3, 3 => 4};
        assertTrue(run m[2] == 3 with handle)

    @Test
    def map04(): Unit \ Assert =
        let m = Map#{0 => 1, 1 => 2, 2 => 3, 3 => 4};
        assertTrue(run m[3] == 4 with handle)

    @Test
    def map05(): Unit \ Assert =
        let m = Map#{0 => 1, 1 => 2, 2 => 3, 3 => 4};
        assertTrue(not run m[4] == -1  with handle)

    @Test
    def map06(): Unit \ Assert =
        let m = Map#{0 => 1, 1 => 2, 2 => 3, 3 => 4};
        assertTrue(not run m[-1] == -1  with handle)

    @Test
    def record01(): Unit \ Assert =
        let r = { v = Vector#{1, 2, 3, 4} };
        assertTrue(run r#v[1] == 2 with handle)

    @Test
    def record02(): Unit \ Assert =
        let r = { r = { v = Vector#{1, 2, 3, 4} } };
        assertTrue(run r#r#v[1] == 2 with handle)

    @Test
    def record03(): Unit \ Assert =
        let r = { r = { v = Vector#{{ v = Vector#{1, 2, 3, 4} } } } };
        assertTrue(run r#r#v[0]#v[1] == 2 with handle)

    @Test
    def record04(): Unit \ Assert =
        let r = { r = { v = Vector#{Vector#{}, Vector#{{ v = Vector#{1, 2, 3, 4} }} } } };
        assertTrue(run r#r#v[1][0]#v[1] == 2 with handle)

    @Test
    def multiple01(): Unit \ Assert =
        let v = Vector#{Vector#{Vector#{1, 2, 3, 4}}};
        assertTrue(run v[0][0][2] == 3 with handle)

    @Test
    def multiple02(): Unit \ Assert = region rc {
        let v = Vector#{Vector#{Array#{1, 2, 3, 4} @ rc}};
        assertTrue(run v[0][0][2] == 3 with handle)
    }

    @Test
    def multiple03(): Unit \ Assert = region rc {
        let v = Vector#{Array#{Map#{0 => 1, 1 => 2, 2 => 3, 4 => 4}} @ rc};
        assertTrue(run v[0][0][2] == 3 with handle)
    }

    @Test
    def genWithRecord01(): Unit \ Assert =
        assertTrue(run f()()#v[0]()[0]#r#g()[2] == 3 with handle)

    def f(): Unit -> { v = Vector[Unit -> Vector[{ r = { g = Unit -> Map[Int32, Int32] } }]] } =
        () -> {
            v = Vector#{() -> Vector#{{ r = { g = () -> Map#{0 => 1, 2 => 3} } }}}
        }

    def handle(f: Unit -> Bool \ ef): Bool \ ef - OutOfBounds - KeyNotFound = {
        run {
            f()
        } with handler OutOfBounds {
            def outOfBounds(_, _) = false
        } with handler KeyNotFound {
            def keyNotFound(_, _) = false
        }
    }
}
