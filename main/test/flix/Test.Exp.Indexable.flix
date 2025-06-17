mod Test.Exp.Indexable {

    @Test
    def array01(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        run a[0] == 1 with handle
    }

    @Test
    def array02(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        run a[1] == 2 with handle
    }

    @Test
    def array03(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        run a[2] == 3 with handle
    }

    @Test
    def array04(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        run a[3] == 4 with handle
    }

    @Test
    def array05(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        not run a[4] == -1 with handle
    }

    @Test
    def array06(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        not run a[-1] == -1 with handle
    }

    @Test
    def vector01(): Bool =
        let v = Vector#{1, 2, 3, 4};
        run v[0] == 1 with handle

    @Test
    def vector02(): Bool =
        let v = Vector#{1, 2, 3, 4};
        run v[1] == 2 with handle

    @Test
    def vector03(): Bool =
        let v = Vector#{1, 2, 3, 4};
        run v[2] == 3 with handle

    @Test
    def vector04(): Bool =
        let v = Vector#{1, 2, 3, 4};
        run v[3] == 4 with handle

    @Test
    def vector05(): Bool =
        let v = Vector#{1, 2, 3, 4};
        not run v[4] == -1 with handle

    @Test
    def vector06(): Bool =
        let v = Vector#{1, 2, 3, 4};
        not run v[-1] == -1 with handle

    @Test
    def map01(): Bool =
        let m = Map#{0 => 1, 1 => 2, 2 => 3, 3 => 4};
        run m[0] == 1 with handle

    @Test
    def map02(): Bool =
        let m = Map#{0 => 1, 1 => 2, 2 => 3, 3 => 4};
        run m[1] == 2 with handle

    @Test
    def map03(): Bool =
        let m = Map#{0 => 1, 1 => 2, 2 => 3, 3 => 4};
        run m[2] == 3 with handle

    @Test
    def map04(): Bool =
        let m = Map#{0 => 1, 1 => 2, 2 => 3, 3 => 4};
        run m[3] == 4 with handle

    @Test
    def map05(): Bool =
        let m = Map#{0 => 1, 1 => 2, 2 => 3, 3 => 4};
        not run m[4] == -1  with handle

    @Test
    def map06(): Bool =
        let m = Map#{0 => 1, 1 => 2, 2 => 3, 3 => 4};
        not run m[-1] == -1  with handle

    @Test
    def record01(): Bool =
        let r = { v = Vector#{1, 2, 3, 4} };
        run r#v[1] == 2 with handle

    @Test
    def record02(): Bool =
        let r = { r = { v = Vector#{1, 2, 3, 4} } };
        run r#r#v[1] == 2 with handle

    @Test
    def record03(): Bool =
        let r = { r = { v = Vector#{{ v = Vector#{1, 2, 3, 4} } } } };
        run r#r#v[0]#v[1] == 2 with handle

    @Test
    def record04(): Bool =
        let r = { r = { v = Vector#{Vector#{}, Vector#{{ v = Vector#{1, 2, 3, 4} }} } } };
        run r#r#v[1][0]#v[1] == 2 with handle

    @Test
    def multiple01(): Bool =
        let v = Vector#{Vector#{Vector#{1, 2, 3, 4}}};
        run v[0][0][2] == 3 with handle

    @Test
    def multiple02(): Bool = region rc {
        let v = Vector#{Vector#{Array#{1, 2, 3, 4} @ rc}};
        run v[0][0][2] == 3 with handle
    }

    @Test
    def multiple03(): Bool = region rc {
        let v = Vector#{Array#{Map#{0 => 1, 1 => 2, 2 => 3, 4 => 4}} @ rc};
        run v[0][0][2] == 3 with handle
    }

    @Test
    def genWithRecord01(): Bool =
        run f()()#v[0]()[0]#r#g()[2] == 3 with handle

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
