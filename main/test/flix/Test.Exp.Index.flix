mod Test.Exp.Index {

    @Test
    def array01(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        regionedHandler(rc, () -> a[0] == 1)
    }

    @Test
    def array02(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        regionedHandler(rc, () -> a[1] == 2)
    }

    @Test
    def array03(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        regionedHandler(rc, () -> a[2] == 3)
    }

    @Test
    def array04(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        regionedHandler(rc, () -> a[3] == 4)
    }

    @Test
    def array05(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        not regionedHandler(rc, () -> a[4] == -1)
    }

    @Test
    def array06(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        not regionedHandler(rc, () -> a[-1] == -1)
    }

    @Test
    def vector01(): Bool =
        let v = Vector#{1, 2, 3, 4};
        handler(() -> v[0] == 1)

    @Test
    def vector02(): Bool =
        let v = Vector#{1, 2, 3, 4};
        handler(() -> v[1] == 2)

    @Test
    def vector03(): Bool =
        let v = Vector#{1, 2, 3, 4};
        handler(() -> v[2] == 3)

    @Test
    def vector04(): Bool =
        let v = Vector#{1, 2, 3, 4};
        handler(() -> v[3] == 4)

    @Test
    def vector05(): Bool =
        let v = Vector#{1, 2, 3, 4};
        not handler(() -> v[4] == -1)

    @Test
    def vector06(): Bool =
        let v = Vector#{1, 2, 3, 4};
        not handler(() -> v[-1] == -1)

    @Test
    def map01(): Bool =
        let m = Map#{0 => 1, 1 => 2, 2 => 3, 3 => 4};
        handler(() -> checked_ecast (m[0] == 1))

    @Test
    def map02(): Bool =
        let m = Map#{0 => 1, 1 => 2, 2 => 3, 3 => 4};
        handler(() -> checked_ecast (m[1] == 2))

    @Test
    def map03(): Bool =
        let m = Map#{0 => 1, 1 => 2, 2 => 3, 3 => 4};
        handler(() -> checked_ecast (m[2] == 3))

    @Test
    def map04(): Bool =
        let m = Map#{0 => 1, 1 => 2, 2 => 3, 3 => 4};
        handler(() -> checked_ecast (m[3] == 4))

    @Test
    def map05(): Bool =
        let m = Map#{0 => 1, 1 => 2, 2 => 3, 3 => 4};
        not handler(() -> checked_ecast (m[4] == -1))

    @Test
    def map06(): Bool =
        let m = Map#{0 => 1, 1 => 2, 2 => 3, 3 => 4};
        not handler(() -> checked_ecast (m[-1] == -1))

    @Test
    def record01(): Bool =
        let r = { v = Vector#{1, 2, 3, 4} };
        handler(() -> r#v[1] == 2)

    @Test
    def record02(): Bool =
        let r = { r = { v = Vector#{1, 2, 3, 4} } };
        handler(() -> r#r#v[1] == 2)

    @Test
    def record03(): Bool =
        let r = { r = { v = Vector#{{ v = Vector#{1, 2, 3, 4} } } } };
        handler(() -> r#r#v[0]#v[1] == 2)

    @Test
    def record04(): Bool =
        let r = { r = { v = Vector#{Vector#{}, Vector#{{ v = Vector#{1, 2, 3, 4} }} } } };
        handler(() -> r#r#v[1][0]#v[1] == 2)

    @Test
    def multiple01(): Bool =
        let v = Vector#{Vector#{Vector#{1, 2, 3, 4}}};
        handler(() -> v[0][0][2] == 3)

    @Test
    def multiple02(): Bool = region rc {
        let v = Vector#{Vector#{Array#{1, 2, 3, 4} @ rc}};
        regionedHandler(rc, () -> v[0][0][2] == 3)
    }

    @Test
    def multiple03(): Bool = region rc {
        let v = Vector#{Array#{Map#{0 => 1, 1 => 2, 2 => 3, 4 => 4}} @ rc};
        regionedHandler(rc, () -> v[0][0][2] == 3)
    }

    @Test
    def genWithRecord01(): Bool =
        handler(() -> f()()#v[0]#r#g()[2] == 3)

    def f(): Unit -> { v = Vector[Unit -> Vector[{ r = { g = Unit -> Map[Int32, Int32] } }]] } =
        () -> {
            v = Vector#{() -> Vector#{{ r = { g = () -> Map#{0 => 1, 2 => 3} } }}}
        }

    def handler(f: Unit -> Bool \ OutOfBounds + KeyNotFound - a): Bool = {
        try {
            f()
        } with OutOfBounds {
            def outOfBounds(_, _) = false
        } with KeyNotFound {
            def keyNotFound(_, _) = false
        }
    }

    def regionedHandler(_: Region[rc], f: Unit -> Bool \ rc + OutOfBounds + KeyNotFound - a): Bool = {
        try {
            f()
        } with OutOfBounds {
            def outOfBounds(_, _) = false
        } with KeyNotFound {
            def keyNotFound(_, _) = false
        }
    }
}
