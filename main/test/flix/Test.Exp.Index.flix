mod Test.Exp.Index {

    @test
    def array01(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        regionedHandler(rc, () -> checked_ecast (a[0] == 1))
    }

    @test
    def array02(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        regionedHandler(rc, () -> checked_ecast (a[1] == 2))
    }

    @test
    def array03(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        regionedHandler(rc, () -> checked_ecast (a[2] == 3))
    }

    @test
    def array04(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        regionedHandler(rc, () -> checked_ecast (a[3] == 4))
    }

    @test
    def array05(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        not regionedHandler(rc, () -> checked_ecast (a[4] == -1))
    }

    @test
    def array06(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        not regionedHandler(rc, () -> checked_ecast (a[-1] == -1))
    }

    @test
    def vector01(): Bool =
        let a = Vector#{1, 2, 3, 4};
        handler(() -> checked_ecast (a[0] == 1))

    @test
    def vector02(): Bool =
        let a = Vector#{1, 2, 3, 4};
        handler(() -> checked_ecast (a[1] == 2))

    @test
    def vector03(): Bool =
        let a = Vector#{1, 2, 3, 4};
        handler(() -> checked_ecast (a[2] == 3))

    @test
    def vector04(): Bool =
        let a = Vector#{1, 2, 3, 4};
        handler(() -> checked_ecast (a[3] == 4))

    @test
    def vector05(): Bool =
        let a = Vector#{1, 2, 3, 4};
        not handler(() -> checked_ecast (a[4] == -1))

    @test
    def vector06(): Bool =
        let a = Vector#{1, 2, 3, 4};
        not handler(() -> checked_ecast (a[-1] == -1))

    def handler(f: Unit -> Bool \ OutOfBounds + KeyNotFound): Bool = {
        try {
            f()
        } with OutOfBounds {
            def outOfBounds(_, _) = false
        } with KeyNotFound {
            def keyNotFound(_, _) = false
        }
    }

    def regionedHandler(_: Region[rc], f: Unit -> Bool \ OutOfBounds + KeyNotFound + rc): Bool = {
        try {
            f()
        } with OutOfBounds {
            def outOfBounds(_, _) = false
        } with KeyNotFound {
            def keyNotFound(_, _) = false
        }
    }
}
