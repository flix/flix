mod Test.Exp.Index {

    @test
    def array01(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        outOfBoundsHandler(rc, () -> a[0] == 1)
    }

    @test
    def array02(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        outOfBoundsHandler(rc, () -> a[1] == 2)
    }

    @test
    def array03(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        outOfBoundsHandler(rc, () -> a[2] == 3)
    }

    @test
    def array04(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        outOfBoundsHandler(rc, () -> a[3] == 4)
    }

    @test
    def array05(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        not outOfBoundsHandler(rc, () -> a[4] == -1)
    }

    @test
    def array06(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        not outOfBoundsHandler(rc, () -> a[-1] == -1)
    }

    def outOfBoundsHandler(_: Region[rc], f: Unit -> Bool \ OutOfBounds + rc): Bool = {
        try {
            f()
        } with OutOfBounds {
            def outOfBounds(_, _) = false
        }
    }
}
