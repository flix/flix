mod Test.Exp.Index {

    @test
    def testIndexable01(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        outOfBoundsHandler(rc, () -> a[0] == 1)
    }

    @test
    def testIndexable02(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        outOfBoundsHandler(rc, () -> a[1] == 2)
    }

    @test
    def testIndexable03(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        outOfBoundsHandler(rc, () -> a[2] == 3)
    }

    @test
    def testIndexable04(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        outOfBoundsHandler(rc, () -> a[3] == 4)
    }

    @test
    def testIndexable05(): Bool = region rc {
        let a = Array#{1, 2, 3, 4} @ rc;
        not outOfBoundsHandler(rc, () -> a[4] == -1)
    }

    @test
    def testIndexable06(): Bool = region rc {
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
