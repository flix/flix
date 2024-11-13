mod Test.Type.Void {

    // We check that `Void` is given a polymorphic type when used as the type of an effect operation.

    pub eff Exc {
        def raise(): Void
    }

    pub def voidable01(): Void \ Exc = Exc.raise()

    pub def voidable02(): a \ Exc = Exc.raise()

    @Test
    pub def testVoid01(): Unit =
        try {
            Exc.raise()
        } with Exc {
            def raise(_k) = ()
        }

    @Test
    pub def testVoid02(): Unit =
        try {
            if(Exc.raise()) () else ()
        } with Exc {
            def raise(_k) = ()
        }

    @Test
    pub def testVoid03(): Unit =
        try {
            if(true) (Exc.raise()) else ()
        } with Exc {
            def raise(_k) = ()
        }

    @Test
    pub def testVoid04(): Unit =
        try {
            discard (Exc.raise() + 1); ()
        } with Exc {
            def raise(_k) = ()
        }

}
