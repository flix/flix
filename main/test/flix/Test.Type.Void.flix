mod Test.Type.Void {

    // We check that `Void` is given a polymorphic type when used as the type of an effect operation.

    pub eff Exc {
        def raise(): Void
    }

    pub def voidable01(): Void \ Exc = Exc.raise()

    pub def voidable02(): a \ Exc = Exc.raise()

    @Test
    pub def testVoid01(): Unit =
        run {
            Exc.raise()
        } with handler Exc {
            def raise(_k) = ()
        }

    @Test
    pub def testVoid02(): Unit =
        run {
            if(Exc.raise()) () else ()
        } with handler Exc {
            def raise(_k) = ()
        }

    @Test
    pub def testVoid03(): Unit =
        run {
            if(true) (Exc.raise()) else ()
        } with handler Exc {
            def raise(_k) = ()
        }

    @Test
    pub def testVoid04(): Unit =
        run {
            discard (Exc.raise() + 1); ()
        } with handler Exc {
            def raise(_k) = ()
        }

}
