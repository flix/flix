mod Test.Type.Void {

    // We check that `Void` is given a polymorphic type when used as the type of an effect operation.

    pub eff Exc {
        pub def raise(): Void
    }

    pub def voidable01(): Void \ Exc = do Exc.raise()

    pub def voidable02(): a \ Exc = do Exc.raise()

    @Test
    pub def testVoid01(): Unit =
        try {
            do Exc.raise()
        } with Exc {
            def raise(_k) = ()
        }

    @Test
    pub def testVoid02(): Unit =
        try {
            if(do Exc.raise()) () else ()
        } with Exc {
            def raise(_k) = ()
        }

    @Test
    pub def testVoid03(): Unit =
        try {
            if(true) (do Exc.raise()) else ()
        } with Exc {
            def raise(_k) = ()
        }

    @Test
    pub def testVoid04(): Unit =
        try {
            discard (do Exc.raise() + 1); ()
        } with Exc {
            def raise(_k) = ()
        }

}
