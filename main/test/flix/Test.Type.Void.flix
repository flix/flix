mod Test.Type.Void {

    // We check that `Void` is given a polymorphic type when used as the type of an effect operation.

    pub eff Raise {
        pub def raise(): Void
    }

    pub def voidable01(): Void \ Raise = do Raise.raise()

    pub def voidable02(): a \ Raise = do Raise.raise()

    @Test
    pub def testVoid01(): Unit =
        try {
            do Raise.raise()
        } with Raise {
            def raise(_k) = ()
        }

    @Test
    pub def testVoid02(): Unit =
        try {
            if(do Raise.raise()) () else ()
        } with Raise {
            def raise(_k) = ()
        }

    @Test
    pub def testVoid03(): Unit =
        try {
            if(true) (do Raise.raise()) else ()
        } with Raise {
            def raise(_k) = ()
        }

    @Test
    pub def testVoid04(): Unit =
        try {
            discard (do Raise.raise() + 1); ()
        } with Raise {
            def raise(_k) = ()
        }

}
