mod Test.Type.Void {

    // We check that `Void` is given a polymorphic type when used as the type of an effect operation.

    pub eff Throw {
        pub def throw(): Void
    }

    pub def voidable01(): Void \ Throw = do Throw.throw()

    pub def voidable02(): a \ Throw = do Throw.throw()

    @Test
    pub def testVoid01(): Unit =
        try {
            do Throw.throw()
        } with Throw {
            def throw(_k) = ()
        }

    @Test
    pub def testVoid02(): Unit =
        try {
            if(do Throw.throw()) () else ()
        } with Throw {
            def throw(_k) = ()
        }

    @Test
    pub def testVoid03(): Unit =
        try {
            if(true) (do Throw.throw()) else ()
        } with Throw {
            def throw(_k) = ()
        }

    @Test
    pub def testVoid04(): Unit =
        try {
            discard (do Throw.throw() + 1); ()
        } with Throw {
            def throw(_k) = ()
        }

}
