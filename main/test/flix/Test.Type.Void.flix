mod Test.Type.Void {

    // We check that `Void` can be used as where another type is expected.
    // -- But only for do expressions.

    pub eff Throw {
        pub def throw(): Void
    }

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
