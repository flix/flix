mod Test.Handler.Spawn {

    eff Ask {
        def ask(x: Int32): Unit
    }

    eff Gen {
        def gen(): Int32
    }

    def runAsk(tx: Sender[Int32]): Unit \ Global =
        try {
            Ask.ask(42)
        } with Ask {
            def ask(x, _) = Channel.send(x, tx)
        }

    def runGen(tx: Sender[Int32]): Unit \ Global =
        try {
            Channel.send(Gen.gen(), tx)
        } with Gen {
            def gen(k) = k(42)
        }

    def runGenAsk(tx: Sender[Int32]): Unit \ Global =
        try {
            Ask.ask(
                try {
                    Gen.gen()
                } with Gen {
                    def gen(k) = k(42)
                }
            )
        } with Ask {
            def ask(x, _) = Channel.send(x + x, tx)
        }

    @Test
    def testSpawn01(): Bool \ Global + NonDet =
        let result = {
            let (tx, rx) = Channel.unbuffered();
            spawn runGen(tx);
            Channel.recv(rx)
        };
        Assert.eq(42, result)

    @Test
    def testSpawn02(): Bool \ Global + NonDet =
        let result = {
            let (tx, rx) = Channel.unbuffered();
            spawn runAsk(tx);
            Channel.recv(rx)
        };
        Assert.eq(42, result)

    @Test
    def testSpawn03(): Bool \ Global + NonDet =
        let result = {
            let (tx, rx) = Channel.unbuffered();
            spawn runGenAsk(tx);
            Channel.recv(rx)
        };
        Assert.eq(84, result)

}
