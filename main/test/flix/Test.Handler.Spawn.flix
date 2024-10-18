mod Test.Handler.Spawn {

    eff Ask {
        pub def ask(x: Int32): Unit
    }

    eff Gen {
        pub def gen(): Int32
    }

    def runAsk(tx: Sender[Int32, rc]): Unit \ rc =
        try {
            Ask.ask(42)
        } with Ask {
            def ask(x, _) = Channel.send(x, tx)
        }

    def runGen(tx: Sender[Int32, rc]): Unit \ rc =
        try {
            Channel.send(Gen.gen(), tx)
        } with Gen {
            def gen(k) = k(42)
        }

    def runGenAsk(tx: Sender[Int32, rc]): Unit \ rc =
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
    def testSpawn01(): Bool \ IO =
        let result = region rc {
            let (tx, rx) = Channel.unbuffered(rc);
            spawn runGen(tx) @ rc;
            Channel.recv(rx)
        };
        Assert.eq(42, result)

    @Test
    def testSpawn02(): Bool \ IO =
        let result = region rc {
            let (tx, rx) = Channel.unbuffered(rc);
            spawn runAsk(tx) @ rc;
            Channel.recv(rx)
        };
        Assert.eq(42, result)

    @Test
    def testSpawn03(): Bool \ IO =
        let result = region rc {
            let (tx, rx) = Channel.unbuffered(rc);
            spawn runGenAsk(tx) @ rc;
            Channel.recv(rx)
        };
        Assert.eq(84, result)

}
