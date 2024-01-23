mod Test.Handler.Spawn.Unit {

    eff Ask {
        pub def ask(x: Unit): Unit
    }

    eff Gen {
        pub def gen(): Unit
    }

    def runAsk(tx: Sender[Unit, rc]): Unit \ rc =
        try {
            do Ask.ask(())
        } with Ask {
            def ask(x, _) = Channel.send(x, tx)
        }

    def runGen(tx: Sender[Unit, rc]): Unit \ rc =
        try {
            Channel.send(do Gen.gen(), tx)
        } with Gen {
            def gen(k) = k(())
        }

    def runGenAsk(tx: Sender[Unit, rc]): Unit \ rc =
        try {
            do Ask.ask(
                try {
                    do Gen.gen()
                } with Gen {
                    def gen(k) = k(())
                }
            )
        } with Ask {
            def ask(x, _) = Channel.send(x, tx)
        }

    @Test
    def testSpawn01(): Bool \ IO =
        let result = region rc {
            let (tx, rx) = Channel.unbuffered(rc);
            spawn runGen(tx) @ rc;
            Channel.recv(rx)
        };
        Assert.eq((), result)

    @Test
    def testSpawn02(): Bool \ IO =
        let result = region rc {
            let (tx, rx) = Channel.unbuffered(rc);
            spawn runAsk(tx) @ rc;
            Channel.recv(rx)
        };
        Assert.eq((), result)

    @Test
    def testSpawn03(): Bool \ IO =
        let result = region rc {
            let (tx, rx) = Channel.unbuffered(rc);
            spawn runGenAsk(tx) @ rc;
            Channel.recv(rx)
        };
        Assert.eq((), result)

}
