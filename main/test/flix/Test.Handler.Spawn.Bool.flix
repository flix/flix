mod Test.Handler.Spawn.Bool {

    eff Ask {
        pub def ask(x: Bool): Unit
    }

    eff Gen {
        pub def gen(): Bool
    }

    def runAsk(tx: Sender[Bool, rc]): Unit \ rc =
        try {
            do Ask.ask(true)
        } with Ask {
            def ask(x, _) = Channel.send(x, tx)
        }

    def runGen(tx: Sender[Bool, rc]): Unit \ rc =
        try {
            Channel.send(do Gen.gen(), tx)
        } with Gen {
            def gen(k) = k(true)
        }

    def runGenAsk(tx: Sender[Bool, rc]): Unit \ rc =
        try {
            do Ask.ask(
                try {
                    do Gen.gen()
                } with Gen {
                    def gen(k) = k(false)
                }
            )
        } with Ask {
            def ask(x, _) = Channel.send(not x, tx)
        }

    @Test
    def testSpawn01(): Bool \ IO =
        let result = region rc {
            let (tx, rx) = Channel.unbuffered(rc);
            spawn runGen(tx) @ rc;
            Channel.recv(rx)
        };
        Assert.eq(true, result)

    @Test
    def testSpawn02(): Bool \ IO =
        let result = region rc {
            let (tx, rx) = Channel.unbuffered(rc);
            spawn runAsk(tx) @ rc;
            Channel.recv(rx)
        };
        Assert.eq(true, result)

    @Test
    def testSpawn03(): Bool \ IO =
        let result = region rc {
            let (tx, rx) = Channel.unbuffered(rc);
            spawn runGenAsk(tx) @ rc;
            Channel.recv(rx)
        };
        Assert.eq(true, result)

}
