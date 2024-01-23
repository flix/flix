mod Test.Handler.Spawn.Int64 {

    eff Ask {
        pub def ask(x: Int64): Unit
    }

    eff Gen {
        pub def gen(): Int64
    }

    def runAsk(tx: Sender[Int64, rc]): Unit \ rc =
        try {
            do Ask.ask(42i64)
        } with Ask {
            def ask(x, _) = Channel.send(x, tx)
        }

    def runGen(tx: Sender[Int64, rc]): Unit \ rc =
        try {
            Channel.send(do Gen.gen(), tx)
        } with Gen {
            def gen(k) = k(42i64)
        }

    def runGenAsk(tx: Sender[Int64, rc]): Unit \ rc =
        try {
            do Ask.ask(
                try {
                    do Gen.gen()
                } with Gen {
                    def gen(k) = k(21i64)
                }
            )
        } with Ask {
            def ask(x, _) = Channel.send(21i64 + x, tx)
        }

    @Test
    def testSpawn01(): Bool \ IO =
        let result = region rc {
            let (tx, rx) = Channel.unbuffered(rc);
            spawn runGen(tx) @ rc;
            Channel.recv(rx)
        };
        Assert.eq(42i64, result)

    @Test
    def testSpawn02(): Bool \ IO =
        let result = region rc {
            let (tx, rx) = Channel.unbuffered(rc);
            spawn runAsk(tx) @ rc;
            Channel.recv(rx)
        };
        Assert.eq(42i64, result)

    @Test
    def testSpawn03(): Bool \ IO =
        let result = region rc {
            let (tx, rx) = Channel.unbuffered(rc);
            spawn runGenAsk(tx) @ rc;
            Channel.recv(rx)
        };
        Assert.eq(42i64, result)

}
