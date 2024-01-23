mod Test.Handler.Spawn.Int8 {

    eff Ask {
        pub def ask(x: Int8): Unit
    }

    eff Gen {
        pub def gen(): Int8
    }

    def runAsk(tx: Sender[Int8, rc]): Unit \ rc =
        try {
            do Ask.ask(42i8)
        } with Ask {
            def ask(x, _) = Channel.send(x, tx)
        }

    def runGen(tx: Sender[Int8, rc]): Unit \ rc =
        try {
            Channel.send(do Gen.gen(), tx)
        } with Gen {
            def gen(k) = k(42i8)
        }

    def runGenAsk(tx: Sender[Int8, rc]): Unit \ rc =
        try {
            do Ask.ask(
                try {
                    do Gen.gen()
                } with Gen {
                    def gen(k) = k(21i8)
                }
            )
        } with Ask {
            def ask(x, _) = Channel.send(21i8 + x, tx)
        }

    @Test
    def testSpawn01(): Bool \ IO =
        let result = region rc {
            let (tx, rx) = Channel.unbuffered(rc);
            spawn runGen(tx) @ rc;
            Channel.recv(rx)
        };
        Assert.eq(42i8, result)

    @Test
    def testSpawn02(): Bool \ IO =
        let result = region rc {
            let (tx, rx) = Channel.unbuffered(rc);
            spawn runAsk(tx) @ rc;
            Channel.recv(rx)
        };
        Assert.eq(42i8, result)

    @Test
    def testSpawn03(): Bool \ IO =
        let result = region rc {
            let (tx, rx) = Channel.unbuffered(rc);
            spawn runGenAsk(tx) @ rc;
            Channel.recv(rx)
        };
        Assert.eq(42i8, result)

}
