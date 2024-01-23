mod Test.Handler.Spawn.Float64 {

    eff Ask {
        pub def ask(x: Float64): Unit
    }

    eff Gen {
        pub def gen(): Float64
    }

    def runAsk(tx: Sender[Float64, rc]): Unit \ rc =
        try {
            do Ask.ask(42.0f64)
        } with Ask {
            def ask(x, _) = Channel.send(x, tx)
        }

    def runGen(tx: Sender[Float64, rc]): Unit \ rc =
        try {
            Channel.send(do Gen.gen(), tx)
        } with Gen {
            def gen(k) = k(42.0f64)
        }

    def runGenAsk(tx: Sender[Float64, rc]): Unit \ rc =
        try {
            try {
                do Ask.ask(do Gen.gen())
            } with Gen {
                def gen(k) = k(42.0f64)
            }
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
        Assert.eq(42.0f64, result)

    @Test
    def testSpawn02(): Bool \ IO =
        let result = region rc {
            let (tx, rx) = Channel.unbuffered(rc);
            spawn runAsk(tx) @ rc;
            Channel.recv(rx)
        };
        Assert.eq(42.0f64, result)

    @Test
    def testSpawn03(): Bool \ IO =
        let result = region rc {
            let (tx, rx) = Channel.unbuffered(rc);
            spawn runGenAsk(tx) @ rc;
            Channel.recv(rx)
        };
        Assert.eq(84.0f64, result)

}
