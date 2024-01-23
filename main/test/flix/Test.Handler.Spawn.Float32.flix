mod Test.Handler.Spawn.Float32 {

    eff Ask {
        pub def ask(x: Float32): Unit
    }

    eff Gen {
        pub def gen(): Float32
    }

    def runAsk(tx: Sender[Float32, rc]): Unit \ rc =
        try {
            do Ask.ask(42.0f32)
        } with Ask {
            def ask(x, _) = Channel.send(x, tx)
        }

    def runGen(tx: Sender[Float32, rc]): Unit \ rc =
        try {
            Channel.send(do Gen.gen(), tx)
        } with Gen {
            def gen(k) = k(42.0f32)
        }

    def runGenAsk(tx: Sender[Float32, rc]): Unit \ rc =
        try {
            try {
                do Ask.ask(do Gen.gen())
            } with Gen {
                def gen(k) = k(42.0f32)
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
        Assert.eq(42.0f32, result)

    @Test
    def testSpawn02(): Bool \ IO =
        let result = region rc {
            let (tx, rx) = Channel.unbuffered(rc);
            spawn runAsk(tx) @ rc;
            Channel.recv(rx)
        };
        Assert.eq(42.0f32, result)

    @Test
    def testSpawn03(): Bool \ IO =
        let result = region rc {
            let (tx, rx) = Channel.unbuffered(rc);
            spawn runGenAsk(tx) @ rc;
            Channel.recv(rx)
        };
        Assert.eq(84.0f32, result)

}
