mod Test.Handler.Spawn.Enum {

    eff Ask {
        pub def ask(x: Option[Int32]): Unit
    }

    eff Gen {
        pub def gen(): Option[Int32]
    }

    def runAsk(tx: Sender[Option[Int32], rc]): Unit \ rc =
        try {
            do Ask.ask(Some(42))
        } with Ask {
            def ask(x, _) = Channel.send(x, tx)
        }

    def runGen(tx: Sender[Option[Int32], rc]): Unit \ rc =
        try {
            Channel.send(do Gen.gen(), tx)
        } with Gen {
            def gen(k) = k(Some(42))
        }

    def runGenAsk(tx: Sender[Option[Int32], rc]): Unit \ rc =
        try {
            try {
                do Ask.ask(do Gen.gen())
            } with Gen {
                def gen(k) = k(42)
            }
        } with Ask {
            def ask(x, _) = match x {
                case Some(x1) => Channel.send(Some(x1 + x1), tx)
                case None     => unreachable!()
            }
        }

    @Test
    def testSpawn01(): Bool \ IO =
        let result = region rc {
            let (tx, rx) = Channel.unbuffered(rc);
            spawn runGen(tx) @ rc;
            Channel.recv(rx)
        };
        Assert.eq(Some(42), result)

    @Test
    def testSpawn02(): Bool \ IO =
        let result = region rc {
            let (tx, rx) = Channel.unbuffered(rc);
            spawn runAsk(tx) @ rc;
            Channel.recv(rx)
        };
        Assert.eq(Some(42), result)

    @Test
    def testSpawn03(): Bool \ IO =
        let result = region rc {
            let (tx, rx) = Channel.unbuffered(rc);
            spawn runGenAsk(tx) @ rc;
            Channel.recv(rx)
        };
        Assert.eq(Some(84), result)

}
