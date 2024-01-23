mod Test.Handler.Spawn.Char {

    eff Ask {
        pub def ask(x: Char): Unit
    }

    eff Gen {
        pub def gen(): Char
    }

    def runAsk(tx: Sender[Char, rc]): Unit \ rc =
        try {
            do Ask.ask('a')
        } with Ask {
            def ask(x, _) = Channel.send(x, tx)
        }

    def runGen(tx: Sender[Char, rc]): Unit \ rc =
        try {
            Channel.send(do Gen.gen(), tx)
        } with Gen {
            def gen(k) = k('a')
        }

    def runGenAsk(tx: Sender[Char, rc]): Unit \ rc =
        try {
            do Ask.ask(
                try {
                    do Gen.gen()
                } with Gen {
                    def gen(k) = k('b')
                }
            )
        } with Ask {
            def ask(x, _) = Channel.send(next(x), tx)
        }

    def next(c: Char): Char = match c {
        case 'a' => 'b'
        case 'b' => 'a'
        case _   => unreachable!()
    }

    @Test
    def testSpawn01(): Bool \ IO =
        let result = region rc {
            let (tx, rx) = Channel.unbuffered(rc);
            spawn runGen(tx) @ rc;
            Channel.recv(rx)
        };
        Assert.eq('a', result)

    @Test
    def testSpawn02(): Bool \ IO =
        let result = region rc {
            let (tx, rx) = Channel.unbuffered(rc);
            spawn runAsk(tx) @ rc;
            Channel.recv(rx)
        };
        Assert.eq('a', result)

    @Test
    def testSpawn03(): Bool \ IO =
        let result = region rc {
            let (tx, rx) = Channel.unbuffered(rc);
            spawn runGenAsk(tx) @ rc;
            Channel.recv(rx)
        };
        Assert.eq('a', result)

}
