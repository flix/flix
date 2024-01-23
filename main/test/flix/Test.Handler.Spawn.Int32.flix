mod Test.Handler.Spawn.Int32 {

    eff Ask {
        pub def ask(): Int32
    }

    def run(tx: Sender[Int32, rc]): Unit \ rc = try {
        Channel.send(do Ask.ask(), tx)
    } with Ask {
        def ask(k) = k(42)
    }

    @Test
    def testSpawn01(): Bool \ IO =
        let result = region rc {
            let (tx, rx) = Channel.unbuffered(rc);
            spawn run(tx) @ rc;
            Channel.recv(rx)
        };
        Assert.eq(42, result)

}
