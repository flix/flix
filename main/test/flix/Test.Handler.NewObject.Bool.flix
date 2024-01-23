mod Test.Handler.NewObject.Bool {

    import java.lang.Runnable

    eff Ask {
        pub def ask(x: Bool): Unit
    }

    eff Gen {
        pub def gen(): Bool
    }

    def newAskRunnable(r: Ref[Bool, rc]): Runnable \ IO = new Runnable {
        def run(_this: Runnable): Unit \ { IO , rc } =
            try {
              do Ask.ask(true)
            } with Ask {
                def ask(x, _) = Ref.put(x, r)
            }
    }

    def newGenRunnable(r: Ref[Bool, rc]): Runnable \ IO = new Runnable {
        def run(_this: Runnable): Unit \ { IO , rc } =
            try {
              Ref.put(do Gen.gen(), r)
            } with Gen {
                def gen(k) = k(true)
            }
    }

    def newGenAskRunnable(r: Ref[Bool, rc]): Runnable \ IO = new Runnable {
        def run(_this: Runnable): Unit \ { IO , rc } =
            try {
                do Ask.ask(
                    try {
                        do Gen.gen()
                    } with Gen {
                        def gen(k) = k(false)
                    }
                )
            } with Ask {
                def ask(x, _) = Ref.put(not x, r)
            }
    }

    @Test
    def testRunnable01(): Bool \ IO =
        import java.lang.Runnable.run(): Unit \ IO;
        region rc {
            let r = Ref.new(rc, false);
            run(newAskRunnable(r));
            let result = Ref.get(r);
            Assert.eq(true, result)
        }

    @Test
    def testRunnable02(): Bool \ IO =
        import java.lang.Runnable.run(): Unit \ IO;
        region rc {
            let r = Ref.new(rc, false);
            run(newGenRunnable(r));
            let result = Ref.get(r);
            Assert.eq(true, result)
        }

    @Test
    def testRunnable03(): Bool \ IO =
        import java.lang.Runnable.run(): Unit \ IO;
        region rc {
            let r = Ref.new(rc, false);
            run(newGenAskRunnable(r));
            let result = Ref.get(r);
            Assert.eq(true, result)
        }

}
