mod Test.Handler.NewObject {

    import java.lang.Runnable

    eff Ask {
        pub def ask(x: Int32): Unit
    }

    eff Gen {
        pub def gen(): Int32
    }

    def newAskRunnable(r: Ref[Int32, rc]): Runnable \ IO = new Runnable {
        def run(_this: Runnable): Unit \ { rc } =
            try {
              Ask.ask(42)
            } with Ask {
                def ask(x, _) = Ref.put(x, r)
            }
    }

    def newGenRunnable(r: Ref[Int32, rc]): Runnable \ IO = new Runnable {
        def run(_this: Runnable): Unit \ { rc } =
            try {
              Ref.put(Gen.gen(), r)
            } with Gen {
                def gen(k) = k(42)
            }
    }

    def newGenAskRunnable(r: Ref[Int32, rc]): Runnable \ IO = new Runnable {
        def run(_this: Runnable): Unit \ { rc } =
            try {
                Ask.ask(
                    try {
                        Gen.gen()
                    } with Gen {
                        def gen(k) = k(42)
                    }
                )
            } with Ask {
                def ask(x, _) = Ref.put(x + x, r)
            }
    }

    @Test
    def testRunnable01(): Bool \ IO =
        region rc {
            let r = Ref.fresh(rc, 0);
            newAskRunnable(r).run();
            let result = Ref.get(r);
            Assert.eq(42, result)
        }

    @Test
    def testRunnable02(): Bool \ IO =
        region rc {
            let r = Ref.fresh(rc, 0);
            newGenRunnable(r).run();
            let result = Ref.get(r);
            Assert.eq(42, result)
        }

    @Test
    def testRunnable03(): Bool \ IO =
        region rc {
            let r = Ref.fresh(rc, 0);
            newGenAskRunnable(r).run();
            let result = Ref.get(r);
            Assert.eq(84, result)
        }

}
