mod Test.Handler.NewObject.Int32 {

    import java.lang.Runnable

    eff Ask {
        pub def ask(x: Int32): Unit
    }

    eff Gen {
        pub def gen(): Int32
    }

    def newAskRunnable(r: Ref[Int32, rc]): Runnable \ IO = new Runnable {
        def run(_this: Runnable): Unit \ { IO , rc } =
            try {
              do Ask.ask(42)
            } with Ask {
                def ask(x, k) = k(Ref.put(x, r))
            }
    }

    def newGenRunnable(r: Ref[Int32, rc]): Runnable \ IO = new Runnable {
        def run(_this: Runnable): Unit \ { IO , rc } =
            try {
              Ref.put(do Gen.gen(), r)
            } with Gen {
                def gen(k) = k(42)
            }
    }

    @Test
    def testRunnable01(): Bool \ IO =
        import java.lang.Runnable.run(): Unit \ IO;
        region rc {
            let r = Ref.new(rc, 0);
            run(newAskRunnable(r));
            let result = Ref.get(r);
            Assert.eq(42, result)
        }

    @Test
    def testRunnable02(): Bool \ IO =
        import java.lang.Runnable.run(): Unit \ IO;
        region rc {
            let r = Ref.new(rc, 0);
            run(newGenRunnable(r));
            let result = Ref.get(r);
            Assert.eq(42, result)
        }

}
