mod Test.Handler.NewObject.Unit {

    import java.lang.Runnable

    eff Ask {
        pub def ask(x: Unit): Unit
    }

    eff Gen {
        pub def gen(): Unit
    }

    def newAskRunnable(r: Ref[Unit, rc]): Runnable \ IO = new Runnable {
        def run(_this: Runnable): Unit \ { IO , rc } =
            try {
              do Ask.ask(())
            } with Ask {
                def ask(x, _) = Ref.put(x, r)
            }
    }

    def newGenRunnable(r: Ref[Unit, rc]): Runnable \ IO = new Runnable {
        def run(_this: Runnable): Unit \ { IO , rc } =
            try {
              Ref.put(do Gen.gen(), r)
            } with Gen {
                def gen(k) = k(())
            }
    }

    def newGenAskRunnable(r: Ref[Unit, rc]): Runnable \ IO = new Runnable {
        def run(_this: Runnable): Unit \ { IO , rc } =
            try {
                do Ask.ask(
                    try {
                        do Gen.gen()
                    } with Gen {
                        def gen(k) = k(())
                    }
                )
            } with Ask {
                def ask(x, _) = Ref.put(if (x == ()) () else unreachable!(), r)
            }
    }

    @Test
    def testRunnable01(): Bool \ IO =
        import java.lang.Runnable.run(): Unit \ IO;
        region rc {
            let r = Ref.new(rc, ());
            run(newAskRunnable(r));
            let result = Ref.get(r);
            Assert.eq((), result)
        }

    @Test
    def testRunnable02(): Bool \ IO =
        import java.lang.Runnable.run(): Unit \ IO;
        region rc {
            let r = Ref.new(rc, ());
            run(newGenRunnable(r));
            let result = Ref.get(r);
            Assert.eq((), result)
        }

    @Test
    def testRunnable03(): Bool \ IO =
        import java.lang.Runnable.run(): Unit \ IO;
        region rc {
            let r = Ref.new(rc, ());
            run(newGenAskRunnable(r));
            let result = Ref.get(r);
            Assert.eq((), result)
        }

}
