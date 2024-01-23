mod Test.Handler.NewObject.Float64 {

    import java.lang.Runnable

    eff Ask {
        pub def ask(x: Float64): Unit
    }

    eff Gen {
        pub def gen(): Float64
    }

    def newAskRunnable(r: Ref[Float64, rc]): Runnable \ IO = new Runnable {
        def run(_this: Runnable): Unit \ { IO , rc } =
            try {
              do Ask.ask(42.0f64)
            } with Ask {
                def ask(x, _) = Ref.put(x, r)
            }
    }

    def newGenRunnable(r: Ref[Float64, rc]): Runnable \ IO = new Runnable {
        def run(_this: Runnable): Unit \ { IO , rc } =
            try {
              Ref.put(do Gen.gen(), r)
            } with Gen {
                def gen(k) = k(42.0f64)
            }
    }

    def newGenAskRunnable(r: Ref[Float64, rc]): Runnable \ IO = new Runnable {
        def run(_this: Runnable): Unit \ { IO , rc } =
            try {
                do Ask.ask(
                    try {
                        do Gen.gen()
                    } with Gen {
                        def gen(k) = k(42.0f64)
                    }
                )
            } with Ask {
                def ask(x, _) = Ref.put(x + x, r)
            }
    }

    @Test
    def testRunnable01(): Bool \ IO =
        import java.lang.Runnable.run(): Unit \ IO;
        region rc {
            let r = Ref.new(rc, 0.0f64);
            run(newAskRunnable(r));
            let result = Ref.get(r);
            Assert.eq(42.0f64, result)
        }

    @Test
    def testRunnable02(): Bool \ IO =
        import java.lang.Runnable.run(): Unit \ IO;
        region rc {
            let r = Ref.new(rc, 0.0f64);
            run(newGenRunnable(r));
            let result = Ref.get(r);
            Assert.eq(42.0f64, result)
        }

    @Test
    def testRunnable03(): Bool \ IO =
        import java.lang.Runnable.run(): Unit \ IO;
        region rc {
            let r = Ref.new(rc, 0.0f64);
            run(newGenAskRunnable(r));
            let result = Ref.get(r);
            Assert.eq(84.0f64, result)
        }

}
