mod Test.Handler.NewObject.Int8 {

    import java.lang.Runnable

    eff Ask {
        pub def ask(x: Int8): Unit
    }

    eff Gen {
        pub def gen(): Int8
    }

    def newAskRunnable(r: Ref[Int8, rc]): Runnable \ IO = new Runnable {
        def run(_this: Runnable): Unit \ { IO , rc } =
            try {
              do Ask.ask(42i8)
            } with Ask {
                def ask(x, _) = Ref.put(x, r)
            }
    }

    def newGenRunnable(r: Ref[Int8, rc]): Runnable \ IO = new Runnable {
        def run(_this: Runnable): Unit \ { IO , rc } =
            try {
              Ref.put(do Gen.gen(), r)
            } with Gen {
                def gen(k) = k(42i8)
            }
    }

    def newGenAskRunnable(r: Ref[Int8, rc]): Runnable \ IO = new Runnable {
        def run(_this: Runnable): Unit \ { IO , rc } =
            try {
                do Ask.ask(
                    try {
                        do Gen.gen()
                    } with Gen {
                        def gen(k) = k(42i8)
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
            let r = Ref.new(rc, 0i8);
            run(newAskRunnable(r));
            let result = Ref.get(r);
            Assert.eq(42i8, result)
        }

    @Test
    def testRunnable02(): Bool \ IO =
        import java.lang.Runnable.run(): Unit \ IO;
        region rc {
            let r = Ref.new(rc, 0i8);
            run(newGenRunnable(r));
            let result = Ref.get(r);
            Assert.eq(42i8, result)
        }

    @Test
    def testRunnable03(): Bool \ IO =
        import java.lang.Runnable.run(): Unit \ IO;
        region rc {
            let r = Ref.new(rc, 0i8);
            run(newGenAskRunnable(r));
            let result = Ref.get(r);
            Assert.eq(84i8, result)
        }

}
