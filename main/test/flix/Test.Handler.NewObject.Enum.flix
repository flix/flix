mod Test.Handler.NewObject.Enum {

    import java.lang.Runnable

    eff Ask {
        pub def ask(x: Option[Int32]): Unit
    }

    eff Gen {
        pub def gen(): Option[Int32]
    }

    def newAskRunnable(r: Ref[Option[Int32], rc]): Runnable \ IO = new Runnable {
        def run(_this: Runnable): Unit \ { IO , rc } =
            try {
              do Ask.ask(Some(42))
            } with Ask {
                def ask(x, _) = Ref.put(x, r)
            }
    }

    def newGenRunnable(r: Ref[Option[Int32], rc]): Runnable \ IO = new Runnable {
        def run(_this: Runnable): Unit \ { IO , rc } =
            try {
              Ref.put(do Gen.gen(), r)
            } with Gen {
                def gen(k) = k(Some(42))
            }
    }

    def newGenAskRunnable(r: Ref[Option[Int32], rc]): Runnable \ IO = new Runnable {
        def run(_this: Runnable): Unit \ { IO , rc } =
            try {
                do Ask.ask(
                    try {
                        do Gen.gen()
                    } with Gen {
                        def gen(k) = k(Some(42))
                    }
                )
            } with Ask {
                def ask(x, _) = match x {
                    case Some(x1) => Ref.put(Some(x1 + x1), r)
                    case None     => unreachable!()
                }
            }
    }

    @Test
    def testRunnable01(): Bool \ IO =
        import java.lang.Runnable.run(): Unit \ IO;
        region rc {
            let r = Ref.new(rc, None);
            run(newAskRunnable(r));
            let result = Ref.get(r);
            Assert.eq(Some(42), result)
        }

    @Test
    def testRunnable02(): Bool \ IO =
        import java.lang.Runnable.run(): Unit \ IO;
        region rc {
            let r = Ref.new(rc, None);
            run(newGenRunnable(r));
            let result = Ref.get(r);
            Assert.eq(Some(42), result)
        }

    @Test
    def testRunnable03(): Bool \ IO =
        import java.lang.Runnable.run(): Unit \ IO;
        region rc {
            let r = Ref.new(rc, None);
            run(newGenAskRunnable(r));
            let result = Ref.get(r);
            Assert.eq(Some(84), result)
        }

}
