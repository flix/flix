mod Test.Handler.NewObject.AtomicInteger {

    import java.lang.Runnable

    type alias AtomicInteger = ##java.util.concurrent.atomic.AtomicInteger

    def newAtomicInteger(n: Int32): AtomicInteger =
        import new java.util.concurrent.atomic.AtomicInteger(Int32): AtomicInteger \ {} as mk;
        mk(n)

    def toInt(n: AtomicInteger): Int32 =
        import java.util.concurrent.atomic.AtomicInteger.intValue(): Int32 \ {} as intValue;
        intValue(n)

    eff Ask {
        pub def ask(x: AtomicInteger): Unit
    }

    eff Gen {
        pub def gen(): AtomicInteger
    }

    def newAskRunnable(r: Ref[AtomicInteger, rc]): Runnable \ IO = new Runnable {
        def run(_this: Runnable): Unit \ { IO , rc } =
            try {
              do Ask.ask(newAtomicInteger(42))
            } with Ask {
                def ask(x, _) = Ref.put(x, r)
            }
    }

    def newGenRunnable(r: Ref[AtomicInteger, rc]): Runnable \ IO = new Runnable {
        def run(_this: Runnable): Unit \ { IO , rc } =
            try {
              Ref.put(do Gen.gen(), r)
            } with Gen {
                def gen(k) = k(newAtomicInteger(42))
            }
    }

    def newGenAskRunnable(r: Ref[AtomicInteger, rc]): Runnable \ IO = new Runnable {
        def run(_this: Runnable): Unit \ { IO , rc } =
            try {
                do Ask.ask(
                    try {
                        do Gen.gen()
                    } with Gen {
                        def gen(k) = k(newAtomicInteger(42))
                    }
                )
            } with Ask {
                def ask(x, _) = Ref.put(newAtomicInteger(toInt(x) + toInt(x)), r)
            }
    }

    @Test
    def testRunnable01(): Bool \ IO =
        import java.lang.Runnable.run(): Unit \ IO;
        region rc {
            let r = Ref.new(rc, newAtomicInteger(0));
            run(newAskRunnable(r));
            let result = toInt(Ref.get(r));
            Assert.eq(42, result)
        }

    @Test
    def testRunnable02(): Bool \ IO =
        import java.lang.Runnable.run(): Unit \ IO;
        region rc {
            let r = Ref.new(rc, newAtomicInteger(0));
            run(newGenRunnable(r));
            let result = toInt(Ref.get(r));
            Assert.eq(42, result)
        }

    @Test
    def testRunnable03(): Bool \ IO =
        import java.lang.Runnable.run(): Unit \ IO;
        region rc {
            let r = Ref.new(rc, newAtomicInteger(0));
            run(newGenAskRunnable(r));
            let result = toInt(Ref.get(r));
            Assert.eq(84, result)
        }

}
