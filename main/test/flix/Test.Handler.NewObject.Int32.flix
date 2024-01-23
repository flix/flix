mod Test.Handler.NewObject.Int32 {

    import java.lang.Runnable

    eff Ask {
        pub def ask(x: Int32): Unit
    }

    def newAskRunnable(r: Ref[Int32, rc]): Runnable \ IO = new Runnable {
        def run(_this: Runnable): Unit \ { IO , rc} =
            try {
              do Ask.ask(42)
            } with Ask {
                def ask(x, k) = k(Ref.put(x, r))
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

}
