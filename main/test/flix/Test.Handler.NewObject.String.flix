mod Test.Handler.NewObject.String {

    import java.lang.Runnable

    eff Ask {
        pub def ask(x: String): Unit
    }

    eff Gen {
        pub def gen(): String
    }

    def newAskRunnable(r: Ref[String, rc]): Runnable \ IO = new Runnable {
        def run(_this: Runnable): Unit \ { IO , rc } =
            try {
              do Ask.ask("a")
            } with Ask {
                def ask(x, _) = Ref.put(x, r)
            }
    }

    def newGenRunnable(r: Ref[String, rc]): Runnable \ IO = new Runnable {
        def run(_this: Runnable): Unit \ { IO , rc } =
            try {
              Ref.put(do Gen.gen(), r)
            } with Gen {
                def gen(k) = k("a")
            }
    }

    def newGenAskRunnable(r: Ref[String, rc]): Runnable \ IO = new Runnable {
        def run(_this: Runnable): Unit \ { IO , rc } =
            try {
                do Ask.ask(
                    try {
                        do Gen.gen()
                    } with Gen {
                        def gen(k) = k("a")
                    }
                )
            } with Ask {
                def ask(x, _) = Ref.put(next(x), r)
            }
    }

    def next(c: String): String = match c {
        case "a" => "b"
        case "b" => "a"
        case _   => unreachable!()
    }

    @Test
    def testRunnable01(): Bool \ IO =
        import java.lang.Runnable.run(): Unit \ IO;
        region rc {
            let r = Ref.new(rc, "z");
            run(newAskRunnable(r));
            let result = Ref.get(r);
            Assert.eq("a", result)
        }

    @Test
    def testRunnable02(): Bool \ IO =
        import java.lang.Runnable.run(): Unit \ IO;
        region rc {
            let r = Ref.new(rc, "z");
            run(newGenRunnable(r));
            let result = Ref.get(r);
            Assert.eq("a", result)
        }

    @Test
    def testRunnable03(): Bool \ IO =
        import java.lang.Runnable.run(): Unit \ IO;
        region rc {
            let r = Ref.new(rc, "z");
            run(newGenAskRunnable(r));
            let result = Ref.get(r);
            Assert.eq("b", result)
        }

}
