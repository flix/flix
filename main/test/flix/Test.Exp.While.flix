mod Test.Exp.While {

    @test
    def testWhile01(): Bool = region rc {
        let c = Ref.fresh(rc, true);
        let count = Ref.fresh(rc, 0);
        while (Ref.get(c)) {
            Ref.transform(Add.add(1), count);
            Ref.put(false, c)
        };
        Ref.get(count) == 1
    }

    @test
    def testWhile02(): Bool = region rc {
        let c = Ref.fresh(rc, false);
        let count = Ref.fresh(rc, 0);
        while (Ref.get(c)) {
            Ref.transform(Add.add(1), count);
            Ref.put(false, c)
        };
        Ref.get(count) == 0
    }

    @test
    def testWhileNested01(): Bool = region rc {
        let c = Ref.fresh(rc, true);
        let countOuter = Ref.fresh(rc, 0);
        let countInner = Ref.fresh(rc, 0);
        while (Ref.get(c)) {
            Ref.transform(Add.add(1), countOuter);
            while (Ref.get(c)) {
                Ref.transform(Add.add(1), countInner);
                Ref.put(false, c)
            };
            Ref.put(false, c)
        };
        Ref.get(countOuter) == 1 and Ref.get(countInner) == 1
    }

    @test
    def testWhileNested02(): Bool = region rc {
        let c = Ref.fresh(rc, true);
        let countOuter = Ref.fresh(rc, 0);
        let countInner = Ref.fresh(rc, 0);
        while (Ref.get(c)) {
            Ref.transform(Add.add(1), countOuter);
            Ref.put(false, c);
            while (Ref.get(c)) {
                Ref.transform(Add.add(1), countInner);
                Ref.put(false, c)
            }
        };
        Ref.get(countOuter) == 1 and Ref.get(countInner) == 0
    }

    @test
    def testWhileMany01(): Bool = region rc {
        let c = Ref.fresh(rc, 42);
        let count = Ref.fresh(rc, 0);
        while (Ref.get(c) > 0) {
            Ref.transform(Add.add(1), count);
            Ref.transform(x -> x - 1, c)
        };
        Ref.get(count) == 42
    }

    @test
    def testWhileRec01(): Bool = region rc {
        recWhile(Ref.fresh(rc, 10)) == 0
    }

    def recWhile(c: Ref[Int32, r]): Int32 \ r= {
        Ref.transform(x -> x - 1, c);
        while (0 < Ref.get(c)) {
            recWhile(c)
        };
        Ref.get(c)
    }

    eff Break {
        def break(): Void
    }

    @test
    def testWhileWithEff(): Bool = {
        run {
            while(true) {
                Break.break()
            };
            false
        } with handler Break {
            def break(_) = true
        }
    }

}
