mod Test.Exp.IfThen {

    @test
    def testIfThen01(): Bool = region rc {
        let ref = Ref.fresh(rc, false);
        if (true) {
            Ref.put(true, ref)
        };
        Ref.get(ref)
    }

    @test
    def testIfThen02(): Bool = region rc {
        let ref = Ref.fresh(rc, true);
        if (false) {
            Ref.put(false, ref)
        };
        Ref.get(ref)
    }

    @test
    def testIfThen03(): Bool = region rc {
        let ref = Ref.fresh(rc, true);
        if (true) {
            if (false) {
                Ref.put(false, ref)
            }
        };
        Ref.get(ref)
    }

    @test
    def testIfThen04(): Bool = region rc {
        let ref = Ref.fresh(rc, true);
        if (true) {
            if (false) {
                Ref.put(false, ref)
            } else {
                Ref.put(true, ref)
            }
        };
        Ref.get(ref)
    }

    @test
    def testIfThen05(): Bool = region rc {
        let ref = Ref.fresh(rc, true);
        if (true) {
            if (true) {
                Ref.put(true, ref)
            } else {
                Ref.put(false, ref)
            }
        };
        Ref.get(ref)
    }

}
