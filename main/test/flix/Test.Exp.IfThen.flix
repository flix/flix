mod Test.Exp.IfThen {

    use Assert.assertTrue

    @Test
    def testIfThen01(): Unit \ Assert = region rc {
        let ref = Ref.fresh(rc, false);
        if (true) {
            Ref.put(true, ref)
        };
        assertTrue(Ref.get(ref))
    }

    @Test
    def testIfThen02(): Unit \ Assert = region rc {
        let ref = Ref.fresh(rc, true);
        if (false) {
            Ref.put(false, ref)
        };
        assertTrue(Ref.get(ref))
    }

    @Test
    def testIfThen03(): Unit \ Assert = region rc {
        let ref = Ref.fresh(rc, true);
        if (true) {
            if (false) {
                Ref.put(false, ref)
            }
        };
        assertTrue(Ref.get(ref))
    }

    @Test
    def testIfThen04(): Unit \ Assert = region rc {
        let ref = Ref.fresh(rc, true);
        if (true) {
            if (false) {
                Ref.put(false, ref)
            } else {
                Ref.put(true, ref)
            }
        };
        assertTrue(Ref.get(ref))
    }

    @Test
    def testIfThen05(): Unit \ Assert = region rc {
        let ref = Ref.fresh(rc, true);
        if (true) {
            if (true) {
                Ref.put(true, ref)
            } else {
                Ref.put(false, ref)
            }
        };
        assertTrue(Ref.get(ref))
    }

}
