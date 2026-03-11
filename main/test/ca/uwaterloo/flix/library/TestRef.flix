mod TestRef {

    use Assert.{assertEq, assertNeq};

    @Test
    def get01(): Unit \ Assert = region rc {
        assertEq(expected = 1, Ref.get(Ref.fresh(rc, 1)))
    }

    @Test
    def get02(): Unit \ Assert = region rc {
        assertEq(expected = "a", Ref.get(Ref.fresh(rc, "a")))
    }

    @Test
    def get03(): Unit \ Assert = region rc {
        assertNeq(unexpected = "b", Ref.get(Ref.fresh(rc, "A")))
    }

    @Test
    def get04(): Unit \ Assert = region rc {
        assertEq(expected = "A", Ref.get(Ref.get(Ref.fresh(rc, Ref.fresh(rc, "A")))))
    }

    @Test
    def put01(): Unit \ Assert = region rc {
        let rf = Ref.fresh(rc, 1);
        Ref.put(2, rf);
        assertEq(expected = 2, Ref.get(rf))
    }

    @Test
    def put02(): Unit \ Assert = region rc {
        let rf = Ref.fresh(rc, "a");
        Ref.put("A", rf);
        assertEq(expected = "A", Ref.get(rf))
    }

    @Test
    def put03(): Unit \ Assert = region rc {
        let rf = Ref.fresh(rc, Ref.fresh(rc, "a"));
        Ref.put(Ref.fresh(rc, "A"), rf);
        assertEq(expected = "A", Ref.get(Ref.get(rf)))
    }

    @Test
    def transform01(): Unit \ Assert = region rc {
        let rf = Ref.fresh(rc, 1);
        Ref.transform(x -> x + 1, rf);
        assertEq(expected = 2, Ref.get(rf))
    }

    @Test
    def transform02(): Unit \ Assert = region rc {
        let rf = Ref.fresh(rc, "a");
        Ref.transform(_ -> "A", rf);
        assertEq(expected = "A", Ref.get(rf))
    }

    @Test
    def transform03(): Unit \ Assert = region rc {
        let rf = Ref.fresh(rc, Ref.fresh(rc, "a"));
        Ref.transform(rf1 -> { Ref.transform(_ -> "A", rf1) ; rf1 }, rf);
        assertEq(expected = "A", Ref.get(Ref.get(rf)))
    }

}
