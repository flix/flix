mod Test.Portable.Ref {

    def assertTrue(cond: Bool, msg: String): Unit =
        if (cond) () else bug!(msg)

    @Test
    def basic01(): Unit = region rc {
        let rf = Ref.fresh(rc, 42);
        let _ = assertTrue(Ref.get(rf) == 42, "basic01 failed: expected get == 42.");
        Ref.put(43, rf);
        assertTrue(Ref.get(rf) == 43, "basic01 failed: expected get == 43 after put.")
    }

    @Test
    def transform01(): Unit = region rc {
        let rf = Ref.fresh(rc, 10);
        Ref.transform(x -> x + 1, rf);
        assertTrue(Ref.get(rf) == 11, "transform01 failed: expected get == 11 after transform.")
    }

    pub def runAll(): Unit = {
        let _ = basic01();
        let _ = transform01();
        ()
    }

}
