mod Test.Portable.ArrayVector {

    def assertTrue(cond: Bool, msg: String): Unit =
        if (cond) () else bug!(msg)

    @Test
    def nested01(): Unit = region rc {
        let arr = Array#{Vector#{1, 2}, Vector#{3}} @ rc;
        let vec = Array.toVector(arr);
        let _ = assertTrue(Vector.length(vec) == 2, "nested01 failed: outer length.");
        let _ = assertTrue(Vector.length(Vector.get(0, vec)) == 2, "nested01 failed: first inner length.");
        assertTrue(Vector.length(Vector.get(1, vec)) == 1, "nested01 failed: second inner length.")
    }

    @Test
    def object01(): Unit = region rc {
        let arr = Array#{"a", "b"} @ rc;
        let vec = Array.toVector(arr);
        let _ = assertTrue(Vector.length(vec) == 2, "object01 failed: length.");
        assertTrue(Vector.get(1, vec) == "b", "object01 failed: element.")
    }

    pub def runAll(): Unit = {
        let _ = nested01();
        let _ = object01();
        ()
    }

}
