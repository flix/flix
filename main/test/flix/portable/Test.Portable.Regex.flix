mod Test.Portable.Regex {

    def assertTrue(cond: Bool, msg: String): Unit =
        if (cond) () else bug!(msg)

    @Test
    def isMatch01(): Unit =
        let rgx = Regex.compile("a+");
        let _ = assertTrue(Regex.isMatch(rgx, "aaaa"), "isMatch01 failed: expected isMatch.");
        assertTrue(not Regex.isMatch(rgx, "aaab"), "isMatch01 failed: expected not isMatch.")

    @Test
    def isSubmatch01(): Unit =
        let rgx = Regex.compile("b+");
        let _ = assertTrue(Regex.isSubmatch(rgx, "aaabbbccc"), "isSubmatch01 failed: expected isSubmatch.");
        assertTrue(not Regex.isSubmatch(rgx, "aaaccc"), "isSubmatch01 failed: expected not isSubmatch.")

    @Test
    def replace01(): Unit =
        let src = Regex.compile("a+");
        assertTrue(Regex.replace(src = src, dst = "b", "aaax") == "bx", "replace01 failed.")

    @Test
    def split01(): Unit =
        let rgx = Regex.compile(",");
        assertTrue(Regex.split(rgx, "a,b,c") == "a" :: "b" :: "c" :: Nil, "split01 failed.")

    pub def runAll(): Unit = {
        let _ = isMatch01();
        let _ = isSubmatch01();
        let _ = replace01();
        let _ = split01();
        ()
    }

}
