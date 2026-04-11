mod Test.Portable.String {

    def assertTrue(cond: Bool, msg: String): Unit =
        if (cond) () else bug!(msg)

    @Test
    def lower01(): Unit = {
        let s = String.toLowerCase("AbC");
        assertTrue(s == "abc", "lower01 failed.")
    }

    @Test
    def upper01(): Unit = {
        let s = String.toUpperCase("aBc");
        assertTrue(s == "ABC", "upper01 failed.")
    }

    @Test
    def mixed01(): Unit = {
        let s = String.toLowerCase("Hello, 42!");
        assertTrue(s == "hello, 42!", "mixed01 failed.")
    }

    @Test
    def lowerFinalSigma01(): Unit = {
        // Σ lowercases to ς in Final_Sigma context.
        let s = String.toLowerCase("\u0391\u03A3"); // "ΑΣ"
        assertTrue(s == "\u03B1\u03C2", "lowerFinalSigma01 failed.") // "ας"
    }

    @Test
    def lowerDottedI01(): Unit = {
        // Root-locale special-casing: İ -> i + combining dot above.
        let s = String.toLowerCase("\u0130"); // "İ"
        let _ = assertTrue(s == "i\u0307", "lowerDottedI01 failed.");
        assertTrue(String.length(s) == 2, "lowerDottedI01 length failed.")
    }

    @Test
    def upperSharpS01(): Unit = {
        // Root-locale special-casing: ß -> SS.
        let s = String.toUpperCase("\u00DF"); // "ß"
        let _ = assertTrue(s == "SS", "upperSharpS01 failed.");
        assertTrue(String.length(s) == 2, "upperSharpS01 length failed.")
    }

    pub def runAll(): Unit = {
        let _ = lower01();
        let _ = upper01();
        let _ = mixed01();
        let _ = lowerFinalSigma01();
        let _ = lowerDottedI01();
        let _ = upperSharpS01();
        ()
    }

}
