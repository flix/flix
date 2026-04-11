mod Test.Portable.StringBuilder {

    def assertTrue(cond: Bool, msg: String): Unit =
        if (cond) () else bug!(msg)

    @Test
    def basic01(): Unit = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendString("Hello, ", sb);
        StringBuilder.append(42, sb);
        assertTrue(StringBuilder.toString(sb) == "Hello, 42", "basic01 failed.")
    }

    @Test
    def codePoint01(): Unit = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendCodePoint(65, sb); // 'A'
        assertTrue(StringBuilder.toString(sb) == "A", "codePoint01 failed.")
    }

    @Test
    def lengthAndSetLength01(): Unit = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendString("abcd", sb);
        let len1 = StringBuilder.length(sb);
        StringBuilder.setLength(2, sb);
        let _ = assertTrue(len1 == 4, "lengthAndSetLength01 failed: expected len1 == 4.");
        assertTrue(StringBuilder.toString(sb) == "ab", "lengthAndSetLength01 failed: expected \"ab\".")
    }

    pub def runAll(): Unit = {
        let _ = basic01();
        let _ = codePoint01();
        let _ = lengthAndSetLength01();
        ()
    }

}
