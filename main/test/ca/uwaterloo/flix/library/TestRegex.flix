mod TestRegex {

    use Assert.{assertEq, assertNeq, assertTrue, assertFalse}

    /////////////////////////////////////////////////////////////////////////////
    // Regex.toString                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toString01(): Unit \ Assert =
        assertNeq(unexpected = "", ToString.toString(Regex.unmatchable()))

    /////////////////////////////////////////////////////////////////////////////
    // unmatchable                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unmatchable01(): Unit \ Assert =
        assertEq(expected = "^\\b$", Regex.unmatchable() |> Regex.pattern)

    /////////////////////////////////////////////////////////////////////////////
    // quote                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def quote01(): Unit \ Assert =
        assertEq(expected = "\\Qab\\E", Regex.quote("ab"))

    @Test
    def quote02(): Unit \ Assert =
        assertEq(expected = "\\Qa*b\\E", Regex.quote("a*b"))

    /////////////////////////////////////////////////////////////////////////////
    // pattern                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def pattern01(): Unit \ Assert =
        assertEq(expected = "a*b", Regex.pattern(regex"a*b"))

    @Test
    def pattern02(): Unit \ Assert =
        assertEq(expected = "\\p{Blank}+", Regex.pattern(regex"\\p{Blank}+"))

    /////////////////////////////////////////////////////////////////////////////
    // flags                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flags01(): Unit \ Assert =
        assertEq(expected = Ok(Set#{}), Result.map(Regex.flags, String.toRegex("\\p{Blank}+")))

    @Test
    def flags02(): Unit \ Assert =
        assertEq(expected = Ok(Set#{}), Result.map(Regex.flags, String.toRegexWithFlags(Set#{}, "\\p{Blank}+")))

    @Test
    def flags03(): Unit \ Assert =
        use Regex.Flag.{CaseInsenstive};
        assertEq(expected = Ok(Set#{CaseInsenstive}), Result.map(Regex.flags, String.toRegexWithFlags(Set#{CaseInsenstive}, "\\p{Blank}")))

    @Test
    def flags04(): Unit \ Assert =
        use Regex.Flag.{CaseInsenstive, Multiline};
        assertEq(expected = Ok(Set#{CaseInsenstive, Multiline}), Result.map(Regex.flags, String.toRegexWithFlags(Set#{CaseInsenstive, Multiline}, "\\p{Blank}")))

    /////////////////////////////////////////////////////////////////////////////
    // isMatch                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isMatch01(): Unit \ Assert =
        assertFalse(Regex.isMatch(regex"a+", ""))

    @Test
    def isMatch02(): Unit \ Assert =
        assertTrue(Regex.isMatch(regex"a*", ""))

    @Test
    def isMatch03(): Unit \ Assert =
        assertTrue(Regex.isMatch(regex"a*", "a"))

    @Test
    def isMatch04(): Unit \ Assert =
        assertTrue(Regex.isMatch(regex"a*", "aa"))

    @Test
    def isMatch05(): Unit \ Assert =
        assertFalse(Regex.isMatch(regex"a*", "aab"))

    @Test
    def isMatch06(): Unit \ Assert =
        assertTrue(Regex.isMatch(regex"a*b", "aab"))

    /////////////////////////////////////////////////////////////////////////////
    // isSubmatch                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isSubmatch01(): Unit \ Assert =
        assertFalse(Regex.isSubmatch(regex"a+", ""))

    @Test
    def isSubmatch02(): Unit \ Assert =
        assertTrue(Regex.isSubmatch(regex"a*", "a"))

    @Test
    def isSubmatch03(): Unit \ Assert =
        assertTrue(Regex.isSubmatch(regex"a*", "aa"))

    @Test
    def isSubmatch04(): Unit \ Assert =
        assertTrue(Regex.isSubmatch(regex"a*", "aab"))

    @Test
    def isSubmatch05(): Unit \ Assert =
        assertTrue(Regex.isSubmatch(regex"a*b", "aab"))

    @Test
    def isSubmatch06(): Unit \ Assert =
        assertTrue(Regex.isSubmatch(regex"b+", "aab"))

    @Test
    def isSubmatch07(): Unit \ Assert =
        assertTrue(Regex.isSubmatch(regex"b+", "bbbc"))

    @Test
    def isSubmatch08(): Unit \ Assert =
        assertTrue(Regex.isSubmatch(regex"b+", "aabbbc"))

    /////////////////////////////////////////////////////////////////////////////
    // indices                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indices01(): Unit \ Assert =
        assertEq(expected = List#{}, Regex.indices(Regex.unmatchable(), ""))

    @Test
    def indices02(): Unit \ Assert =
        assertEq(expected = List#{}, Regex.indices(Regex.unmatchable(), "a::b::c"))

    @Test
    def indices03(): Unit \ Assert =
        assertEq(expected = List#{}, Regex.indices(regex":{2}", ""))

    @Test
    def indices04(): Unit \ Assert =
        assertEq(expected = List#{}, Regex.indices(regex":{2}", "a"))

    @Test
    def indices05(): Unit \ Assert =
        assertEq(expected = List#{0}, Regex.indices(regex":{2}", "::"))

    @Test
    def indices06(): Unit \ Assert =
        assertEq(expected = List#{}, Regex.indices(regex":{2}", "ab"))

    @Test
    def indices07(): Unit \ Assert =
        assertEq(expected = List#{0}, Regex.indices(regex":{2}", "::a"))

    @Test
    def indices08(): Unit \ Assert =
        assertEq(expected = List#{1}, Regex.indices(regex":{2}", "a::"))

    @Test
    def indices09(): Unit \ Assert =
        assertEq(expected = List#{1, 4}, Regex.indices(regex":{2}", "a::b::"))

    @Test
    def indices10(): Unit \ Assert =
        assertEq(expected = List#{1, 4}, Regex.indices(regex":{2}", "a::b::c"))

    @Test
    def indices11(): Unit \ Assert =
        assertEq(expected = List#{1, 4}, Regex.indices(regex":{2}", "a::b::c:"))

    @Test
    def indices12(): Unit \ Assert =
        assertEq(expected = List#{2, 5}, Regex.indices(regex":{2}", ":a::b::c"))

    @Test
    def indices13(): Unit \ Assert =
        assertEq(expected = List#{0, 2}, Regex.indices(regex":{2}", "::::"))

    /////////////////////////////////////////////////////////////////////////////
    // submatches                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def submatches01(): Unit \ Assert =
        assertEq(expected = List#{}, Regex.submatches(Regex.unmatchable(), ""))

    @Test
    def submatches02(): Unit \ Assert =
        assertEq(expected = List#{}, Regex.submatches(Regex.unmatchable(), "a::b::c"))

    @Test
    def submatches03(): Unit \ Assert =
        assertEq(expected = List#{}, Regex.submatches(regex"\\p{Alpha}+", ""))

    @Test
    def submatches04(): Unit \ Assert =
        assertEq(expected = List#{"a"}, Regex.submatches(regex"\\p{Alpha}+", "a"))

    @Test
    def submatches05(): Unit \ Assert =
        assertEq(expected = List#{}, Regex.submatches(regex"\\p{Alpha}+", "::"))

    @Test
    def submatches06(): Unit \ Assert =
        assertEq(expected = List#{"ab"}, Regex.submatches(regex"\\p{Alpha}+", "ab"))

    @Test
    def submatches07(): Unit \ Assert =
        assertEq(expected = List#{"a"}, Regex.submatches(regex"\\p{Alpha}+", "::a"))

    @Test
    def submatches08(): Unit \ Assert =
        assertEq(expected = List#{"a"}, Regex.submatches(regex"\\p{Alpha}+", "a::"))

    @Test
    def submatches09(): Unit \ Assert =
        assertEq(expected = List#{"a", "b"}, Regex.submatches(regex"\\p{Alpha}+", "a::b::"))

    @Test
    def submatches10(): Unit \ Assert =
        assertEq(expected = List#{"a", "b", "c"}, Regex.submatches(regex"\\p{Alpha}+", "a::b::c"))

    @Test
    def submatches11(): Unit \ Assert =
        assertEq(expected = List#{"a", "b", "c"}, Regex.submatches(regex"\\p{Alpha}+", "a::b::c:"))

    @Test
    def submatches12(): Unit \ Assert =
        assertEq(expected = List#{"a", "b", "c"}, Regex.submatches(regex"\\p{Alpha}+", ":a::b::c"))

    @Test
    def submatches13(): Unit \ Assert =
        assertEq(expected = List#{}, Regex.submatches(regex"\\p{Alpha}+", "::::"))

    /////////////////////////////////////////////////////////////////////////////
    // countSubmatches                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def countSubmatches01(): Unit \ Assert =
        assertEq(expected = 0, Regex.countSubmatches(Regex.unmatchable(), ""))

    @Test
    def countSubmatches02(): Unit \ Assert =
        assertEq(expected = 0, Regex.countSubmatches(Regex.unmatchable(), "a::b::c"))

    @Test
    def countSubmatches03(): Unit \ Assert =
        assertEq(expected = 0, Regex.countSubmatches(regex"\\p{Alpha}+", ""))

    @Test
    def countSubmatches04(): Unit \ Assert =
        assertEq(expected = 1, Regex.countSubmatches(regex"\\p{Alpha}+", "a"))

    @Test
    def countSubmatches05(): Unit \ Assert =
        assertEq(expected = 0, Regex.countSubmatches(regex"\\p{Alpha}+", "::"))

    @Test
    def countSubmatches06(): Unit \ Assert =
        assertEq(expected = 1, Regex.countSubmatches(regex"\\p{Alpha}+", "ab"))

    @Test
    def countSubmatches07(): Unit \ Assert =
        assertEq(expected = 1, Regex.countSubmatches(regex"\\p{Alpha}+", "::a"))

    @Test
    def countSubmatches08(): Unit \ Assert =
        assertEq(expected = 1, Regex.countSubmatches(regex"\\p{Alpha}+", "a::"))

    @Test
    def countSubmatches09(): Unit \ Assert =
        assertEq(expected = 2, Regex.countSubmatches(regex"\\p{Alpha}+", "a::b::"))

    @Test
    def countSubmatches10(): Unit \ Assert =
        assertEq(expected = 3, Regex.countSubmatches(regex"\\p{Alpha}+", "a::b::c"))

    @Test
    def countSubmatches11(): Unit \ Assert =
        assertEq(expected = 3, Regex.countSubmatches(regex"\\p{Alpha}+", "a::b::c:"))

    @Test
    def countSubmatches12(): Unit \ Assert =
        assertEq(expected = 3, Regex.countSubmatches(regex"\\p{Alpha}+", ":a::b::c"))

    @Test
    def countSubmatches13(): Unit \ Assert =
        assertEq(expected = 0, Regex.countSubmatches(regex"\\p{Alpha}+", "::::"))

    /////////////////////////////////////////////////////////////////////////////
    // split                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def split01(): Unit \ Assert =
        assertEq(expected = List#{"A", "B", "C"}, Regex.split(regex"\\p{Blank}+", "A B C"))

    @Test
    def split02(): Unit \ Assert =
        assertEq(expected = List#{"A", "B", "C"}, Regex.split(regex"\\p{Blank}+", "A  B   C"))

    @Test
    def split03(): Unit \ Assert =
        assertEq(expected = List#{"ABC"}, Regex.split(regex"\\p{Blank}+", "ABC"))

    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def replace01(): Unit \ Assert =
        assertEq(expected = "", Regex.replace(src = regex"\\p{Blank}+", dst = "_", ""))

    @Test
    def replace02(): Unit \ Assert =
        assertEq(expected = "A_B_C", Regex.replace(src = regex"\\p{Blank}+", dst = "_", "A B C"))

    @Test
    def replace03(): Unit \ Assert =
        assertEq(expected = "A_B_C", Regex.replace(src = regex"\\p{Blank}+", dst = "_", "A  B   C"))

    @Test
    def replace04(): Unit \ Assert =
        assertEq(expected = "ABC", Regex.replace(src = regex"\\p{Blank}+", dst = "_", "ABC"))

    @Test
    def replace05(): Unit \ Assert =
        assertEq(expected = "A__B__C", Regex.replace(src = regex"\\p{Blank}+", dst = "__", "A B  C"))

    @Test
    def replace06(): Unit \ Assert =
        assertEq(expected = "__A__B__C__", Regex.replace(src = regex"\\p{Blank}+", dst = "__", " A B  C  "))

    /////////////////////////////////////////////////////////////////////////////
    // replaceFirst                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def replaceFirst01(): Unit \ Assert =
        assertEq(expected = "", Regex.replaceFirst(src = regex"\\p{Blank}+", dst = "_", ""))

    @Test
    def replaceFirst02(): Unit \ Assert =
        assertEq(expected = "A_B C", Regex.replaceFirst(src = regex"\\p{Blank}+", dst = "_", "A B C"))

    @Test
    def replaceFirst03(): Unit \ Assert =
        assertEq(expected = "A_B   C", Regex.replaceFirst(src = regex"\\p{Blank}+", dst = "_", "A  B   C"))

    @Test
    def replaceFirst04(): Unit \ Assert =
        assertEq(expected = "ABC", Regex.replaceFirst(src = regex"\\p{Blank}+", dst = "_", "ABC"))

    @Test
    def replaceFirst05(): Unit \ Assert =
        assertEq(expected = "A__B  C", Regex.replaceFirst(src = regex"\\p{Blank}+", dst = "__", "A B  C"))

    @Test
    def replaceFirst06(): Unit \ Assert =
        assertEq(expected = "__A B  C  ", Regex.replaceFirst(src = regex"\\p{Blank}+", dst = "__", " A B  C  "))

    /////////////////////////////////////////////////////////////////////////////
    // startsWith                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def startsWith01(): Unit \ Assert =
        assertFalse(Regex.startsWith(Regex.unmatchable(), ""))

    @Test
    def startsWith02(): Unit \ Assert =
        assertFalse(Regex.startsWith(Regex.unmatchable(), "A B C"))

    @Test
    def startsWith03(): Unit \ Assert =
        assertFalse(Regex.startsWith(regex"\\p{Alpha}+", ""))

    @Test
    def startsWith04(): Unit \ Assert =
        assertFalse(Regex.startsWith(regex"\\p{Alpha}+", "   A B C"))

    @Test
    def startsWith05(): Unit \ Assert =
        assertTrue(Regex.startsWith(regex"\\p{Alpha}+", "A B C"))

    @Test
    def startsWith06(): Unit \ Assert =
        assertTrue(Regex.startsWith(regex"\\p{Alpha}+", "AA BBB CCCC"))

    /////////////////////////////////////////////////////////////////////////////
    // endsWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def endsWith01(): Unit \ Assert =
        assertFalse(Regex.endsWith(Regex.unmatchable(), ""))

    @Test
    def endsWith02(): Unit \ Assert =
        assertFalse(Regex.endsWith(Regex.unmatchable(), "A B C"))

    @Test
    def endsWith03(): Unit \ Assert =
        assertFalse(Regex.endsWith(regex"\\p{Alpha}+", ""))

    @Test
    def endsWith04(): Unit \ Assert =
        assertFalse(Regex.endsWith(regex"\\p{Alpha}+", "A B C  "))

    @Test
    def endsWith05(): Unit \ Assert =
        assertTrue(Regex.endsWith(regex"\\p{Alpha}+", "A B C"))

    @Test
    def endsWith06(): Unit \ Assert =
        assertTrue(Regex.endsWith(regex"\\p{Alpha}+", "AA BBB CCCC"))

    /////////////////////////////////////////////////////////////////////////////
    // getPrefix                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def getPrefix01(): Unit \ Assert =
        assertEq(expected = None, Regex.getPrefix(Regex.unmatchable(), ""))

    @Test
    def getPrefix02(): Unit \ Assert =
        assertEq(expected = None, Regex.getPrefix(Regex.unmatchable(), "A B C"))

    @Test
    def getPrefix03(): Unit \ Assert =
        assertEq(expected = None, Regex.getPrefix(regex"\\p{Alpha}+", ""))

    @Test
    def getPrefix04(): Unit \ Assert =
        assertEq(expected = None, Regex.getPrefix(regex"\\p{Alpha}+", "   A B C"))

    @Test
    def getPrefix05(): Unit \ Assert =
        assertEq(expected = Some("A"), Regex.getPrefix(regex"\\p{Alpha}+", "A B C"))

    @Test
    def getPrefix06(): Unit \ Assert =
        assertEq(expected = Some("AA"), Regex.getPrefix(regex"\\p{Alpha}+", "AA BBB CCCC"))

    /////////////////////////////////////////////////////////////////////////////
    // getSuffix                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def getSuffix01(): Unit \ Assert =
        assertEq(expected = None, Regex.getSuffix(Regex.unmatchable(), ""))

    @Test
    def getSuffix02(): Unit \ Assert =
        assertEq(expected = None, Regex.getSuffix(Regex.unmatchable(), "A B C"))

    @Test
    def getSuffix03(): Unit \ Assert =
        assertEq(expected = None, Regex.getSuffix(regex"\\p{Alpha}+", ""))

    @Test
    def getSuffix04(): Unit \ Assert =
        assertEq(expected = None, Regex.getSuffix(regex"\\p{Alpha}+", "A B C  "))

    @Test
    def getSuffix05(): Unit \ Assert =
        assertEq(expected = Some("C"), Regex.getSuffix(regex"\\p{Alpha}+", "A B C"))

    @Test
    def getSuffix06(): Unit \ Assert =
        assertEq(expected = Some("CCCC"), Regex.getSuffix(regex"\\p{Alpha}+", "AA BBB CCCC"))

    /////////////////////////////////////////////////////////////////////////////
    // stripPrefix                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def stripPrefix01(): Unit \ Assert =
        assertEq(expected = None, Regex.stripPrefix(Regex.unmatchable(), ""))

    @Test
    def stripPrefix02(): Unit \ Assert =
        assertEq(expected = None, Regex.stripPrefix(Regex.unmatchable(), "A B C"))

    @Test
    def stripPrefix03(): Unit \ Assert =
        assertEq(expected = None, Regex.stripPrefix(regex"\\p{Alpha}+", ""))

    @Test
    def stripPrefix04(): Unit \ Assert =
        assertEq(expected = None, Regex.stripPrefix(regex"\\p{Alpha}+", "   A B C"))

    @Test
    def stripPrefix05(): Unit \ Assert =
        assertEq(expected = Some(" B C"), Regex.stripPrefix(regex"\\p{Alpha}+", "A B C"))

    @Test
    def stripPrefix06(): Unit \ Assert =
        assertEq(expected = Some(" BBB CCCC"), Regex.stripPrefix(regex"\\p{Alpha}+", "AA BBB CCCC"))

    /////////////////////////////////////////////////////////////////////////////
    // stripSuffix                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def stripSuffix01(): Unit \ Assert =
        assertEq(expected = None, Regex.stripSuffix(Regex.unmatchable(), ""))

    @Test
    def stripSuffix02(): Unit \ Assert =
        assertEq(expected = None, Regex.stripSuffix(Regex.unmatchable(), "A B C"))

    @Test
    def stripSuffix03(): Unit \ Assert =
        assertEq(expected = None, Regex.stripSuffix(regex"\\p{Alpha}+", ""))

    @Test
    def stripSuffix04(): Unit \ Assert =
        assertEq(expected = None, Regex.stripSuffix(regex"\\p{Alpha}+", "A B C  "))

    @Test
    def stripSuffix05(): Unit \ Assert =
        assertEq(expected = Some("A B "), Regex.stripSuffix(regex"\\p{Alpha}+", "A B C"))

    @Test
    def stripSuffix06(): Unit \ Assert =
        assertEq(expected = Some("AA BBB "), Regex.stripSuffix(regex"\\p{Alpha}+", "AA BBB CCCC"))

    /////////////////////////////////////////////////////////////////////////////
    // indexOfFirst                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOfFirst01(): Unit \ Assert =
        assertEq(expected = None, Regex.indexOfFirst(regex"\\p{Alpha}+", ""))

    @Test
    def indexOfFirst02(): Unit \ Assert =
        assertEq(expected = None, Regex.indexOfFirst(regex"\\p{Alpha}+", "_"))

    @Test
    def indexOfFirst03(): Unit \ Assert =
        assertEq(expected = Some(0), Regex.indexOfFirst(regex"\\p{Alpha}+", "a"))

    @Test
    def indexOfFirst04(): Unit \ Assert =
        assertEq(expected = None, Regex.indexOfFirst(regex"\\p{Alpha}+", "__"))

    @Test
    def indexOfFirst05(): Unit \ Assert =
        assertEq(expected = Some(0), Regex.indexOfFirst(regex"\\p{Alpha}+", "ab"))

    @Test
    def indexOfFirst06(): Unit \ Assert =
        assertEq(expected = Some(1), Regex.indexOfFirst(regex"\\p{Alpha}+", "_b"))

    @Test
    def indexOfFirst07(): Unit \ Assert =
        assertEq(expected = Some(0), Regex.indexOfFirst(regex"\\p{Alpha}+", "ab"))

    @Test
    def indexOfFirst08(): Unit \ Assert =
        assertEq(expected = Some(0), Regex.indexOfFirst(regex"\\p{Alpha}+", "ab_"))

    @Test
    def indexOfFirst09(): Unit \ Assert =
        assertEq(expected = Some(1), Regex.indexOfFirst(regex"\\p{Alpha}+", "_bc"))

    /////////////////////////////////////////////////////////////////////////////
    // indexOfLast                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOfLast01(): Unit \ Assert =
        assertEq(expected = None, Regex.indexOfLast(regex"\\p{Alpha}+", ""))

    @Test
    def indexOfLast02(): Unit \ Assert =
        assertEq(expected = None, Regex.indexOfLast(regex"\\p{Alpha}+", "_"))

    @Test
    def indexOfLast03(): Unit \ Assert =
        assertEq(expected = Some(0), Regex.indexOfLast(regex"\\p{Alpha}+", "a"))

    @Test
    def indexOfLast04(): Unit \ Assert =
        assertEq(expected = None, Regex.indexOfLast(regex"\\p{Alpha}+", "__"))

    @Test
    def indexOfLast05(): Unit \ Assert =
        assertEq(expected = Some(0), Regex.indexOfLast(regex"\\p{Alpha}+", "a_"))

    @Test
    def indexOfLast06(): Unit \ Assert =
        assertEq(expected = Some(1), Regex.indexOfLast(regex"\\p{Alpha}+", "_b"))

    @Test
    def indexOfLast07(): Unit \ Assert =
        assertEq(expected = Some(0), Regex.indexOfLast(regex"\\p{Alpha}+", "aa"))

    @Test
    def indexOfLast08(): Unit \ Assert =
        assertEq(expected = Some(0), Regex.indexOfLast(regex"\\p{Alpha}{2}", "ab"))

    @Test
    def indexOfLast09(): Unit \ Assert =
        assertEq(expected = Some(2), Regex.indexOfLast(regex"\\p{Alpha}{2}", "abcd"))

    /////////////////////////////////////////////////////////////////////////////
    // getFirst                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def getFirst01(): Unit \ Assert =
        assertEq(expected = None, Regex.getFirst(regex"\\p{Alpha}+", ""))

    @Test
    def getFirst02(): Unit \ Assert =
        assertEq(expected = None, Regex.getFirst(regex"\\p{Alpha}+", "_"))

    @Test
    def getFirst03(): Unit \ Assert =
        assertEq(expected = Some("a"), Regex.getFirst(regex"\\p{Alpha}+", "a"))

    @Test
    def getFirst04(): Unit \ Assert =
        assertEq(expected = None, Regex.getFirst(regex"\\p{Alpha}+", "__"))

    @Test
    def getFirst05(): Unit \ Assert =
        assertEq(expected = Some("ab"), Regex.getFirst(regex"\\p{Alpha}+", "ab"))

    @Test
    def getFirst06(): Unit \ Assert =
        assertEq(expected = Some("b"), Regex.getFirst(regex"\\p{Alpha}+", "_b"))

    @Test
    def getFirst07(): Unit \ Assert =
        assertEq(expected = Some("a"), Regex.getFirst(regex"\\p{Alpha}+", "a_"))

    @Test
    def getFirst08(): Unit \ Assert =
        assertEq(expected = Some("ab"), Regex.getFirst(regex"\\p{Alpha}+", "ab_"))

    @Test
    def getFirst09(): Unit \ Assert =
        assertEq(expected = Some("bc"), Regex.getFirst(regex"\\p{Alpha}+", "_bc"))

    /////////////////////////////////////////////////////////////////////////////
    // getLast                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def getLast01(): Unit \ Assert =
        assertEq(expected = None, Regex.getLast(regex"\\p{Alpha}+", ""))

    @Test
    def getLast02(): Unit \ Assert =
        assertEq(expected = None, Regex.getLast(regex"\\p{Alpha}+", "_"))

    @Test
    def getLast03(): Unit \ Assert =
        assertEq(expected = Some("a"), Regex.getLast(regex"\\p{Alpha}+", "a"))

    @Test
    def getLast04(): Unit \ Assert =
        assertEq(expected = None, Regex.getLast(regex"\\p{Alpha}+", "__"))

    @Test
    def getLast05(): Unit \ Assert =
        assertEq(expected = Some("a"), Regex.getLast(regex"\\p{Alpha}+", "a_"))

    @Test
    def getLast06(): Unit \ Assert =
        assertEq(expected = Some("b"), Regex.getLast(regex"\\p{Alpha}+", "_b"))

    @Test
    def getLast07(): Unit \ Assert =
        assertEq(expected = Some("aa"), Regex.getLast(regex"\\p{Alpha}+", "aa"))

    @Test
    def getLast08(): Unit \ Assert =
        assertEq(expected = Some("ab"), Regex.getLast(regex"\\p{Alpha}{2}", "ab"))

    @Test
    def getLast09(): Unit \ Assert =
        assertEq(expected = Some("cd"), Regex.getLast(regex"\\p{Alpha}{2}", "abcd"))

    /////////////////////////////////////////////////////////////////////////////
    // indexOfFirstWithOffset                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOfFirstWithOffset01(): Unit \ Assert =
        assertEq(expected = None, Regex.indexOfFirstWithOffset(regex"\\p{Alpha}+", 0, ""))

    @Test
    def indexOfFirstWithOffset02(): Unit \ Assert =
        assertEq(expected = None, Regex.indexOfFirstWithOffset(regex"\\p{Alpha}+", 0, "_"))

    @Test
    def indexOfFirstWithOffset03(): Unit \ Assert =
        assertEq(expected = Some(0), Regex.indexOfFirstWithOffset(regex"\\p{Alpha}+", 0, "a"))

    @Test
    def indexOfFirstWithOffset04(): Unit \ Assert =
        assertEq(expected = None, Regex.indexOfFirstWithOffset(regex"\\p{Alpha}+", 1, "a"))

    @Test
    def indexOfFirstWithOffset05(): Unit \ Assert =
        assertEq(expected = Some(0), Regex.indexOfFirstWithOffset(regex"\\p{Alpha}+", 0, "ab"))

    @Test
    def indexOfFirstWithOffset06(): Unit \ Assert =
        assertEq(expected = Some(1), Regex.indexOfFirstWithOffset(regex"\\p{Alpha}+", 1, "aa"))

    @Test
    def indexOfFirstWithOffset07(): Unit \ Assert =
        assertEq(expected = Some(1), Regex.indexOfFirstWithOffset(regex"\\p{Alpha}+", 0, "_b"))

    @Test
    def indexOfFirstWithOffset08(): Unit \ Assert =
        assertEq(expected = Some(0), Regex.indexOfFirstWithOffset(regex"\\p{Alpha}+", 0, "ab ab"))

    @Test
    def indexOfFirstWithOffset09(): Unit \ Assert =
        assertEq(expected = Some(1), Regex.indexOfFirstWithOffset(regex"\\p{Alpha}+", 1, "ab ab"))

    @Test
    def indexOfFirstWithOffset10(): Unit \ Assert =
        assertEq(expected = Some(3), Regex.indexOfFirstWithOffset(regex"\\p{Alpha}+", 2, "ab ab"))

    @Test
    def indexOfFirstWithOffset11(): Unit \ Assert =
        assertEq(expected = Some(3), Regex.indexOfFirstWithOffset(regex"\\p{Alpha}+", 3, "ab ab"))

    /////////////////////////////////////////////////////////////////////////////
    // indexOfLastWithOffset                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOfLastWithOffset01(): Unit \ Assert =
        assertEq(expected = None, Regex.indexOfLastWithOffset(regex"\\p{Alpha}+", 0, ""))

    @Test
    def indexOfLastWithOffset02(): Unit \ Assert =
        assertEq(expected = None, Regex.indexOfLastWithOffset(regex"\\p{Alpha}+", 0, "_"))

    @Test
    def indexOfLastWithOffset03(): Unit \ Assert =
        assertEq(expected = Some(0), Regex.indexOfLastWithOffset(regex"\\p{Alpha}+", 0, "a"))

    @Test
    def indexOfLastWithOffset04(): Unit \ Assert =
        assertEq(expected = None, Regex.indexOfLastWithOffset(regex"\\p{Alpha}+", 1, "a"))

    @Test
    def indexOfLastWithOffset05(): Unit \ Assert =
        assertEq(expected = Some(1), Regex.indexOfLastWithOffset(regex"\\p{Alpha}+", 1, "ab"))

    @Test
    def indexOfLastWithOffset06(): Unit \ Assert =
        assertEq(expected = None, Regex.indexOfLastWithOffset(regex"\\p{Alpha}+", 2, "aa"))

    @Test
    def indexOfLastWithOffset07(): Unit \ Assert =
        assertEq(expected = None, Regex.indexOfLastWithOffset(regex"\\p{Alpha}+", 3, "ab"))

    @Test
    def indexOfLastWithOffset08(): Unit \ Assert =
        assertEq(expected = Some(3), Regex.indexOfLastWithOffset(regex"\\p{Alpha}+", 3, "ab ab"))

    @Test
    def indexOfLastWithOffset09(): Unit \ Assert =
        assertEq(expected = Some(3), Regex.indexOfLastWithOffset(regex"\\p{Alpha}+", 2, "ab ab"))

    @Test
    def indexOfLastWithOffset10(): Unit \ Assert =
        assertEq(expected = Some(3), Regex.indexOfLastWithOffset(regex"\\p{Alpha}+", 1, "ab ab"))

    @Test
    def indexOfLastWithOffset11(): Unit \ Assert =
        assertEq(expected = Some(3), Regex.indexOfLastWithOffset(regex"\\p{Alpha}+", 0, "ab ab"))

    @Test
    def indexOfLastWithOffset12(): Unit \ Assert =
        assertEq(expected = None, Regex.indexOfLastWithOffset(regex"\\p{Alpha}+", -1, "ab ab"))

    @Test
    def indexOfLastWithOffset13(): Unit \ Assert =
        assertEq(expected = None, Regex.indexOfLastWithOffset(regex"\\p{Alpha}+", 2, "ab __"))

    /////////////////////////////////////////////////////////////////////////////
    // getFirstWithOffset                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def getFirstWithOffset01(): Unit \ Assert =
        assertEq(expected = None, Regex.getFirstWithOffset(regex"\\p{Alpha}+", 0, ""))

    @Test
    def getFirstWithOffset02(): Unit \ Assert =
        assertEq(expected = None, Regex.getFirstWithOffset(regex"\\p{Alpha}+", 0, "_"))

    @Test
    def getFirstWithOffset03(): Unit \ Assert =
        assertEq(expected = Some("a"), Regex.getFirstWithOffset(regex"\\p{Alpha}+", 0, "a"))

    @Test
    def getFirstWithOffset04(): Unit \ Assert =
        assertEq(expected = None, Regex.getFirstWithOffset(regex"\\p{Alpha}+", 1, "a"))

    @Test
    def getFirstWithOffset05(): Unit \ Assert =
        assertEq(expected = Some("ab"), Regex.getFirstWithOffset(regex"\\p{Alpha}+", 0, "ab"))

    @Test
    def getFirstWithOffset06(): Unit \ Assert =
        assertEq(expected = Some("a"), Regex.getFirstWithOffset(regex"\\p{Alpha}+", 1, "aa"))

    @Test
    def getFirstWithOffset07(): Unit \ Assert =
        assertEq(expected = Some("b"), Regex.getFirstWithOffset(regex"\\p{Alpha}+", 0, "_b"))

    @Test
    def getFirstWithOffset08(): Unit \ Assert =
        assertEq(expected = Some("ab"), Regex.getFirstWithOffset(regex"\\p{Alpha}+", 0, "ab cd"))

    @Test
    def getFirstWithOffset09(): Unit \ Assert =
        assertEq(expected = Some("b"), Regex.getFirstWithOffset(regex"\\p{Alpha}+", 1, "ab cd"))

    @Test
    def getFirstWithOffset10(): Unit \ Assert =
        assertEq(expected = Some("cd"), Regex.getFirstWithOffset(regex"\\p{Alpha}+", 2, "ab cd"))

    @Test
    def getFirstWithOffset11(): Unit \ Assert =
        assertEq(expected = Some("cd"), Regex.getFirstWithOffset(regex"\\p{Alpha}+", 3, "ab cd"))

    /////////////////////////////////////////////////////////////////////////////
    // getLastWithOffset                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def getLastWithOffset01(): Unit \ Assert =
        assertEq(expected = None, Regex.getLastWithOffset(regex"\\p{Alpha}+", 0, ""))

    @Test
    def getLastWithOffset02(): Unit \ Assert =
        assertEq(expected = None, Regex.getLastWithOffset(regex"\\p{Alpha}+", 0, "_"))

    @Test
    def getLastWithOffset03(): Unit \ Assert =
        assertEq(expected = Some("a"), Regex.getLastWithOffset(regex"\\p{Alpha}+", 0, "a"))

    @Test
    def getLastWithOffset04(): Unit \ Assert =
        assertEq(expected = None, Regex.getLastWithOffset(regex"\\p{Alpha}+", 1, "a"))

    @Test
    def getLastWithOffset05(): Unit \ Assert =
        assertEq(expected = Some("b"), Regex.getLastWithOffset(regex"\\p{Alpha}+", 1, "ab"))

    @Test
    def getLastWithOffset06(): Unit \ Assert =
        assertEq(expected = None, Regex.getLastWithOffset(regex"\\p{Alpha}+", 2, "aa"))

    @Test
    def getLastWithOffset07(): Unit \ Assert =
        assertEq(expected = None, Regex.getLastWithOffset(regex"\\p{Alpha}+", 3, "ab"))

    @Test
    def getLastWithOffset08(): Unit \ Assert =
        assertEq(expected = Some("cd"), Regex.getLastWithOffset(regex"\\p{Alpha}+", 3, "ab cd"))

    @Test
    def getLastWithOffset09(): Unit \ Assert =
        assertEq(expected = Some("cd"), Regex.getLastWithOffset(regex"\\p{Alpha}+", 2, "ab cd"))

    @Test
    def getLastWithOffset10(): Unit \ Assert =
        assertEq(expected = Some("cd"), Regex.getLastWithOffset(regex"\\p{Alpha}+", 1, "ab cd"))

    @Test
    def getLastWithOffset11(): Unit \ Assert =
        assertEq(expected = Some("cd"), Regex.getLastWithOffset(regex"\\p{Alpha}+", 0, "ab cd"))

    @Test
    def getLastWithOffset12(): Unit \ Assert =
        assertEq(expected = None, Regex.getLastWithOffset(regex"\\p{Alpha}+", -1, "ab cd"))

    @Test
    def getLastWithOffset13(): Unit \ Assert =
        assertEq(expected = None, Regex.getLastWithOffset(regex"\\p{Alpha}+", 2, "ab __"))

    /////////////////////////////////////////////////////////////////////////////
    // breakOnFirst                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def breakOnFirst01(): Unit \ Assert =
        assertEq(expected = ("", ""), Regex.breakOnFirst(regex":{2}", ""))

    @Test
    def breakOnFirst02(): Unit \ Assert =
        assertEq(expected = ("aaa", ""), Regex.breakOnFirst(regex":{2}", "aaa"))

    @Test
    def breakOnFirst03(): Unit \ Assert =
        assertEq(expected = ("", "::"), Regex.breakOnFirst(regex":{2}", "::"))

    @Test
    def breakOnFirst04(): Unit \ Assert =
        assertEq(expected = ("", "::aaa"), Regex.breakOnFirst(regex":{2}", "::aaa"))

    @Test
    def breakOnFirst05(): Unit \ Assert =
        assertEq(expected = ("aaa:", ""), Regex.breakOnFirst(regex":{2}", "aaa:"))

    @Test
    def breakOnFirst06(): Unit \ Assert =
        assertEq(expected = ("aaa", "::"), Regex.breakOnFirst(regex":{2}", "aaa::"))

    @Test
    def breakOnFirst07(): Unit \ Assert =
        assertEq(expected = ("aaa", "::bbb"), Regex.breakOnFirst(regex":{2}", "aaa::bbb"))

    @Test
    def breakOnFirst08(): Unit \ Assert =
        assertEq(expected = ("aaa", "::bbb::ccc"), Regex.breakOnFirst(regex":{2}", "aaa::bbb::ccc"))

    /////////////////////////////////////////////////////////////////////////////
    // breakAfterFirst                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def breakAfterFirst01(): Unit \ Assert =
        assertEq(expected = ("", ""), Regex.breakAfterFirst(regex":{2}", ""))

    @Test
    def breakAfterFirst02(): Unit \ Assert =
        assertEq(expected = ("aaa", ""), Regex.breakAfterFirst(regex":{2}", "aaa"))

    @Test
    def breakAfterFirst03(): Unit \ Assert =
        assertEq(expected = ("::", ""), Regex.breakAfterFirst(regex":{2}", "::"))

    @Test
    def breakAfterFirst04(): Unit \ Assert =
        assertEq(expected = ("::", "aaa"), Regex.breakAfterFirst(regex":{2}", "::aaa"))

    @Test
    def breakAfterFirst05(): Unit \ Assert =
        assertEq(expected = ("aaa:", ""), Regex.breakAfterFirst(regex":{2}", "aaa:"))

    @Test
    def breakAfterFirst06(): Unit \ Assert =
        assertEq(expected = ("aaa::", ""), Regex.breakAfterFirst(regex":{2}", "aaa::"))

    @Test
    def breakAfterFirst07(): Unit \ Assert =
        assertEq(expected = ("aaa::", "bbb"), Regex.breakAfterFirst(regex":{2}", "aaa::bbb"))

    @Test
    def breakAfterFirst08(): Unit \ Assert =
        assertEq(expected = ("aaa::", "bbb::ccc"), Regex.breakAfterFirst(regex":{2}", "aaa::bbb::ccc"))

    /////////////////////////////////////////////////////////////////////////////
    // breakOnLast                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def breakOnLast01(): Unit \ Assert =
        assertEq(expected = ("", ""), Regex.breakOnLast(regex":{2}", ""))

    @Test
    def breakOnLast02(): Unit \ Assert =
        assertEq(expected = ("aaa", ""), Regex.breakOnLast(regex":{2}", "aaa"))

    @Test
    def breakOnLast03(): Unit \ Assert =
        assertEq(expected = ("::", ""), Regex.breakOnLast(regex":{2}", "::"))

    @Test
    def breakOnLast04(): Unit \ Assert =
        assertEq(expected = ("aaa", ""), Regex.breakOnLast(regex":{2}", "aaa"))

    @Test
    def breakOnLast05(): Unit \ Assert =
        assertEq(expected = ("aaa:", ""), Regex.breakOnLast(regex":{2}", "aaa:"))

    @Test
    def breakOnLast06(): Unit \ Assert =
        assertEq(expected = ("aaa::", ""), Regex.breakOnLast(regex":{2}", "aaa::"))

    @Test
    def breakOnLast07(): Unit \ Assert =
        assertEq(expected = ("aaa::", "bbb"), Regex.breakOnLast(regex":{2}", "aaa::bbb"))

    @Test
    def breakOnLast08(): Unit \ Assert =
        assertEq(expected = ("aaa::bbb::", "ccc"), Regex.breakOnLast(regex":{2}", "aaa::bbb::ccc"))

    /////////////////////////////////////////////////////////////////////////////
    // breakBeforeLast                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def breakBeforeLast01(): Unit \ Assert =
        assertEq(expected = ("", ""), Regex.breakBeforeLast(regex":{2}", ""))

    @Test
    def breakBeforeLast02(): Unit \ Assert =
        assertEq(expected = ("aaa", ""), Regex.breakBeforeLast(regex":{2}", "aaa"))

    @Test
    def breakBeforeLast03(): Unit \ Assert =
        assertEq(expected = ("", "::"), Regex.breakBeforeLast(regex":{2}", "::"))

    @Test
    def breakBeforeLast04(): Unit \ Assert =
        assertEq(expected = ("aaa", ""), Regex.breakBeforeLast(regex":{2}", "aaa"))

    @Test
    def breakBeforeLast05(): Unit \ Assert =
        assertEq(expected = ("aaa:", ""), Regex.breakBeforeLast(regex":{2}", "aaa:"))

    @Test
    def breakBeforeLast06(): Unit \ Assert =
        assertEq(expected = ("aaa", "::"), Regex.breakBeforeLast(regex":{2}", "aaa::"))

    @Test
    def breakBeforeLast07(): Unit \ Assert =
        assertEq(expected = ("aaa", "::bbb"), Regex.breakBeforeLast(regex":{2}", "aaa::bbb"))

    @Test
    def breakBeforeLast08(): Unit \ Assert =
        assertEq(expected = ("aaa::bbb", "::ccc"), Regex.breakBeforeLast(regex":{2}", "aaa::bbb::ccc"))

    /////////////////////////////////////////////////////////////////////////////
    // isMatchWithBounds                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isMatchWithBounds01(): Unit \ Assert =
        assertFalse(Regex.isMatchWithBounds(regex"a+", start = 0, end = 1, ""))

    @Test
    def isMatchWithBounds02(): Unit \ Assert =
        assertTrue(Regex.isMatchWithBounds(regex"a+", start = 0, end = 1, "a"))

    @Test
    def isMatchWithBounds03(): Unit \ Assert =
        assertFalse(Regex.isMatchWithBounds(regex"a+", start = 0, end = 3, "a"))

    @Test
    def isMatchWithBounds04(): Unit \ Assert =
        assertTrue(Regex.isMatchWithBounds(regex"a+", start = 0, end = 1, "aa"))

    @Test
    def isMatchWithBounds05(): Unit \ Assert =
        assertFalse(Regex.isMatchWithBounds(regex"a+", start = 2, end = 3, "aab"))

    @Test
    def isMatchWithBounds06(): Unit \ Assert =
        assertTrue(Regex.isMatchWithBounds(regex"b+", start = 2, end = 3, "aab"))

    /////////////////////////////////////////////////////////////////////////////
    // isSubmatchWithBounds                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isSubmatchWithBounds01(): Unit \ Assert =
        assertFalse(Regex.isSubmatchWithBounds(regex"a+", start = 0, end = 1, ""))

    @Test
    def isSubmatchWithBounds02(): Unit \ Assert =
        assertTrue(Regex.isSubmatchWithBounds(regex"a+", start = 0, end = 1, "a"))

    @Test
    def isSubmatchWithBounds03(): Unit \ Assert =
        assertTrue(Regex.isSubmatchWithBounds(regex"a+", start = 1, end = 2, "aa"))

    @Test
    def isSubmatchWithBounds04(): Unit \ Assert =
        assertFalse(Regex.isSubmatchWithBounds(regex"a+", start = -1, end = 1, "aab"))

    @Test
    def isSubmatchWithBounds05(): Unit \ Assert =
        assertTrue(Regex.isSubmatchWithBounds(regex"a+", start = 0, end = 2, "aab"))

    @Test
    def isSubmatchWithBounds06(): Unit \ Assert =
        assertTrue(Regex.isSubmatchWithBounds(regex"b+", start = 2, end = 3, "aab"))

    @Test
    def isSubmatchWithBounds07(): Unit \ Assert =
        assertTrue(Regex.isSubmatchWithBounds(regex"b+", start = 1, end = 2, "bbbc"))

    @Test
    def isSubmatchWithBounds08(): Unit \ Assert =
        assertTrue(Regex.isSubmatchWithBounds(regex"b+", start = 0, end = 5, "aabbbc"))

    /////////////////////////////////////////////////////////////////////////////
    // indicesWithBounds                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indicesWithBounds01(): Unit \ Assert =
        assertEq(expected = List#{}, Regex.indicesWithBounds(regex":{2}", start = 0, end = 1, ""))

    @Test
    def indicesWithBounds02(): Unit \ Assert =
        assertEq(expected = List#{}, Regex.indicesWithBounds(regex":{2}", start = -1, end = 10, "a::b::c"))

    @Test
    def indicesWithBounds03(): Unit \ Assert =
        assertEq(expected = List#{1, 4}, Regex.indicesWithBounds(regex":{2}", start = 0, end = 7, "a::b::c"))

    @Test
    def indicesWithBounds04(): Unit \ Assert =
        assertEq(expected = List#{1}, Regex.indicesWithBounds(regex":{2}", start = 0, end = 3, "a::b::c"))

    @Test
    def indicesWithBounds05(): Unit \ Assert =
        assertEq(expected = List#{4}, Regex.indicesWithBounds(regex":{2}", start = 3, end = 7, "a::b::c"))

    @Test
    def indicesWithBounds06(): Unit \ Assert =
        assertEq(expected = List#{}, Regex.indicesWithBounds(regex":{2}", start = 0, end = 3, "a:b"))

    /////////////////////////////////////////////////////////////////////////////
    // submatchesWithBounds                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def submatchesWithBounds01(): Unit \ Assert =
        assertEq(expected = List#{}, Regex.submatchesWithBounds(regex"\\p{Alpha}+", start = 0, end = 1, ""))

    @Test
    def submatchesWithBounds02(): Unit \ Assert =
        assertEq(expected = List#{}, Regex.submatchesWithBounds(regex"\\p{Alpha}+", start = -1, end = 10, "a::b::c"))

    @Test
    def submatchesWithBounds03(): Unit \ Assert =
        assertEq(expected = List#{"aaa"}, Regex.submatchesWithBounds(regex"\\p{Alpha}+", start = 0, end = 3, "aaa"))

    @Test
    def submatchesWithBounds04(): Unit \ Assert =
        assertEq(expected = List#{}, Regex.submatchesWithBounds(regex"\\p{Alpha}+", start = 0, end = 10, "aaa"))

    @Test
    def submatchesWithBounds05(): Unit \ Assert =
        assertEq(expected = List#{}, Regex.submatchesWithBounds(regex"\\p{Alpha}+", start = 0, end = 2, "::"))

    @Test
    def submatchesWithBounds06(): Unit \ Assert =
        assertEq(expected = List#{"ab"}, Regex.submatchesWithBounds(regex"\\p{Alpha}+", start = 0, end = 2, "ab"))

    @Test
    def submatchesWithBounds07(): Unit \ Assert =
        assertEq(expected = List#{"a"}, Regex.submatchesWithBounds(regex"\\p{Alpha}+", start = 0, end = 3, "::a"))

    @Test
    def submatchesWithBounds08(): Unit \ Assert =
        assertEq(expected = List#{"a"}, Regex.submatchesWithBounds(regex"\\p{Alpha}+", start = 0, end = 3, "a::"))

    @Test
    def submatchesWithBounds09(): Unit \ Assert =
        assertEq(expected = List#{"a"}, Regex.submatchesWithBounds(regex"\\p{Alpha}+", start = 0, end = 3, "a::b::"))

    @Test
    def submatchesWithBounds10(): Unit \ Assert =
        assertEq(expected = List#{"b", "c"}, Regex.submatchesWithBounds(regex"\\p{Alpha}+", start = 2, end = 7, "a::b::c"))

    @Test
    def submatchesWithBounds11(): Unit \ Assert =
        assertEq(expected = List#{"a", "b", "c"}, Regex.submatchesWithBounds(regex"\\p{Alpha}+", start = 0, end = 7, "a::b::c:"))

    @Test
    def submatchesWithBounds12(): Unit \ Assert =
        assertEq(expected = List#{"a", "b"}, Regex.submatchesWithBounds(regex"\\p{Alpha}+", start = 0, end = 6, ":a::b::c"))

    @Test
    def submatchesWithBounds13(): Unit \ Assert =
        assertEq(expected = List#{}, Regex.submatchesWithBounds(regex"\\p{Alpha}+", start = 0, end = 4, "::::"))

    /////////////////////////////////////////////////////////////////////////////
    // countSubmatchesWithBounds                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def countSubmatchesWithBounds01(): Unit \ Assert =
        assertEq(expected = 0, Regex.countSubmatchesWithBounds(regex"\\p{Alpha}+", start = 0, end = 1, ""))

    @Test
    def countSubmatchesWithBounds02(): Unit \ Assert =
        assertEq(expected = 0, Regex.countSubmatchesWithBounds(regex"\\p{Alpha}+", start = -1, end = 10, "a::b::c"))

    @Test
    def countSubmatchesWithBounds03(): Unit \ Assert =
        assertEq(expected = 3, Regex.countSubmatchesWithBounds(regex"\\p{Alpha}+", start = 0, end = 7, "a::b::c"))

    @Test
    def countSubmatchesWithBounds04(): Unit \ Assert =
        assertEq(expected = 2, Regex.countSubmatchesWithBounds(regex"\\p{Alpha}+", start = 0, end = 4, "a::b::c"))

    @Test
    def countSubmatchesWithBounds05(): Unit \ Assert =
        assertEq(expected = 2, Regex.countSubmatchesWithBounds(regex"\\p{Alpha}+", start = 1, end = 7, "a::b::c"))

    @Test
    def countSubmatchesWithBounds06(): Unit \ Assert =
        assertEq(expected = 1, Regex.countSubmatchesWithBounds(regex"\\p{Alpha}+", start = 0, end = 2, "ab"))

    @Test
    def countSubmatchesWithBounds07(): Unit \ Assert =
        assertEq(expected = 1, Regex.countSubmatchesWithBounds(regex"\\p{Alpha}+", start = 1, end = 3, "::a"))

    @Test
    def countSubmatchesWithBounds08(): Unit \ Assert =
        assertEq(expected = 1, Regex.countSubmatchesWithBounds(regex"\\p{Alpha}+", start = 0, end = 2, "a::"))

    @Test
    def countSubmatchesWithBounds09(): Unit \ Assert =
        assertEq(expected = 2, Regex.countSubmatchesWithBounds(regex"\\p{Alpha}+", start = 0, end = 4, "a::b::"))

    @Test
    def countSubmatchesWithBounds10(): Unit \ Assert =
        assertEq(expected = 2, Regex.countSubmatchesWithBounds(regex"\\p{Alpha}+", start = 0, end = 4, "a::b::c"))

    @Test
    def countSubmatchesWithBounds11(): Unit \ Assert =
        assertEq(expected = 1, Regex.countSubmatchesWithBounds(regex"\\p{Alpha}+", start = 4, end = 8, "a::b::c:"))

    @Test
    def countSubmatchesWithBounds12(): Unit \ Assert =
        assertEq(expected = 1, Regex.countSubmatchesWithBounds(regex"\\p{Alpha}+", start = 0, end = 4, ":a::b::c"))

    @Test
    def countSubmatchesWithBounds13(): Unit \ Assert =
        assertEq(expected = 0, Regex.countSubmatchesWithBounds(regex"\\p{Alpha}+", start = 0, end = 4, "::::"))

    /////////////////////////////////////////////////////////////////////////////
    // indexOfFirstWithBounds                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOfFirstWithBounds01(): Unit \ Assert =
        assertEq(expected = None, Regex.indexOfFirstWithBounds(regex"\\p{Alpha}+", start = 0, end = 1, ""))

    @Test
    def indexOfFirstWithBounds02(): Unit \ Assert =
        assertEq(expected = None, Regex.indexOfFirstWithBounds(regex"\\p{Alpha}+", start = -1, end = 5, "a::b"))

    @Test
    def indexOfFirstWithBounds03(): Unit \ Assert =
        assertEq(expected = Some(0), Regex.indexOfFirstWithBounds(regex"\\p{Alpha}+", start = 0, end = 1, "a"))

    @Test
    def indexOfFirstWithBounds04(): Unit \ Assert =
        assertEq(expected = Some(0), Regex.indexOfFirstWithBounds(regex"\\p{Alpha}+", start = 0, end = 7, "a::b::c"))

    @Test
    def indexOfFirstWithBounds05(): Unit \ Assert =
        assertEq(expected = Some(0), Regex.indexOfFirstWithBounds(regex"\\p{Alpha}+", start = 0, end = 2, "a::b::c"))

    @Test
    def indexOfFirstWithBounds06(): Unit \ Assert =
        assertEq(expected = Some(3), Regex.indexOfFirstWithBounds(regex"\\p{Alpha}+", start = 3, end = 7, "a::b::c"))

    @Test
    def indexOfFirstWithBounds07(): Unit \ Assert =
        assertEq(expected = Some(6), Regex.indexOfFirstWithBounds(regex"\\p{Alpha}+", start = 6, end = 7, "a::b::c"))

    @Test
    def indexOfFirstWithBounds08(): Unit \ Assert =
        assertEq(expected = Some(3), Regex.indexOfFirstWithBounds(regex"\\p{Alpha}+", start = 1, end = 6, "a::b::c"))

    @Test
    def indexOfFirstWithBounds09(): Unit \ Assert =
        assertEq(expected = None, Regex.indexOfFirstWithBounds(regex"\\p{Alpha}+", start = 7, end = 9, "a::b::c::"))

    /////////////////////////////////////////////////////////////////////////////
    // indexOfLastWithBounds                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOfLastWithBounds01(): Unit \ Assert =
        assertEq(expected = None, Regex.indexOfLastWithBounds(regex"\\p{Alpha}+", start = 0, end = 1, ""))

    @Test
    def indexOfLastWithBounds02(): Unit \ Assert =
        assertEq(expected = None, Regex.indexOfLastWithBounds(regex"\\p{Alpha}+", start = -1, end = 5, "a::b"))

    @Test
    def indexOfLastWithBounds03(): Unit \ Assert =
        assertEq(expected = Some(0), Regex.indexOfLastWithBounds(regex"\\p{Alpha}+", start = 0, end = 1, "a"))

    @Test
    def indexOfLastWithBounds04(): Unit \ Assert =
        assertEq(expected = Some(6), Regex.indexOfLastWithBounds(regex"\\p{Alpha}+", start = 0, end = 7, "a::b::c"))

    @Test
    def indexOfLastWithBounds05(): Unit \ Assert =
        assertEq(expected = Some(6), Regex.indexOfLastWithBounds(regex"\\p{Alpha}+", start = 5, end = 7, "a::b::c"))

    @Test
    def indexOfLastWithBounds06(): Unit \ Assert =
        assertEq(expected = Some(3), Regex.indexOfLastWithBounds(regex"\\p{Alpha}+", start = 0, end = 6, "a::b::c"))

    @Test
    def indexOfLastWithBounds07(): Unit \ Assert =
        assertEq(expected = Some(0), Regex.indexOfLastWithBounds(regex"\\p{Alpha}+", start = 0, end = 3, "a::b::c"))

    @Test
    def indexOfLastWithBounds08(): Unit \ Assert =
        assertEq(expected = Some(3), Regex.indexOfLastWithBounds(regex"\\p{Alpha}+", start = 1, end = 6, "a::b::c"))

    @Test
    def indexOfLastWithBounds09(): Unit \ Assert =
        assertEq(expected = None, Regex.indexOfLastWithBounds(regex"\\p{Alpha}+", start = 0, end = 2, "::a::b::c"))

    /////////////////////////////////////////////////////////////////////////////
    // getFirstWithBounds                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def getFirstWithBounds01(): Unit \ Assert =
        assertEq(expected = None, Regex.getFirstWithBounds(regex"\\p{Alpha}+", start = 0, end = 1, ""))

    @Test
    def getFirstWithBounds02(): Unit \ Assert =
        assertEq(expected = None, Regex.getFirstWithBounds(regex"\\p{Alpha}+", start = -1, end = 5, "a::b"))

    @Test
    def getFirstWithBounds03(): Unit \ Assert =
        assertEq(expected = Some("a"), Regex.getFirstWithBounds(regex"\\p{Alpha}+", start = 0, end = 1, "a"))

    @Test
    def getFirstWithBounds04(): Unit \ Assert =
        assertEq(expected = Some("a"), Regex.getFirstWithBounds(regex"\\p{Alpha}+", start = 0, end = 7, "a::b::c"))

    @Test
    def getFirstWithBounds05(): Unit \ Assert =
        assertEq(expected = Some("a"), Regex.getFirstWithBounds(regex"\\p{Alpha}+", start = 0, end = 2, "a::b::c"))

    @Test
    def getFirstWithBounds06(): Unit \ Assert =
        assertEq(expected = Some("b"), Regex.getFirstWithBounds(regex"\\p{Alpha}+", start = 3, end = 7, "a::b::c"))

    @Test
    def getFirstWithBounds07(): Unit \ Assert =
        assertEq(expected = Some("c"), Regex.getFirstWithBounds(regex"\\p{Alpha}+", start = 6, end = 7, "a::b::c"))

    @Test
    def getFirstWithBounds08(): Unit \ Assert =
        assertEq(expected = Some("b"), Regex.getFirstWithBounds(regex"\\p{Alpha}+", start = 1, end = 6, "a::b::c"))

    @Test
    def getFirstWithBounds09(): Unit \ Assert =
        assertEq(expected = None, Regex.getFirstWithBounds(regex"\\p{Alpha}+", start = 7, end = 9, "a::b::c::"))

    /////////////////////////////////////////////////////////////////////////////
    // getLastWithBounds                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def getLastWithBounds01(): Unit \ Assert =
        assertEq(expected = None, Regex.getLastWithBounds(regex"\\p{Alpha}+", start = 0, end = 1, ""))

    @Test
    def getLastWithBounds02(): Unit \ Assert =
        assertEq(expected = None, Regex.getLastWithBounds(regex"\\p{Alpha}+", start = -1, end = 5, "a::b"))

    @Test
    def getLastWithBounds03(): Unit \ Assert =
        assertEq(expected = Some("a"), Regex.getLastWithBounds(regex"\\p{Alpha}+", start = 0, end = 1, "a"))

    @Test
    def getLastWithBounds04(): Unit \ Assert =
        assertEq(expected = Some("c"), Regex.getLastWithBounds(regex"\\p{Alpha}+", start = 0, end = 7, "a::b::c"))

    @Test
    def getLastWithBounds05(): Unit \ Assert =
        assertEq(expected = Some("c"), Regex.getLastWithBounds(regex"\\p{Alpha}+", start = 5, end = 7, "a::b::c"))

    @Test
    def getLastWithBounds06(): Unit \ Assert =
        assertEq(expected = Some("b"), Regex.getLastWithBounds(regex"\\p{Alpha}+", start = 0, end = 6, "a::b::c"))

    @Test
    def getLastWithBounds07(): Unit \ Assert =
        assertEq(expected = Some("a"), Regex.getLastWithBounds(regex"\\p{Alpha}+", start = 0, end = 3, "a::b::c"))

    @Test
    def getLastWithBounds08(): Unit \ Assert =
        assertEq(expected = Some("b"), Regex.getLastWithBounds(regex"\\p{Alpha}+", start = 1, end = 6, "a::b::c"))

    @Test
    def getLastWithBounds09(): Unit \ Assert =
        assertEq(expected = None, Regex.getLastWithBounds(regex"\\p{Alpha}+", start = 0, end = 2, "::a::b::c"))

}
