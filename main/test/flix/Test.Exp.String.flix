mod Test.Exp.String {

    use Assert.assertEq

    @Test
    def testString01(): Unit \ Assert =
        assertEq(expected = "", "")

    @Test
    def testString02(): Unit \ Assert =
        assertEq(expected = "a", "a")

    @Test
    def testString03(): Unit \ Assert =
        assertEq(expected = "z", "z")

    @Test
    def testString04(): Unit \ Assert =
        assertEq(expected = "A", "A")

    @Test
    def testString05(): Unit \ Assert =
        assertEq(expected = "Z", "Z")

    @Test
    def testString06(): Unit \ Assert =
        assertEq(expected = "\r", "\r")

    @Test
    def testString07(): Unit \ Assert =
        assertEq(expected = "\n", "\n")

    @Test
    def testString08(): Unit \ Assert =
        assertEq(expected = "\t", "\t")

    @Test
    def testString09(): Unit \ Assert =
        assertEq(expected = "\\", "\\")

    @Test
    def testString10(): Unit \ Assert =
        assertEq(expected = "\"", "\"")

    @Test
    def testString11(): Unit \ Assert =
        assertEq(expected = "\'", "\'")

    @Test
    def testString12(): Unit \ Assert =
        assertEq(expected = "\'\t\r\n\"", "\'\t\r\n\"")

    @Test
    def testString13(): Unit \ Assert =
        assertEq(expected = "\uffff", "\uffff")

    @Test
    def testString14(): Unit \ Assert =
        assertEq(expected = "十", "十")

    @Test
    def testString15(): Unit \ Assert =
        assertEq(expected = "\u200b", "\u200b")

    @Test
    def testString16(): Unit \ Assert =
        assertEq(expected = "A", "\u0041")

    @Test
    def testString17(): Unit \ Assert =
        assertEq(expected = "Z", "\u005A")

    @Test
    def testString18(): Unit \ Assert =
        assertEq(expected = "a", "\u0061")

    @Test
    def testString19(): Unit \ Assert =
        assertEq(expected = "z", "\u007a")

    @Test
    def testString20(): Unit \ Assert =
        assertEq(expected = "☃", "\u2603")

    @Test
    def testString21(): Unit \ Assert =
        assertEq(expected = "$" + "{", "\${")

    @Test
    def testString22(): Unit \ Assert =
        assertEq(expected = "%" + "{", "\%{")

}
