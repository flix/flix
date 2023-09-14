mod Test.Exp.String {

    @Test
    def testString01(): String = ""

    @Test
    def testString02(): String = "a"

    @Test
    def testString03(): String = "z"

    @Test
    def testString04(): String = "A"

    @Test
    def testString05(): String = "Z"

    @Test
    def testString06(): String = "\r"

    @Test
    def testString07(): String = "\n"

    @Test
    def testString08(): String = "\t"

    @Test
    def testString09(): String = "\\"

    @Test
    def testString10(): String = "\""

    @Test
    def testString11(): String = "\'"

    @Test
    def testString12(): String = "\'\t\r\n\""

    @Test
    def testString13(): String = "\uffff"

    @Test
    def testString14(): String = "十"

    @Test
    def testString15(): String = "\u200b"

    @Test
    def testString16(): Bool = "\u0041" == "A"

    @Test
    def testString17(): Bool = "\u005A" == "Z"

    @Test
    def testString18(): Bool = "\u0061" == "a"

    @Test
    def testString19(): Bool = "\u007a" == "z"

    @Test
    def testString20(): Bool = "\u2603" == "☃"

    @Test
    def testString21(): Bool = "\${" == "$" + "{"

    @Test
    def testString22(): Bool = "\%{" == "%" + "{"
}
