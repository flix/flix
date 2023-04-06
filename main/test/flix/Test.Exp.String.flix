mod Test.Exp.String {

    @test
    def testString01(): String = ""

    @test
    def testString02(): String = "a"

    @test
    def testString03(): String = "z"

    @test
    def testString04(): String = "A"

    @test
    def testString05(): String = "Z"

    @test
    def testString06(): String = "\r"

    @test
    def testString07(): String = "\n"

    @test
    def testString08(): String = "\t"

    @test
    def testString09(): String = "\\"

    @test
    def testString10(): String = "\""

    @test
    def testString11(): String = "\'"

    @test
    def testString12(): String = "\'\t\r\n\""

    @test
    def testString13(): String = "\uffff"

    @test
    def testString14(): String = "十"

    @test
    def testString15(): String = "\u200b"

    @test
    def testString16(): Bool = "\u0041" == "A"

    @test
    def testString17(): Bool = "\u005A" == "Z"

    @test
    def testString18(): Bool = "\u0061" == "a"

    @test
    def testString19(): Bool = "\u007a" == "z"

    @test
    def testString20(): Bool = "\u2603" == "☃"

    @test
    def testString21(): Bool = "\${" == "$" + "{"

    @test
    def testString22(): Bool = "\%{" == "%" + "{"
}
