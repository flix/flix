mod Test.Exp.Regex {

    use Assert.assertTrue

    @CompileTest
    def testRegex01(): Regex = regex""

    @CompileTest
    def testRegex02(): Regex = regex".*"

    // Double-quote is not a special regex character so it only needs a single slash
    @CompileTest
    def testRegex03(): Regex = regex"\"{3}"

    @Test
    def testRegex04(): Unit \ Assert =
        assertTrue(Regex.isMatch(regex"\"{3}", "\"\"\""))

    @Test
    def testRegex05(): Unit \ Assert =
        assertTrue(Regex.isMatch(regex"\"a*b\"", "\"aaaab\""))

    // Open-square-bracket is a special regex character so it need double slash to escape
    @Test
    def testRegex06(): Unit \ Assert =
        assertTrue(Regex.isMatch(regex"\\[{3}", "[[["))

    @Test
    def testRegex07(): Unit \ Assert =
        assertTrue(Regex.isMatch(regex"\\[a*b\\]", "[aaaab]"))

    @Test
    def testRegex08(): Unit \ Assert =
        assertTrue(Regex.isMatch(regex"\\[+a*b\\]+", "[[[[aaaab]]"))

    @Test
    def testRegex09(): Unit \ Assert =
        assertTrue(Regex.isMatch(regex"\\[+\"a*b\"\\]+", "[[[[\"aaaab\"]]"))

    @Test
    def testRegex10(): Unit \ Assert =
        assertTrue(Regex.isMatch(regex"\\[+\\{a*b\\}\\]+", "[[[[{aaaab}]]"))

    @Test
    def testRegex11(): Unit \ Assert =
        assertTrue(Regex.isMatch(regex"\\[+\\{+a*b\\}+\\]+", "[[[[{{aaaab}}]]"))

    @Test
    def testRegex12(): Unit \ Assert =
        assertTrue(Regex.isMatch(regex"\\[+\\{+\"+a*b\"+\\}+\\]+", "[[[[{{\"\"\"aaaab\"\"\"}}]]"))

}
