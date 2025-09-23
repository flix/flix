mod Test.Exp.Regex {

    @test
    def testRegex01(): Regex = regex""

    @test
    def testRegex02(): Regex = regex".*"

    // Double-quote is not a special regex character so it only needs a single slash
    @test
    def testRegex03(): Regex = regex"\"{3}"

    @test
    def testRegex04(): Bool =
        Regex.isMatch(regex"\"{3}", "\"\"\"")

    @test
    def testRegex05(): Bool =
        Regex.isMatch(regex"\"a*b\"", "\"aaaab\"")

    // Open-square-bracket is a special regex character so it need double slash to escape
    @test
    def testRegex06(): Bool =
        Regex.isMatch(regex"\\[{3}", "[[[")

    @test
    def testRegex07(): Bool =
        Regex.isMatch(regex"\\[a*b\\]", "[aaaab]")

    @test
    def testRegex08(): Bool =
        Regex.isMatch(regex"\\[+a*b\\]+", "[[[[aaaab]]")

    @test
    def testRegex09(): Bool =
        Regex.isMatch(regex"\\[+\"a*b\"\\]+", "[[[[\"aaaab\"]]")

    @test
    def testRegex10(): Bool =
        Regex.isMatch(regex"\\[+\\{a*b\\}\\]+", "[[[[{aaaab}]]")

    @test
    def testRegex11(): Bool =
        Regex.isMatch(regex"\\[+\\{+a*b\\}+\\]+", "[[[[{{aaaab}}]]")

    @test
    def testRegex12(): Bool =
        Regex.isMatch(regex"\\[+\\{+\"+a*b\"+\\}+\\]+", "[[[[{{\"\"\"aaaab\"\"\"}}]]")
}
