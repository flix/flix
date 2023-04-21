mod Test.Exp.Char {

    @test
    def testChar01(): Char = 'a'

    @test
    def testChar02(): Char = 'b'

    @test
    def testChar03(): Char = 'c'

    @test
    def testChar04(): Char = ' '

    @test
    def testChar05(): Char = '\r'

    @test
    def testChar06(): Char = '\n'

    @test
    def testChar07(): Char = '\t'

    @test
    def testChar08(): Char = '\\'

    @test
    def testChar09(): Char = '\''

    @test
    def testChar10(): Char = '0'

    @test
    def testChar11(): Char = '\u0000'

    @test
    def testChar12(): Char = '\u007f'

    @test
    def testChar13(): Char = '\uffff'

    @test
    def testChar14(): Char = '十'

    @test
    def testChar15(): Char = '\u200b'

    @test
    def testChar16(): Bool = '\u0041' == 'A'

    @test
    def testChar17(): Bool = '\u005A' == 'Z'

    @test
    def testChar18(): Bool = '\u0061' == 'a'

    @test
    def testChar19(): Bool = '\u007a' == 'z'

    @test
    def testChar20(): Bool = '\u2603' == '☃'

}
