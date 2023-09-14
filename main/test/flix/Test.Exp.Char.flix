mod Test.Exp.Char {

    @Test
    def testChar01(): Char = 'a'

    @Test
    def testChar02(): Char = 'b'

    @Test
    def testChar03(): Char = 'c'

    @Test
    def testChar04(): Char = ' '

    @Test
    def testChar05(): Char = '\r'

    @Test
    def testChar06(): Char = '\n'

    @Test
    def testChar07(): Char = '\t'

    @Test
    def testChar08(): Char = '\\'

    @Test
    def testChar09(): Char = '\''

    @Test
    def testChar10(): Char = '0'

    @Test
    def testChar11(): Char = '\u0000'

    @Test
    def testChar12(): Char = '\u007f'

    @Test
    def testChar13(): Char = '\uffff'

    @Test
    def testChar14(): Char = '十'

    @Test
    def testChar15(): Char = '\u200b'

    @Test
    def testChar16(): Bool = '\u0041' == 'A'

    @Test
    def testChar17(): Bool = '\u005A' == 'Z'

    @Test
    def testChar18(): Bool = '\u0061' == 'a'

    @Test
    def testChar19(): Bool = '\u007a' == 'z'

    @Test
    def testChar20(): Bool = '\u2603' == '☃'

}
