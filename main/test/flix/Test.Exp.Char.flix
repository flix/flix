mod Test.Exp.Char {

    use Assert.assertEq

    @CompileTest
    def charLit01(): Char = 'a'

    @CompileTest
    def charLit02(): Char = 'b'

    @CompileTest
    def charLit03(): Char = 'c'

    @CompileTest
    def charLit04(): Char = ' '

    @CompileTest
    def charLit05(): Char = '\r'

    @CompileTest
    def charLit06(): Char = '\n'

    @CompileTest
    def charLit07(): Char = '\t'

    @CompileTest
    def charLit08(): Char = '\\'

    @CompileTest
    def charLit09(): Char = '\''

    @CompileTest
    def charLit10(): Char = '0'

    @CompileTest
    def charLit11(): Char = '\u0000'

    @CompileTest
    def charLit12(): Char = '\u007f'

    @CompileTest
    def charLit13(): Char = '\uffff'

    @CompileTest
    def charLit14(): Char = '十'

    @CompileTest
    def charLit15(): Char = '\u200b'

    @Test
    def charEscape01(): Unit \ Assert =
        assertEq(expected = 'A', '\u0041')

    @Test
    def charEscape02(): Unit \ Assert =
        assertEq(expected = 'Z', '\u005A')

    @Test
    def charEscape03(): Unit \ Assert =
        assertEq(expected = 'a', '\u0061')

    @Test
    def charEscape04(): Unit \ Assert =
        assertEq(expected = 'z', '\u007a')

    @Test
    def charEscape05(): Unit \ Assert =
        assertEq(expected = '☃',  '\u2603')   // Snowman

    @Test
    def charEscape06(): Unit \ Assert =
        assertEq(expected = '\n', '\u000A')   // Line feed

    @Test
    def charEscape07(): Unit \ Assert =
        assertEq(expected = '\r', '\u000D')   // Carriage return

    @Test
    def charEscape08(): Unit \ Assert =
        assertEq(expected = '\t', '\u0009')   // Tab

    @Test
    def charEscape09(): Unit \ Assert =
        assertEq(expected = '"', '\u0022')   // Double quote

    @Test
    def charEscape10(): Unit \ Assert =
        assertEq(expected = '\'', '\u0027')  // Single quote

    @Test
    def charEscape11(): Unit \ Assert =
        assertEq(expected = '\\', '\u005C')  // Backslash

    @Test
    def charEscape12(): Unit \ Assert =
        assertEq(expected = ' ', '\u0020')  // Space

    @Test
    def charEscape13(): Unit \ Assert =
        assertEq(expected = '€', '\u20AC')  // Euro sign

    @Test
    def charEscape14(): Unit \ Assert =
        assertEq(expected = '→', '\u2192')  // Right arrow

}
