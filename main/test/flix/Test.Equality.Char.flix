mod Test.Equality.Char {

    @Test
    def testEqChar01(): Bool = 'a' == 'a'

    @Test
    def testEqChar02(): Bool = 'z' == 'z'

    @Test
    def testNeqChar01(): Bool = 'a' != 'b'

    @Test
    def testNeqChar02(): Bool = 'z' != 'w'

}
