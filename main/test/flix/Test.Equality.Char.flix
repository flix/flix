mod Test.Equality.Char {

    @test
    def testEqChar01(): Bool = 'a' == 'a'

    @test
    def testEqChar02(): Bool = 'z' == 'z'

    @test
    def testNeqChar01(): Bool = 'a' != 'b'

    @test
    def testNeqChar02(): Bool = 'z' != 'w'

}
