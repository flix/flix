mod Test.Equality.Int16 {

    @Test
    def testEqInt16_01(): Bool = 0i16 == 0i16

    @Test
    def testEqInt16_02(): Bool = +1i16 == +1i16

    @Test
    def testEqInt16_03(): Bool = -1i16 == -1i16

    @Test
    def testNeqInt16_01(): Bool = -1i16 != +1i16

    @Test
    def testNeqInt16_02(): Bool = +1i16 != -1i16

}
