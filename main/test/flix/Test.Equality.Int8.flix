mod Test.Equality.Int8 {

    @Test
    def testEqInt8_01(): Bool = 0i8 == 0i8

    @Test
    def testEqInt8_02(): Bool = +1i8 == +1i8

    @Test
    def testEqInt8_03(): Bool = -1i8 == -1i8

    @Test
    def testNeqInt8_01(): Bool = -1i8 != +1i8

    @Test
    def testNeqInt8_02(): Bool = +1i8 != -1i8

}
