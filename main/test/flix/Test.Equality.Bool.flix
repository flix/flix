mod Test.Equality.Bool {

    @Test
    def testEqBool01(): Bool = true == true

    @Test
    def testEqBool02(): Bool = false == false

    @Test
    def testNeqBool01(): Bool = true != false

    @Test
    def testNeqBool02(): Bool = false != true

}
