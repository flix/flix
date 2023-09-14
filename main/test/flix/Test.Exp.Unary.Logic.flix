mod Test.Exp.Unary.Logic {

    @Test
    def testLogicalNot01(): Bool = false == not true

    @Test
    def testLogicalNot02(): Bool = true == not false

    @Test
    def testLogicalNot03(): Bool = false == not true

    @Test
    def testLogicalNot04(): Bool = true == not false

}
