mod Test.Exp.Chained.ApplyRecordSelect {

    use Assert.assertEq;

    @Test
    def testApplyRecordSelect01(): Unit \ Assert = {
        let f = () -> { x = 1 };
        assertEq(expected = 1, f()#x)
    }

    @Test
    def testApplyRecordSelect02(): Unit \ Assert = {
        let r = { f = () -> 42 };
        assertEq(expected = 42, r#f())
    }

    @Test
    def testApplyRecordSelect03(): Unit \ Assert = {
        let f = () -> { g = () -> { y = 12 } };
        assertEq(expected = 12, f()#g()#y)
    }

    @Test
    def testApplyRecordSelect4(): Unit \ Assert = {
        let f = () -> { g = () -> { h = () -> 12 } };
        assertEq(expected = 12, f()#g()#h())
    }

    @Test
    def testApplyRecordSelect5(): Unit \ Assert = {
        let f = () -> { g = () -> { h = () -> { x = 13 } } };
        assertEq(expected = 13, f()#g()#h()#x)
    }

    @Test
    def testApplyRecordSelect6(): Unit \ Assert = {
        let f = y -> { g = () -> { h = () -> { x = y + 13 } } };
        assertEq(expected = 15, f(2)#g()#h()#x)
    }

}
