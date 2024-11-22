mod Test.Exp.Chained.ApplyRecordSelect {

    @Test
    def testApplyRecordSelect01(): Bool = {
        let f = () -> { x = 1 };
        f()#x == 1
    }

    @Test
    def testApplyRecordSelect02(): Bool = {
        let r = { f = () -> 42 };
        r#f() == 42
    }

    @Test
    def testApplyRecordSelect03(): Bool = {
        let f = () -> { g = () -> { y = 12 } };
        f()#g()#y == 12
    }

    @Test
    def testApplyRecordSelect4(): Bool = {
        let f = () -> { g = () -> { h = () -> 12 } };
        f()#g()#h() == 12
    }

    @Test
    def testApplyRecordSelect5(): Bool = {
        let f = () -> { g = () -> { h = () -> { x = 13 } } };
        f()#g()#h()#x == 13
    }

    @Test
    def testApplyRecordSelect6(): Bool = {
        let f = y -> { g = () -> { h = () -> { x = y + 13 } } };
        f(2)#g()#h()#x == 15
    }

}
