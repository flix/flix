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

}
