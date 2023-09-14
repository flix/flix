mod Test.Exp.Record.Select {

    @Test
    def testRecordSelect01(): String =
        let r = { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
        r.fstName

    @Test
    def testRecordSelect02(): String =
        let r = { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
        r.lstName

    @Test
    def testRecordSelect03(): Int32 =
        let r = { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
        r.age

    @Test
    def testRecordSelect04(): Bool =
        let r = { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
        r.cowboy

    @Test
    def testRecordMultiSelect01(): Unit =
        let r = { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
        let _fstName = r.fstName;
        let _lstName = r.lstName;
        let _age = r.age;
        let _cowboy = r.cowboy;
        ()

    @Test
    def testRecordMultiSelect02(): Unit =
        let r = { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
        let _cowboy = r.cowboy;
        let _age = r.age;
        let _lstName = r.lstName;
        let _fstName = r.fstName;
        ()

    @Test
    def testRecordMultiSelect03(): Unit =
        let r = { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
        let _fstName1 = r.fstName;
        let _fstName2 = r.fstName;
        let _lstName1 = r.lstName;
        let _lstName2 = r.lstName;
        ()

    @Test
    def testRecordMultiSelect04(): Unit =
        let r = { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
        let _lstName1 = r.lstName;
        let _fstName1 = r.fstName;
        let _lstName2 = r.lstName;
        let _fstName2 = r.fstName;
        ()

    @Test
    def testNestedSelect01(): Unit =
        let r = { a = { x = 1, y = 2}, b = { x = 1, y = 4 } };
        let _ax = r.a.x;
        let _ay = r.a.y;
        let _bx = r.b.x;
        let _by = r.b.y;
        ()

    @Test
    def testNestedSelect02(): Unit =
        let r = { a = { b = { c = { d = 42 } } } };
        let _a = r.a;
        let _b = r.a.b;
        let _c = r.a.b.c;
        let _d = r.a.b.c.d;
        ()

    @Test
    def testSelectDuplicateLabels01(): Int32 =
        let r = { a = 21, a = 42, a = 84};
        let a1 = r.a;
        let a2 = { -a | r }.a;
        let a3 = { -a, -a | r }.a;
        a1 + a2 + a3

    @Test
    def testSelectDuplicateLabels02(): Int32 =
        let r = { a = 21, b = true, a = 42, b = false, a = 84};
        let a1 = r.a;
        let a2 = { -a | r }.a;
        let a3 = { -a, -a | r }.a;
        a1 + a2 + a3

    @Test
    def testSelectDuplicateLabels03(): String =
        let r = { a = true, a = "hello world", a = 84};
        { -a | r }.a

    @Test
    def testSelectDuplicateLabels04(): Int32 =
        let r = { a = true, a = "hello world", a = 84};
        { -a, -a | r }.a

}
