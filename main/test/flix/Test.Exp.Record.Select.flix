mod Test.Exp.Record.Select {

    use Assert.assertEq

    @Test
    def testRecordSelect01(): Unit \ Assert =
        let r = { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
        assertEq(expected = "Lucky", r#fstName)

    @Test
    def testRecordSelect02(): Unit \ Assert =
        let r = { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
        assertEq(expected = "Luke", r#lstName)

    @Test
    def testRecordSelect03(): Unit \ Assert =
        let r = { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
        assertEq(expected = 42, r#age)

    @Test
    def testRecordSelect04(): Unit \ Assert =
        let r = { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
        assertEq(expected = true, r#cowboy)

    @Test
    def testRecordMultiSelect01(): Unit \ Assert =
        let r = { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
        assertEq(expected = "Lucky", r#fstName);
        assertEq(expected = "Luke", r#lstName);
        assertEq(expected = 42, r#age);
        assertEq(expected = true, r#cowboy)

    @Test
    def testRecordMultiSelect02(): Unit \ Assert =
        let r = { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
        assertEq(expected = true, r#cowboy);
        assertEq(expected = 42, r#age);
        assertEq(expected = "Luke", r#lstName);
        assertEq(expected = "Lucky", r#fstName)

    @Test
    def testRecordMultiSelect03(): Unit \ Assert =
        let r = { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
        assertEq(expected = "Lucky", r#fstName);
        assertEq(expected = "Lucky", r#fstName);
        assertEq(expected = "Luke", r#lstName);
        assertEq(expected = "Luke", r#lstName)

    @Test
    def testRecordMultiSelect04(): Unit \ Assert =
        let r = { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
        assertEq(expected = "Luke", r#lstName);
        assertEq(expected = "Lucky", r#fstName);
        assertEq(expected = "Luke", r#lstName);
        assertEq(expected = "Lucky", r#fstName)

    @Test
    def testNestedSelect01(): Unit \ Assert =
        let r = { a = { x = 1, y = 2}, b = { x = 1, y = 4 } };
        assertEq(expected = 1, r#a#x);
        assertEq(expected = 2, r#a#y);
        assertEq(expected = 1, r#b#x);
        assertEq(expected = 4, r#b#y)

    @Test
    def testNestedSelect02(): Unit \ Assert =
        let r = { a = { b = { c = { d = 42 } } } };
        assertEq(expected = 42, r#a#b#c#d)

    @Test
    def testSelectDuplicateLabels01(): Unit \ Assert =
        let r = { a = 21, a = 42, a = 84};
        let a1 = r#a;
        let a2 = { -a | r }#a;
        let a3 = { -a, -a | r }#a;
        assertEq(expected = 147, a1 + a2 + a3)

    @Test
    def testSelectDuplicateLabels02(): Unit \ Assert =
        let r = { a = 21, b = true, a = 42, b = false, a = 84};
        let a1 = r#a;
        let a2 = { -a | r }#a;
        let a3 = { -a, -a | r }#a;
        assertEq(expected = 147, a1 + a2 + a3)

    @Test
    def testSelectDuplicateLabels03(): Unit \ Assert =
        let r = { a = true, a = "hello world", a = 84};
        assertEq(expected = "hello world", { -a | r }#a)

    @Test
    def testSelectDuplicateLabels04(): Unit \ Assert =
        let r = { a = true, a = "hello world", a = 84};
        assertEq(expected = 84, { -a, -a | r }#a)

}
