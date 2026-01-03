mod Test.Exp.Record.Polymorphism {

    use Assert.assertEq

    @CompileTest
    def testRecordNonPolyExtend01(): Unit =
        let p1 = { x = 0, y = 0 };
        let p2 = { x = 1, y = 2 };
        let p3 = { x = 7, y = 8 };
        let _p4 = withZ(p1, 21);
        let _p5 = withZ(p2, 42);
        let _p6 = withZ(p3, 84);
        ()

    def withZ(rec: { x = Int32, y = Int32 }, z: Int32): { x = Int32, y = Int32, z = Int32 } =
        { +z = z | rec }

    @CompileTest
    def testRecordNonPolyExtend02(): Unit =
        let p1 = { fstName = "Luke", lstName = "Skywalker" };
        let p2 = { fstName = "Lucky", lstName = "Luke" };
        let p3 = { fstName = "Gandalf", lstName = "the Grey" };
        let _p4 = withAge(p1, 21);
        let _p5 = withAge(p2, 42);
        let _p6 = withAge(p3, 84);
        ()

    def withAge(rec: { fstName = String, lstName = String }, age: Int32): { fstName = String, lstName = String, age = Int32 } =
        { +age = age | rec }

    @CompileTest
    def testRecordPolyExtend01(): Unit =
        let p1 = { x = 0, y = 0 };
        let p2 = { x = 0, y = 0, z = 0};
        let p3 = { fstName = "Luke", lstName = "Skywalker" };
        let p4 = { fstName = "Darth", lstName = "Vader" };
        let _p5 = withColor(p1, "Red");
        let _p6 = withColor(p2, "Blu");
        let _p7 = withColor(p3, "White");
        let _p8 = withColor(p4, "Black");
        ()

    def withColor(rec: {| r}, col: String): { color = String | r } =
        { +color = col | rec }

    @CompileTest
    def testRecordPolyExtend02(): Unit =
        let p1 = { fstName = "Luke", lstName = "Skywalker" };
        let p2 = { fstName = "Lucky", lstName = "Luke" };
        let p3 = { fstName = "Bruce", lstName = "Wayne", aka = "Batman" };
        let p4 = { name = "Cher" };
        let _p5 = withAgeAndSex(21, "m", p1);
        let _p6 = withAgeAndSex(42, "m", p2);
        let _p7 = withAgeAndSex(42, "m", p3);
        let _p8 = withAgeAndSex(42, "f", p4);
        ()

    def withAgeAndSex(age: Int32, sex: String, rec: {| r}): { age = Int32, sex = String | r } =
        { +age = age, +sex = sex | rec }

    @Test
    def testRecordPolySelect01(): Unit \ Assert =
        let p1 = { fstName = "Luke", lstName = "Skywalker" };
        assertEq(expected = "Luke", withAgeAndSex(21, "m", p1)#fstName)

    @Test
    def testRecordPolySelect02(): Unit \ Assert =
        let p1 = { fstName = "Luke", lstName = "Skywalker" };
        assertEq(expected = "Skywalker", withAgeAndSex(21, "m", p1)#lstName)

    @Test
    def testRecordPolySelect03(): Unit \ Assert =
        let p1 = { fstName = "Luke", lstName = "Skywalker" };
        assertEq(expected = 21, withAgeAndSex(21, "m", p1)#age)

    @Test
    def testRecordPolySelect04(): Unit \ Assert =
        let p1 = { fstName = "Luke", lstName = "Skywalker" };
        assertEq(expected = "m", withAgeAndSex(21, "m", p1)#sex)

    @Test
    def testRecordPolySelectDuplicate01(): Unit \ Assert =
        let p1 = { fstName = "Luke", lstName = "Skywalker" };
        let p2 = p1 |> withAgeAndSex(21, "m");
        let p3 = p2 |> withAgeAndSex(22, "f");
        assertEq(expected = 22, p3#age);
        assertEq(expected = "f", p3#sex)

    @Test
    def testRecordPolySelectDuplicate02(): Unit \ Assert =
        let p1 = { fstName = "Luke", lstName = "Skywalker" };
        let p2 = p1 |> withAgeAndSex(21, "m");
        let p3 = p2 |> withAgeAndSex(22, "f");
        let p4 = p3 |> withAgeAndSex(23, "m");
        assertEq(expected = 23, p4#age);
        assertEq(expected = "m", p4#sex)

}
