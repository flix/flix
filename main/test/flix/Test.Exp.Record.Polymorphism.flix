mod Test.Exp.Record.Polymorphism {

    @test
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

    @test
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

    @test
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

    @test
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

    @test
    def testRecordPolySelect01(): Bool =
        let p1 = { fstName = "Luke", lstName = "Skywalker" };
        withAgeAndSex(21, "m", p1)#fstName == "Luke"

    @test
    def testRecordPolySelect02(): Bool =
        let p1 = { fstName = "Luke", lstName = "Skywalker" };
        withAgeAndSex(21, "m", p1)#lstName == "Skywalker"

    @test
    def testRecordPolySelect03(): Bool =
        let p1 = { fstName = "Luke", lstName = "Skywalker" };
        withAgeAndSex(21, "m", p1)#age == 21

    @test
    def testRecordPolySelect04(): Bool =
        let p1 = { fstName = "Luke", lstName = "Skywalker" };
        withAgeAndSex(21, "m", p1)#sex == "m"

    @test
    def testRecordPolySelectDuplicate01(): Bool =
        let p1 = { fstName = "Luke", lstName = "Skywalker" };
        let p2 = p1 |> withAgeAndSex(21, "m");
        let p3 = p2 |> withAgeAndSex(22, "f");
        p3#age == 22 and p3#sex == "f"

    @test
    def testRecordPolySelectDuplicate02(): Bool =
        let p1 = { fstName = "Luke", lstName = "Skywalker" };
        let p2 = p1 |> withAgeAndSex(21, "m");
        let p3 = p2 |> withAgeAndSex(22, "f");
        let p4 = p3 |> withAgeAndSex(23, "m");
        p4#age == 23 and p4#sex == "m"

    def |>(x: a, f: a -> b \ ef): b \ ef = f(x)

}
