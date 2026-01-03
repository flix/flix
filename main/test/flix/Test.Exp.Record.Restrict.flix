mod Test.Exp.Record.Restrict {

    @CompileTest
    def testRecordRestrict01(): { lstName = String, age = Int32, cowboy = Bool } =
        let r0 = { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
        let r1 = { -fstName | r0 };
        r1

    @CompileTest
    def testRecordRestrict02(): { age = Int32, cowboy = Bool } =
        let r0 = { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
        let r1 = { -fstName | r0 };
        let r2 = { -lstName | r1 };
        r2

    @CompileTest
    def testRecordRestrict03(): { cowboy = Bool } =
        let r0 = { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
        let r1 = { -fstName | r0 };
        let r2 = { -lstName | r1 };
        let r3 = { -age | r2 };
        r3

    @CompileTest
    def testRecordRestrict04(): {|} =
        let r0 = { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
        let r1 = { -fstName | r0 };
        let r2 = { -lstName | r1 };
        let r3 = { -age | r2 };
        let r4 = { -cowboy | r3 };
        r4

    @CompileTest
    def testRecordRestrict05(): Bool =
        let r0 = { a = 123, a = 456 };
        let r1 = { -a | r0 };
        r1#a == 456

    @CompileTest
    def testRecordRestrict06(): Bool =
        let r0 = { a = 123, a = 456, a = 789 };
        let r1 = { -a | r0 };
        let r2 = { -a | r1 };
        r2#a == 789

    @CompileTest
    pub def testRecordRestrict07(): Bool = {
        // test that restrict does not mutate `r1`.
        // the arbitrary value juggling is to avoid inlining
        let r1 = {x = 1, y = 1, z = 1};
        let r2 = {-z | r1};
        let tple = (r1#z + r2#y) |> (i -> (r1#z, i));
        fst(tple) == 1
    }

    @CompileTest
    def testRecordRestrictMultiple01(): { fstName = String, lstName = String } =
        let r0 = { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
        { -age, -cowboy | r0 }

    @CompileTest
    def testRecordRestrictMultiple02(): { age = Int32, cowboy = Bool } =
        let r0 = { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
        { -fstName, -lstName | r0 }

    @CompileTest
    def testRecordRestrictUnordered01(): {|} =
        let r0 = { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
        { -lstName, -fstName, -age, -cowboy | r0 }

    @CompileTest
    def testRecordRestrictUnordered02(): {|} =
        let r0 = { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
        { -lstName, -age, -fstName, -cowboy | r0 }

    @CompileTest
    def testRecordRestrictUnordered03(): {|} =
        let r0 = { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
        {  -cowboy, -age, -lstName, -fstName | r0 }

    @CompileTest
    def testRecordRestrictDuplicateLabels01(): { a = Int32, a = Int32 } =
        let r = { a = 1, a = 2, a = 3};
        { -a | r }

    @CompileTest
    def testRecordRestrictDuplicateLabels02(): { a = Int32 } =
        let r = { a = 1, a = 2, a = 3};
        { -a, -a | r }

    @CompileTest
    def testRecordRestrictDuplicateLabels03(): {|} =
        let r = { a = 1, a = 2, a = 3};
        { -a, -a, -a | r }

    @CompileTest
    def testRecordRestrictDuplicateLabels04(): { b = Int32, a = Int32, b = Int32, a = Int32 } =
        let r = { a = 1, b = 42, a = 2, b = 84, a = 3};
        { -a | r }

    @CompileTest
    def testRecordRestrictDuplicateLabels05(): { a = Int32, b = Int32, a = Int32 } =
        let r = { a = 1, b = 42, a = 2, b = 84, a = 3};
        { -a, -b | r }

    @CompileTest
    def testRecordRestrictDuplicateLabels06(): { b = Int32, a = Int32 } =
        let r = { a = 1, b = 42, a = 2, b = 84, a = 3};
        { -a, -b, -a | r }

    @CompileTest
    def testRecordRestrictDuplicateLabels07(): { a = Int32 } =
        let r = { a = 1, b = 42, a = 2, b = 84, a = 3};
        { -a, -b, -a, -b | r }

    @CompileTest
    def testRecordRestrictDuplicateLabels08(): {|} =
        let r = { a = 1, b = 42, a = 2, b = 84, a = 3};
        { -a, -b, -a, -b, -a | r }

}
