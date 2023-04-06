mod Test.Exp.Record.Extend {

    @test
    def testRecordExtend01(): { fstName = String } =
        let r0 = {};
        let r1 = { +fstName = "Lucky" | r0 };
        r1

    @test
    def testRecordExtend02(): { fstName = String, lstName = String } =
        let r0 = {};
        let r1 = { +fstName = "Lucky" | r0 };
        let r2 = { +lstName = "Luke" | r1 };
        r2

    @test
    def testRecordExtend03(): { fstName = String, lstName = String, age = Int32 } =
        let r0 = {};
        let r1 = { +fstName = "Lucky" | r0 };
        let r2 = { +lstName = "Luke" | r1 };
        let r3 = { +age = 42 | r2 };
        r3

    @test
    def testRecordExtend04(): { fstName = String, lstName = String, age = Int32, cowboy = Bool } =
        let r0 = {};
        let r1 = { +fstName = "Lucky" | r0 };
        let r2 = { +lstName = "Luke" | r1 };
        let r3 = { +age = 42 | r2 };
        let r4 = { +cowboy = true | r3 };
        r4

    @test
    def testRecordForkExtend01(): Unit =
        let r = { lstName = "Lannister" };
        let _ = { +fstName = "Tywin" | r };
        let _ = { +fstName = "Jaime" | r };
        let _ = { +lstName = "Cersei" | r };
        let _ = { +lstName = "Tyrion" | r };
        ()

    @test
    def testRecordForkExtend02(): Unit =
        let r0 = { x = 0, y = 0 };
        let r1 = { +z = 0 | r0 };
        let _ = { +color = "red" | r1 };
        let _ = { +color = "blu" | r1 };
        ()

    @test
    def testRecordDuplicateExtend01(): { x = Bool, x = Int32, x = String } =
        let r0 = { +x = "ab" | {} };
        let r1 = { +x = 1234 | r0 };
        let r2 = { +x = true | r1 };
            r2

    @test
    def testRecordDuplicateExtend02(): { x = Int32, x = Int32, x = Int32 } =
        let r0 = { +x = 123 | {} };
        let r1 = { +x = 456 | r0 };
        let r2 = { +x = 789 | r1 };
            r2

    @test
    def testRecordDuplicateExtend03(): { x = Int32, y = Char, x = Int32, y = Char, x = Int32 } =
        let r0 = { +x = 123 | {} };
        let r1 = { +y = 'a' | r0 };
        let r2 = { +x = 456 | r1 };
        let r3 = { +y = 'b' | r2 };
        let r4 = { +x = 789 | r3 };
            r4

    @test
    def testRecordDuplicateExtend04(): Bool =
        let r0 = { +x = 123 | {} };
        let r1 = { +x = "abc" | r0 };
        let r2 = { +x = "def" | r0 };
            r1.x != r2.x

}
