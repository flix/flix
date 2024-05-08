mod Test.Exp.Record.Multiple {

    @test
    def testMultipleRecordOps01(): { a = Int32, d = Int32 } =
        let r = { a = 1, b = 2, c = 3};
        { a = 42, -b, -c, +d = 84 | r }

    @test
    def testMultipleRecordOps02(): {|} =
        let r = { a = 1, b = 2, c = 3};
        { -a, -b, -c, a = 7, b = 8, c = 9 | r }

    @test
    def testMultipleRecordOps03(): { d = Int32 } =
        let r = { a = 1, b = 2, c = 3};
        { d = 1, +d = 2, -c, -b, -a | r }

}
