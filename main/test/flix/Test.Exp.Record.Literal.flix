mod Test.Exp.Record.Literal {

    @Test
    def testEmptyRecord01(): {} =
        {}

    @Test
    def testEmptyRecord02(): { a = {} } =
        { a = {} }

    @Test
    def testEmptyRecord03(): { a = {}, b = {} } =
        { a = {}, b = {} }

    @Test
    def testEmptyRecord04(): { a = { b = { c = {} } } } =
        { a = { b = { c = {} } } }

    @Test
    def testRecordLiteral01(): { name = String } =
        { name = "Lucky Luke" }

    @Test
    def testRecordLiteral02(): { fstName = String, lstName = String } =
        { fstName = "Lucky", lstName = "Luke" }

    @Test
    def testRecordLiteral03(): { fstName = String, lstName = String, age = Int32 } =
        { fstName = "Lucky", lstName = "Luke", age = 42 }

    @Test
    def testRecordLiteral04(): { fstName = String, lstName = String, age = Int32, cowboy = Bool } =
        { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true }

    @Test
    def testRecordLiteral05(): { x = Int32, y = Int32 } =
        { x = 0, y = 0 }

    @Test
    def testRecordLiteral06(): { x = Int32, y = Int32, z = Int32 } =
        { x = 0, y = 0, z = 0 }

    @Test
    def testRecordLiteralOrder01(): { x = Int32, y = Int32, z = Int32 } =
        { x = 1, y = 2, z = 3 }

    @Test
    def testRecordLiteralOrder02(): { x = Int32, y = Int32, z = Int32 } =
        { y = 2, z = 3, x = 1 }

    @Test
    def testRecordLiteralOrder03(): { x = Int32, y = Int32, z = Int32 } =
        { z = 3, x = 1, y = 2 }

    @Test
    def testNestedRecordLiteral01(): { a = { x = Int32, y = Int32 } } =
        { a = { x = 1, y = 2 } }

    @Test
    def testNestedRecordLiteral02(): { a = { x = Int32, y = Int32 }, b = { x = Int32, y = Int32 } } =
        { a = { x = 1, y = 2 }, b = { x = 3, y = 4 } }

    @Test
    def testNestedRecordLiteral03(): { a = { b  = { c = { d = Int32 } } } } =
        { a = { b = { c = { d = 42 } } } }

    @Test
    def testDuplicateLabels01(): { a = Int32, a = Int32 } =
        { a = 21, a = 42 }

    @Test
    def testDuplicateLabels02(): { a = Int32, a = Int32, a = Int32 } =
        { a = 21, a = 42, a = 84 }

    @Test
    def testDuplicateLabels03(): { a = Bool, a = Char, a = Int32 } =
        { a = true, a = 'x', a = 42 }

    @Test
    def testDuplicateLabels04(): { a = Int32, b = Bool, a = Int32, b = Bool } =
        { a = 21, b = true, a = 42, b = false }

    @Test
    def testNestedLabels01(): { a = { a = { a = {} } } } =
        { a = { a = { a = {} } } }

}
