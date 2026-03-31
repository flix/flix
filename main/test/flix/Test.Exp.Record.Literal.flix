mod Test.Exp.Record.Literal {

    @CompileTest
    def testEmptyRecord01(): {|} =
        {}

    @CompileTest
    def testEmptyRecord02(): { a = {|} } =
        { a = {} }

    @CompileTest
    def testEmptyRecord03(): { a = {|}, b = {|} } =
        { a = {}, b = {} }

    @CompileTest
    def testEmptyRecord04(): { a = { b = { c = {|} } } } =
        { a = { b = { c = {} } } }

    @CompileTest
    def testRecordLiteral01(): { name = String } =
        { name = "Lucky Luke" }

    @CompileTest
    def testRecordLiteral02(): { fstName = String, lstName = String } =
        { fstName = "Lucky", lstName = "Luke" }

    @CompileTest
    def testRecordLiteral03(): { fstName = String, lstName = String, age = Int32 } =
        { fstName = "Lucky", lstName = "Luke", age = 42 }

    @CompileTest
    def testRecordLiteral04(): { fstName = String, lstName = String, age = Int32, cowboy = Bool } =
        { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true }

    @CompileTest
    def testRecordLiteral05(): { x = Int32, y = Int32 } =
        { x = 0, y = 0 }

    @CompileTest
    def testRecordLiteral06(): { x = Int32, y = Int32, z = Int32 } =
        { x = 0, y = 0, z = 0 }

    @CompileTest
    def testRecordLiteralOrder01(): { x = Int32, y = Int32, z = Int32 } =
        { x = 1, y = 2, z = 3 }

    @CompileTest
    def testRecordLiteralOrder02(): { x = Int32, y = Int32, z = Int32 } =
        { y = 2, z = 3, x = 1 }

    @CompileTest
    def testRecordLiteralOrder03(): { x = Int32, y = Int32, z = Int32 } =
        { z = 3, x = 1, y = 2 }

    @CompileTest
    def testNestedRecordLiteral01(): { a = { x = Int32, y = Int32 } } =
        { a = { x = 1, y = 2 } }

    @CompileTest
    def testNestedRecordLiteral02(): { a = { x = Int32, y = Int32 }, b = { x = Int32, y = Int32 } } =
        { a = { x = 1, y = 2 }, b = { x = 3, y = 4 } }

    @CompileTest
    def testNestedRecordLiteral03(): { a = { b  = { c = { d = Int32 } } } } =
        { a = { b = { c = { d = 42 } } } }

    @CompileTest
    def testDuplicateLabels01(): { a = Int32, a = Int32 } =
        { a = 21, a = 42 }

    @CompileTest
    def testDuplicateLabels02(): { a = Int32, a = Int32, a = Int32 } =
        { a = 21, a = 42, a = 84 }

    @CompileTest
    def testDuplicateLabels03(): { a = Bool, a = Char, a = Int32 } =
        { a = true, a = 'x', a = 42 }

    @CompileTest
    def testDuplicateLabels04(): { a = Int32, b = Bool, a = Int32, b = Bool } =
        { a = 21, b = true, a = 42, b = false }

    @CompileTest
    def testNestedLabels01(): { a = { a = { a = {|} } } } =
        { a = { a = { a = {} } } }

}
