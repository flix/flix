@test
def testEmptyRecord01(): {} =
    {}

@test
def testEmptyRecord02(): { a: {} } =
    { a = {} }

@test
def testEmptyRecord03(): { a: {}, b: {} } =
    { a = {}, b = {} }

@test
def testEmptyRecord04(): { a: { b: { c: {} } } } =
    { a = { b = { c = {} } } }

@test
def testRecordLiteral01(): { name: String } =
    { name = "Lucky Luke" }

@test
def testRecordLiteral02(): { fstName: String, lstName: String } =
    { fstName = "Lucky", lstName = "Luke" }

@test
def testRecordLiteral03(): { fstName: String, lstName: String, age: Int } =
    { fstName = "Lucky", lstName = "Luke", age = 42 }

@test
def testRecordLiteral04(): { fstName: String, lstName: String, age: Int, cowboy: Bool } =
    { fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true }

@test
def testRecordLiteral05(): { x: Int, y: Int } =
    { x = 0, y = 0 }

@test
def testRecordLiteral06(): { x: Int, y: Int, z: Int } =
    { x = 0, y = 0, z = 0 }

@test
def testRecordLiteralOrder01(): { x: Int, y: Int, z: Int } =
    { x = 1, y = 2, z = 3 }

@test
def testRecordLiteralOrder02(): { x: Int, y: Int, z: Int } =
    { y = 2, z = 3, x = 1 }

@test
def testRecordLiteralOrder03(): { x: Int, y: Int, z: Int } =
    { z = 3, x = 1, y = 2 }

@test
def testNestedRecordLiteral01(): { a: { x: Int, y: Int } } =
    { a = { x = 1, y = 2 } }

@test
def testNestedRecordLiteral02(): { a: { x: Int, y: Int }, b: { x: Int, y: Int } } =
    { a = { x = 1, y = 2 }, b = { x = 3, y = 4 } }

@test
def testNestedRecordLiteral03(): { a: { b : { c: { d: Int } } } } =
    { a = { b = { c = { d = 42 } } } }

@test
def testDuplicateLabels01(): { a: Int, a: Int } =
    { a = 21, a = 42 }

@test
def testDuplicateLabels02(): { a: Int, a: Int, a: Int } =
    { a = 21, a = 42, a = 84 }

@test
def testDuplicateLabels03(): { a: Bool, a: Char, a: Int } =
    { a = true, a = 'x', a = 42 }

@test
def testDuplicateLabels04(): { a: Int, b: Bool, a: Int, b: Bool } =
    { a = 21, b = true, a = 42, b = false }

@test
def testNestedLabels01(): { a: { a: { a: {} } } } =
    { a = { a = { a = {} } } }
