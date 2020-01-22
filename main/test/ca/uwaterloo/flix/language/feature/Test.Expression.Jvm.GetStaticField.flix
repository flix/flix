@test
def testGetStaticField01(): Bool =
    import get flix.test.TestClass:BOOL_FIELD as getField;
    getField() == false

