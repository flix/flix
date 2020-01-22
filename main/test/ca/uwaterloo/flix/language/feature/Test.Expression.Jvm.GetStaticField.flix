@test
def testGetStaticBoolField01(): Bool =
    import get flix.test.TestClass:BOOL_FIELD as getField;
    getField() == true

@test
def testGetStaticCharField01(): Bool =
    import get flix.test.TestClass:CHAR_FIELD as getField;
    getField() == 'A'

