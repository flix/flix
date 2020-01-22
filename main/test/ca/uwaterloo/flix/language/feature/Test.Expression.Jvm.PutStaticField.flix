@test
def testPutStaticBoolField01(): Bool =
    import get flix.test.TestClass:BOOL_FIELD as getField;
    import put flix.test.TestClass:BOOL_FIELD as putField;
    putField(true);
    getField() == true

@test
def testPutStaticBoolField02(): Bool =
    import get flix.test.TestClass:BOOL_FIELD as getField;
    import put flix.test.TestClass:BOOL_FIELD as putField;
    putField(false);
    getField() == false

@test
def testPutStaticCharField01(): Bool =
    import get flix.test.TestClass:CHAR_FIELD as getField;
    import put flix.test.TestClass:CHAR_FIELD as putField;
    putField('a');
    getField() == 'a'

@test
def testPutStaticCharField02(): Bool =
    import get flix.test.TestClass:CHAR_FIELD as getField;
    import put flix.test.TestClass:CHAR_FIELD as putField;
    putField('z');
    getField() == 'z'

@test
def testPutStaticCharField03(): Bool =
    import get flix.test.TestClass:CHAR_FIELD as getField;
    import put flix.test.TestClass:CHAR_FIELD as putField;
    putField('A');
    getField() == 'A'

@test
def testPutStaticCharField04(): Bool =
    import get flix.test.TestClass:CHAR_FIELD as getField;
    import put flix.test.TestClass:CHAR_FIELD as putField;
    putField('Z');
    getField() == 'Z'

