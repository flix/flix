@test
def testGetStaticBoolField01(): Bool =
    import get flix.test.TestClass:BOOL_FIELD as getField;
    getField() == true

@test
def testGetStaticCharField01(): Bool =
    import get flix.test.TestClass:CHAR_FIELD as getField;
    getField() == 'A'

@test
def testGetStaticFloat32Field01(): Bool =
    import get flix.test.TestClass:FLOAT32_FIELD as getField;
    getField() == 123.456f32

@test
def testGetStaticFloat64Field01(): Bool =
    import get flix.test.TestClass:FLOAT64_FIELD as getField;
    getField() == 123.456f64

