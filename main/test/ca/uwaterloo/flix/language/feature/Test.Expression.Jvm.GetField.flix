namespace Test/Expression/Jvm/GetField {

    @test
    def testGetField01(): Bool =
        import flix.test.TestClass:__new__() as newObject;
        import get flix.test.TestClass.boolField as getBoolField;
        let o = newObject();
        getBoolField(o) == true

    @test
    def testGetField02(): Bool =
        import flix.test.TestClass:__new__() as newObject;
        import get flix.test.TestClass.charField as getCharField;
        let o = newObject();
        getCharField(o) == 'A'



}