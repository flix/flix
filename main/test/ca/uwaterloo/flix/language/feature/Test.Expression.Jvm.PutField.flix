namespace Test/Expression/Jvm/PutField {

    @test
    def testGetField01(): Bool =
        import flix.test.TestClass:__new__() as newObject;
        import get flix.test.TestClass.boolField as getField;
        import put flix.test.TestClass.boolField as setField;
        let o = newObject();
        o.setField(false);
        getField(o) == false



}