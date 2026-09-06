mod Test.Exp.Jvm.NewObject.Protected {

    use Assert.assertEq

    import dev.flix.test.TestClassWithProtectedMethods

    // Implement a protected abstract method; observe it through a public caller.
    // Regression test for https://github.com/flix/flix/issues/11415.
    @Test
    def testProtectedAbstract01(): Unit \ Assert + IO =
        let obj = new TestClassWithProtectedMethods {
            def protectedAbstractMethod(_this: TestClassWithProtectedMethods, x: Int32): Int32 = x + 1
        };
        assertEq(expected = 6, obj.callProtectedAbstract(5))

    // The override is dispatched virtually from the superclass's public method.
    @Test
    def testProtectedAbstract02(): Unit \ Assert + IO =
        let obj = new TestClassWithProtectedMethods {
            def protectedAbstractMethod(_this: TestClassWithProtectedMethods, x: Int32): Int32 = x * x
        };
        assertEq(expected = 49, obj.callProtectedAbstract(7))

    // Override both the protected abstract method and a protected concrete method.
    @Test
    def testProtectedConcrete01(): Unit \ Assert + IO =
        let obj = new TestClassWithProtectedMethods {
            def protectedAbstractMethod(_this: TestClassWithProtectedMethods, x: Int32): Int32 = x
            def protectedConcreteMethod(_this: TestClassWithProtectedMethods, y: String): String = "over:" + y
        };
        assertEq(expected = "over:hi", obj.callProtectedConcrete("hi"))

    // A protected concrete method that is NOT overridden keeps the superclass behavior.
    @Test
    def testProtectedConcrete02(): Unit \ Assert + IO =
        let obj = new TestClassWithProtectedMethods {
            def protectedAbstractMethod(_this: TestClassWithProtectedMethods, x: Int32): Int32 = x
        };
        assertEq(expected = "base:hi", obj.callProtectedConcrete("hi"))

}
