mod Test.Exp.Jvm.NewObject.Annotations {

    use Assert.{assertEq, assertTrue}

    import dev.flix.test.TestJvmAnnotation
    import dev.flix.test.TestJvmAnnotationClassRetention
    import dev.flix.test.TestBoolInterface
    import dev.flix.test.TestInt32Interface
    import dev.flix.test.TestVoidInterface
    import dev.flix.test.TestOverloadedMethods
    import dev.flix.test.TestDefaultMethods
    import dev.flix.test.TestJvmAnnotationHelper
    import java.lang.Object

    /// Test that a RUNTIME-retained JVM annotation on a method in an anonymous class
    /// is emitted to bytecode and visible via reflection.
    @Test
    def testRuntimeAnnotationPresent01(): Unit \ Assert + IO =
        let anon = new TestBoolInterface {
            @TestJvmAnnotation
            def testMethod(_this: TestBoolInterface, x: Bool): Bool = not x
        };
        let obj: Object = unchecked_cast(anon as Object);
        assertTrue(TestJvmAnnotationHelper.hasTestJvmAnnotation(obj, "testMethod"))

    /// Test that a method without JVM annotations has no TestJvmAnnotation.
    @Test
    def testNoAnnotationWhenAbsent01(): Unit \ Assert + IO =
        let anon = new TestBoolInterface {
            def testMethod(_this: TestBoolInterface, x: Bool): Bool = not x
        };
        let obj: Object = unchecked_cast(anon as Object);
        assertTrue(not TestJvmAnnotationHelper.hasTestJvmAnnotation(obj, "testMethod"))

    /// Test annotation on an Int32 interface method.
    @Test
    def testRuntimeAnnotationPresent02(): Unit \ Assert + IO =
        let anon = new TestInt32Interface {
            @TestJvmAnnotation
            def testMethod(_this: TestInt32Interface, x: Int32): Int32 = x + 1
        };
        let obj: Object = unchecked_cast(anon as Object);
        assertTrue(TestJvmAnnotationHelper.hasTestJvmAnnotation(obj, "testMethod"))

    /// Test annotation on a void interface method.
    @Test
    def testRuntimeAnnotationPresent03(): Unit \ Assert + IO =
        let anon = new TestVoidInterface {
            @TestJvmAnnotation
            def testMethod(_this: TestVoidInterface): Unit = ()
        };
        let obj: Object = unchecked_cast(anon as Object);
        assertTrue(TestJvmAnnotationHelper.hasTestJvmAnnotation(obj, "testMethod"))

    /// Test that an annotated Bool method still executes correctly.
    @Test
    def testAnnotationDoesNotAffectExecution01(): Unit \ Assert + IO =
        let anon = new TestBoolInterface {
            @TestJvmAnnotation
            def testMethod(_this: TestBoolInterface, x: Bool): Bool = not x
        };
        assertTrue(TestBoolInterface.runTest(anon))

    /// Test that an annotated Int32 method still executes correctly.
    @Test
    def testAnnotationDoesNotAffectExecution02(): Unit \ Assert + IO =
        let anon = new TestInt32Interface {
            @TestJvmAnnotation
            def testMethod(_this: TestInt32Interface, x: Int32): Int32 = x + 1
        };
        assertTrue(TestInt32Interface.runTest(anon))

    /// Test annotation on a method that captures a lexical variable.
    @Test
    def testAnnotationWithClosure01(): Unit \ Assert + IO =
        let offset = 10;
        let anon = new TestInt32Interface {
            @TestJvmAnnotation
            def testMethod(_this: TestInt32Interface, x: Int32): Int32 = x + offset
        };
        let obj: Object = unchecked_cast(anon as Object);
        assertTrue(TestJvmAnnotationHelper.hasTestJvmAnnotation(obj, "testMethod"))

    /// Test annotation on all overloaded methods.
    @Test
    def testAnnotationOnOverloadedMethods01(): Unit \ Assert + IO =
        let anon = new TestOverloadedMethods {
            @TestJvmAnnotation
            def overloadedMethod(_this: TestOverloadedMethods): Int32 = 42
            @TestJvmAnnotation
            def overloadedMethod(_this: TestOverloadedMethods, x: Int32): Int32 = x + 1
            @TestJvmAnnotation
            def overloadedMethod(_this: TestOverloadedMethods, x: String, y: Float64, z: Float64): String = "${x}${y / z}"
        };
        let obj: Object = unchecked_cast(anon as Object);
        assertTrue(TestJvmAnnotationHelper.hasTestJvmAnnotation(obj, "overloadedMethod"))

    /// Test annotation on a method that overrides a default method.
    @Test
    def testAnnotationOnDefaultMethodOverride01(): Unit \ Assert + IO =
        let anon = new TestDefaultMethods {
            @TestJvmAnnotation
            def methodWithNoImplementation(_this: TestDefaultMethods, x: Int32): Int32 = x + 1
            @TestJvmAnnotation
            def methodWithDefaultImplementation(_this: TestDefaultMethods, x: Int32): Int32 = x + 2
        };
        let obj: Object = unchecked_cast(anon as Object);
        assertTrue(TestJvmAnnotationHelper.hasTestJvmAnnotation(obj, "methodWithNoImplementation"));
        assertTrue(TestJvmAnnotationHelper.hasTestJvmAnnotation(obj, "methodWithDefaultImplementation"))

    /// Test that a CLASS-retained annotation is not visible at runtime.
    @Test
    def testClassRetentionNotVisibleAtRuntime01(): Unit \ Assert + IO =
        let anon = new TestBoolInterface {
            @TestJvmAnnotationClassRetention
            def testMethod(_this: TestBoolInterface, x: Bool): Bool = not x
        };
        let obj: Object = unchecked_cast(anon as Object);
        assertTrue(not TestJvmAnnotationHelper.hasTestJvmAnnotationClassRetention(obj, "testMethod"))

    /// Test that both RUNTIME and CLASS annotations can be applied to the same method.
    @Test
    def testMultipleAnnotationsOnMethod01(): Unit \ Assert + IO =
        let anon = new TestBoolInterface {
            @TestJvmAnnotation
            @TestJvmAnnotationClassRetention
            def testMethod(_this: TestBoolInterface, x: Bool): Bool = not x
        };
        let obj: Object = unchecked_cast(anon as Object);
        assertTrue(TestJvmAnnotationHelper.hasTestJvmAnnotation(obj, "testMethod"));
        assertTrue(not TestJvmAnnotationHelper.hasTestJvmAnnotationClassRetention(obj, "testMethod"))

}
