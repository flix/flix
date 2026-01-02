mod Test.Exp.Jvm.NewObject {

    use Assert.{assertEq, assertTrue}

    import dev.flix.test.TestBoolArrayInterface
    import dev.flix.test.TestBoolInterface
    import dev.flix.test.TestCharArrayInterface
    import dev.flix.test.TestCharInterface
    import dev.flix.test.TestClass
    import dev.flix.test.TestClassWithDefaultConstructor
    import dev.flix.test.TestClassWithNonDefaultConstructor
    import dev.flix.test.TestDefaultMethods
    import dev.flix.test.TestFloat32ArrayInterface
    import dev.flix.test.TestFloat32Interface
    import dev.flix.test.TestFloat64ArrayInterface
    import dev.flix.test.TestFloat64Interface
    import dev.flix.test.TestFunctionalInterface
    import dev.flix.test.TestGenericInterface
    import dev.flix.test.TestGenericMethod
    import dev.flix.test.TestInt8ArrayInterface
    import dev.flix.test.TestInt16ArrayInterface
    import dev.flix.test.TestInt16Interface
    import dev.flix.test.TestInt32ArrayInterface
    import dev.flix.test.TestInt32Interface
    import dev.flix.test.TestInt64ArrayInterface
    import dev.flix.test.TestInt64Interface
    import dev.flix.test.TestNonPublicInterface
    import dev.flix.test.TestOverloadedMethods
    import dev.flix.test.TestStackOffsets
    import dev.flix.test.TestThrowingInterface
    import dev.flix.test.TestVarargsInterface
    import dev.flix.test.TestVoidInterface
    import dev.flix.test.TestClassWithProtectedConstructor
    import dev.flix.test.{TestClassWithStaticNestedClass$NestedClass => NestedClass}
    import java.io.Serializable
    import java.lang.Object
    import java.lang.Class
    import java.util.Comparator

    def implementSerializable(): Serializable \ IO =
        new Serializable { }

    def implementSerializableAgain(): Serializable \ IO =
        new Serializable { }

    @Test
    def testImplementInterface01(): Unit \ Assert + IO =
        assertTrue(implementSerializable().toString().startsWith("Anon"))

    @Test
    def testImplementInterface02(): Unit \ Assert + IO =
        assertTrue(implementSerializable().hashCode() != implementSerializable().hashCode())

    @Test
    def testImplementInterface03(): Unit \ Assert + IO =
        assertTrue(not implementSerializable().equals(implementSerializable()))

    @Test
    def testImplementInterface04(): Unit \ Assert + IO =
        assertTrue(implementSerializable().getClass().equals(
            implementSerializable().getClass()
        ))

    @Test
    def testImplementInterface05(): Unit \ Assert + IO =
        assertTrue(not implementSerializable().getClass().equals(
            implementSerializableAgain().getClass()
        ))

    @Test
    def testVoidInterface01(): Unit \ Assert + IO =
        let anon = new TestVoidInterface {
            def testMethod(_this: TestVoidInterface): Unit = ()
        };
        assertTrue(TestVoidInterface.runTest(anon))

    @Test
    def testBoolInterface01(): Unit \ Assert + IO =
        let anon = new TestBoolInterface {
            def testMethod(_this: TestBoolInterface, x: Bool): Bool = not x
        };
        assertTrue(TestBoolInterface.runTest(anon))

    @Test
    def testCharInterface01(): Unit \ Assert + IO =
        let anon = new TestCharInterface {
            def testMethod(_this: TestCharInterface, x: Char): Char = Char.toUpperCase(x)
        };
        assertTrue(TestCharInterface.runTest(anon))

    @Test
    def testInt16Interface01(): Unit \ Assert + IO =
        let anon = new TestInt16Interface {
            def testMethod(_this: TestInt16Interface, x: Int16): Int16 = x + 1i16
        };
        assertTrue(TestInt16Interface.runTest(anon))

    @Test
    def testInt32Interface01(): Unit \ Assert + IO =
        let anon = new TestInt32Interface {
            def testMethod(_this: TestInt32Interface, x: Int32): Int32 = x + 1
        };
        assertTrue(TestInt32Interface.runTest(anon))

    @Test
    def testInt64Interface01(): Unit \ Assert + IO =
        let anon = new TestInt64Interface {
            def testMethod(_this: TestInt64Interface, x: Int64): Int64 = x + 1i64
        };
        assertTrue(TestInt64Interface.runTest(anon))

    @Test
    def testFloat32Interface01(): Unit \ Assert + IO =
        let anon = new TestFloat32Interface {
            def testMethod(_this: TestFloat32Interface, x: Float32): Float32 = x + 0.23f32
        };
        assertTrue(TestFloat32Interface.runTest(anon))

    @Test
    def testFloat64Interface01(): Unit \ Assert + IO =
        let anon = new TestFloat64Interface {
            def testMethod(_this: TestFloat64Interface, x: Float64): Float64 = x + 0.23f64
        };
        assertTrue(TestFloat64Interface.runTest(anon))

    @Test
    def testBoolArrayInterface01(): Unit \ Assert + IO =
        let anon = new TestBoolArrayInterface {
            def testMethod(_this: TestBoolArrayInterface, xs: Array[Bool, Static]): Array[Bool, Static] \ IO =
                xs |> Array.map(Static, x -> not x)
        };
        assertTrue(TestBoolArrayInterface.runTest(anon))

    @Test
    def testCharArrayInterface01(): Unit \ Assert + IO =
        let anon = new TestCharArrayInterface {
            def testMethod(_this: TestCharArrayInterface, xs: Array[Char, Static]): Array[Char, Static] \ IO =
                xs |> Array.map(Static, Char.toUpperCase)
        };
        assertTrue(TestCharArrayInterface.runTest(anon))

    @Test
    def testInt8ArrayInterface01(): Unit \ Assert + IO =
        let anon = new TestInt8ArrayInterface {
            def testMethod(_this: TestInt8ArrayInterface, xs: Array[Int8, Static]): Array[Int8, Static] \ IO =
                xs |> Array.map(Static, x -> x + 1i8)
        };
        assertTrue(TestInt8ArrayInterface.runTest(anon))

    @Test
    def testInt16ArrayInterface01(): Unit \ Assert + IO =
        let anon = new TestInt16ArrayInterface {
            def testMethod(_this: TestInt16ArrayInterface, xs: Array[Int16, Static]): Array[Int16, Static] \ IO =
                xs |> Array.map(Static, x -> x + 1i16)
        };
        assertTrue(TestInt16ArrayInterface.runTest(anon))

    @Test
    def testInt32ArrayInterface01(): Unit \ Assert + IO =
        let anon = new TestInt32ArrayInterface {
            def testMethod(_this: TestInt32ArrayInterface, xs: Array[Int32, Static]): Array[Int32, Static] \ IO =
                xs |> Array.map(Static, x -> x + 1)
        };
        assertTrue(TestInt32ArrayInterface.runTest(anon))

    @Test
    def testInt64ArrayInterface01(): Unit \ Assert + IO =
        let anon = new TestInt64ArrayInterface {
            def testMethod(_this: TestInt64ArrayInterface, xs: Array[Int64, Static]): Array[Int64, Static] \ IO =
                xs |> Array.map(Static, x -> x + 1i64)
        };
        assertTrue(TestInt64ArrayInterface.runTest(anon))

    @Test
    def testFloat32ArrayInterface01(): Unit \ Assert + IO =
        let anon = new TestFloat32ArrayInterface {
            def testMethod(_this: TestFloat32ArrayInterface, xs: Array[Float32, Static]): Array[Float32, Static] \ IO =
                xs |> Array.map(Static, x -> x + 0.5f32)
        };
        assertTrue(TestFloat32ArrayInterface.runTest(anon))

    @Test
    def testFloat64ArrayInterface01(): Unit \ Assert + IO =
        let anon = new TestFloat64ArrayInterface {
            def testMethod(_this: TestFloat64ArrayInterface, xs: Array[Float64, Static]): Array[Float64, Static] \ IO =
                xs |> Array.map(Static, x -> x + 0.5f64)
        };
        assertTrue(TestFloat64ArrayInterface.runTest(anon))

    @Test
    def testStackOffsets01(): Unit \ Assert + IO =
        let anon = new TestStackOffsets {
            def testMethod(_this: TestStackOffsets, a: Bool, b: Char, c: Int8, d: Int16, e: Int32, f: Int64, g: Float32, h: Float64): String =
                "${a}, ${b}, ${c}, ${d}, ${e}, ${f}, ${g}, ${h}"
        };
        assertTrue(TestStackOffsets.runTest(anon))

    @Test
    def testOverloadedMethods01(): Unit \ Assert + IO =
        let anon = new TestOverloadedMethods {
            def overloadedMethod(_this: TestOverloadedMethods): Int32 = 42
            def overloadedMethod(_this: TestOverloadedMethods, x: Int32): Int32 = x + 1
            def overloadedMethod(_this: TestOverloadedMethods, x: String, y: Float64, z: Float64): String = "${x}${y / z}"
        };
        assertTrue(TestOverloadedMethods.runTest(anon))

    @Test
    def testDefaultMethods01(): Unit \ Assert + IO =
        let anon = new TestDefaultMethods {
            def methodWithNoImplementation(_this: TestDefaultMethods, x: Int32): Int32 = x + 1
        };
        assertEq(expected = 2, anon.methodWithNoImplementation(1));
        assertEq(expected = 43, anon.methodWithDefaultImplementation(1))

    @Test
    def testDefaultMethods02(): Unit \ Assert + IO =
        let anon = new TestDefaultMethods {
            def methodWithNoImplementation(_this: TestDefaultMethods, x: Int32): Int32 = x + 1
            def methodWithDefaultImplementation(_this: TestDefaultMethods, x: Int32): Int32 = x + 2
        };
        assertEq(expected = 2, anon.methodWithNoImplementation(1));
        assertEq(expected = 3, anon.methodWithDefaultImplementation(1))

    @Test
    def testGenericInterface01(): Unit \ Assert + IO =
        let anon = new TestGenericInterface {
            def testMethod(_this: TestGenericInterface, x: Object): Object =
                let str = unchecked_cast(x as String);
                checked_cast("${str}, ${str}")
        };
        assertTrue(TestGenericInterface.runTest(anon))

    @Test
    def testGenericMethod01(): Unit \ Assert + IO =
        let anon = new TestGenericMethod {
            def testMethod(_this: TestGenericMethod, x: Object): Object =
                let str = unchecked_cast(x as String);
                checked_cast("${str}, ${str}")
        };
        assertTrue(TestGenericMethod.runTest(anon))

    @Test
    def testAliasesInObject01(): Unit \ Assert + IO =
        let anon = new TestGenericInterface {
            def testMethod(_this: TestGenericInterface, x: Object): Object =
                let str = unchecked_cast(x as String);
                checked_cast("${str}, ${str}")
        };
        assertTrue(TestGenericInterface.runTest(anon))

    @Test
    def testVarargsInterface01(): Unit \ Assert + IO =
        let anon = new TestVarargsInterface {
            def testMethod(_this: TestVarargsInterface, xs: Array[Int32, Static]): Int32 \ IO =
                xs |> Array.sum
        };
        assertTrue(TestVarargsInterface.runTest(anon))

    @Test
    def testThrowingInterface01(): Unit \ Assert + IO =
        let anon = new TestThrowingInterface {
            def testMethod(_this: TestThrowingInterface): Unit = ()
        };
        assertTrue(TestThrowingInterface.runTest(anon))

    @Test
    def testFunctionalInterface01(): Unit \ Assert + IO =
        let anon = new TestFunctionalInterface {
            def testMethod(_this: TestFunctionalInterface, x: Int32): Int32 = x + 1
        };
        assertTrue(TestFunctionalInterface.runTest(anon))

    @Test
    def testFunctionalInterface02(): Unit \ Assert + IO =
        let anon = new Comparator {
            def compare(_this: Comparator, _t: Object, _u: Object): Int32 = 0
        };
        assertEq(expected = 0, anon.compare("foo", "bar"))

    @Test
    def testClassWithDefaultConstructor01(): Unit \ Assert + IO =
        let anon = new TestClassWithDefaultConstructor {
            def abstractMethod(_this: TestClassWithDefaultConstructor, x: Int32): Int32 = x + 1
        };
        assertEq(expected = 2, anon.abstractMethod(1));
        assertEq(expected = "foobar", anon.concreteMethod("bar"))

    @Test
    def testClassWithDefaultConstructor02(): Unit \ Assert + IO =
        let anon = new TestClassWithDefaultConstructor {
            def abstractMethod(_this: TestClassWithDefaultConstructor, x: Int32): Int32 = x + 1
            def concreteMethod(_this: TestClassWithDefaultConstructor, y: String): String = "flix: ${y}"
        };
        assertEq(expected = 2, anon.abstractMethod(1));
        assertEq(expected = "flix: bar", anon.concreteMethod("bar"))

    @Test
    def testMethodClosure01(): Unit \ Assert + IO =
        let x = Array#{1, 2, 3} @ Static;
        let y = Array#{4, 5, 6} @ Static;
        let anon = new Object {
            def toString(_this: Object): String \ IO = Array.toString(Array.append(Static, x, y))
        };
        assertEq(expected = "Array#{1, 2, 3, 4, 5, 6}", anon.toString())

    @CompileTest
    def testStaticNestedClass01(): NestedClass \ IO =
        new NestedClass {}

    @CompileTest
    def testClassWithProtectedConstructor01(): TestClassWithProtectedConstructor \ IO =
        new TestClassWithProtectedConstructor {}
}
