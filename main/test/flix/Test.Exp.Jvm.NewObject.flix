mod Test.Exp.Jvm.NewObject {

  import java.lang.Object
  import dev.flix.test.{TestBoolArrayInterface, TestBoolInterface, TestCharArrayInterface, TestCharInterface,
                    TestClass, TestClassWithDefaultConstructor, TestClassWithNonDefaultConstructor, TestDefaultMethods,
                    TestFloat32ArrayInterface, TestFloat32Interface, TestFloat64ArrayInterface, TestFloat64Interface,
                    TestFunctionalInterface, TestGenericInterface, TestGenericMethod, TestInt8ArrayInterface,
                    TestInt16ArrayInterface, TestInt16Interface, TestInt32ArrayInterface, TestInt32Interface,
                    TestInt64ArrayInterface, TestInt64Interface, TestNonPublicInterface, TestOverloadedMethods,
                    TestStackOffsets, TestThrowingInterface, TestVarargsInterface, TestVoidInterface}

  def implementSerializable(): ##java.io.Serializable \ IO =
    new ##java.io.Serializable { }

  def implementSerializableAgain(): ##java.io.Serializable \ IO =
    new ##java.io.Serializable { }

  @Test
  def testImplementInterface01(): Bool \ IO =
    import java.lang.Object.toString(): String \ {};
    let anon = checked_cast(implementSerializable());
    toString(anon) |> String.startsWith(prefix = "Anon")

  @Test
  def testImplementInterface02(): Bool \ IO =
    import java.lang.Object.hashCode(): Int32 \ {};
    let anon = checked_cast(implementSerializable());
    let anon2 = checked_cast(implementSerializable());
    hashCode(anon) != hashCode(anon2)

  @Test
  def testImplementInterface03(): Bool \ IO =
    import java.lang.Object.equals(##java.lang.Object): Bool \ {};
    let anon = checked_cast(implementSerializable());
    let anon2 = checked_cast(implementSerializable());
    not equals(anon, anon2)

  @Test
  def testImplementInterface04(): Bool \ IO =
    import java.lang.Object.getClass(): ##java.lang.Class \ {};
    import java.lang.Object.equals(##java.lang.Object): Bool \ {};
    let anon = checked_cast(implementSerializable());
    let anon2 = checked_cast(implementSerializable());
    equals(checked_cast(getClass(anon)), checked_cast(getClass(anon2)))

  @Test
  def testImplementInterface05(): Bool \ IO =
    import java.lang.Object.getClass(): ##java.lang.Class \ {};
    import java.lang.Object.equals(##java.lang.Object): Bool \ {};
    let anon = checked_cast(implementSerializable());
    let anon2 = checked_cast(implementSerializableAgain());
    not equals(checked_cast(getClass(anon)), checked_cast(getClass(anon2)))

  @Test
  def testVoidInterface01(): Bool \ IO =
    import static dev.flix.test.TestVoidInterface.runTest(TestVoidInterface): Bool;
    let anon = new TestVoidInterface {
      def testMethod(_this: TestVoidInterface): Unit = ()
    };
    runTest(anon)

  @Test
  def testBoolInterface01(): Bool \ IO =
    import static dev.flix.test.TestBoolInterface.runTest(TestBoolInterface): Bool;
    let anon = new TestBoolInterface {
      def testMethod(_this: TestBoolInterface, x: Bool): Bool = not x
    };
    runTest(anon)

  @Test
  def testCharInterface01(): Bool \ IO =
    import static dev.flix.test.TestCharInterface.runTest(TestCharInterface): Bool;
    let anon = new TestCharInterface {
      def testMethod(_this: TestCharInterface, x: Char): Char = Char.toUpperCase(x)
    };
    runTest(anon)

  @Test
  def testInt16Interface01(): Bool \ IO =
    import static dev.flix.test.TestInt16Interface.runTest(TestInt16Interface): Bool;
    let anon = new TestInt16Interface {
      def testMethod(_this: TestInt16Interface, x: Int16): Int16 = x + 1i16
    };
    runTest(anon)

  @Test
  def testInt32Interface01(): Bool \ IO =
    import static dev.flix.test.TestInt32Interface.runTest(TestInt32Interface): Bool;
    let anon = new TestInt32Interface {
      def testMethod(_this: TestInt32Interface, x: Int32): Int32 = x + 1
    };
    runTest(anon)

  @Test
  def testInt64Interface01(): Bool \ IO =
    import static dev.flix.test.TestInt64Interface.runTest(TestInt64Interface): Bool;
    let anon = new TestInt64Interface {
      def testMethod(_this: TestInt64Interface, x: Int64): Int64 = x + 1i64
    };
    runTest(anon)

  @Test
  def testFloat32Interface01(): Bool \ IO =
    import static dev.flix.test.TestFloat32Interface.runTest(TestFloat32Interface): Bool;
    let anon = new TestFloat32Interface {
      def testMethod(_this: TestFloat32Interface, x: Float32): Float32 = x + 0.23f32
    };
    runTest(anon)

  @Test
  def testFloat64Interface01(): Bool \ IO =
    import static dev.flix.test.TestFloat64Interface.runTest(TestFloat64Interface): Bool;
    let anon = new TestFloat64Interface {
      def testMethod(_this: TestFloat64Interface, x: Float64): Float64 = x + 0.23f64
    };
    runTest(anon)

  @Test
  def testBoolArrayInterface01(): Bool \ IO =
    import static dev.flix.test.TestBoolArrayInterface.runTest(TestBoolArrayInterface): Bool;
    let anon = new TestBoolArrayInterface {
      def testMethod(_this: TestBoolArrayInterface, xs: Array[Bool, Static]): Array[Bool, Static] =
        xs |> Array.map(Static, x -> not x)
    };
    runTest(anon)

  @Test
  def testCharArrayInterface01(): Bool \ IO =
    import static dev.flix.test.TestCharArrayInterface.runTest(TestCharArrayInterface): Bool;
    let anon = new TestCharArrayInterface {
      def testMethod(_this: TestCharArrayInterface, xs: Array[Char, Static]): Array[Char, Static] =
        xs |> Array.map(Static, Char.toUpperCase)
    };
    runTest(anon)

  @Test
  def testInt8ArrayInterface01(): Bool \ IO =
    import static dev.flix.test.TestInt8ArrayInterface.runTest(TestInt8ArrayInterface): Bool;
    let anon = new TestInt8ArrayInterface {
      def testMethod(_this: TestInt8ArrayInterface, xs: Array[Int8, Static]): Array[Int8, Static] =
        xs |> Array.map(Static, x -> x + 1i8)
    };
    runTest(anon)

  @Test
  def testInt16ArrayInterface01(): Bool \ IO =
    import static dev.flix.test.TestInt16ArrayInterface.runTest(TestInt16ArrayInterface): Bool;
    let anon = new TestInt16ArrayInterface {
      def testMethod(_this: TestInt16ArrayInterface, xs: Array[Int16, Static]): Array[Int16, Static] =
        xs |> Array.map(Static, x -> x + 1i16)
    };
    runTest(anon)

  @Test
  def testInt32ArrayInterface01(): Bool \ IO =
    import static dev.flix.test.TestInt32ArrayInterface.runTest(TestInt32ArrayInterface): Bool;
    let anon = new TestInt32ArrayInterface {
      def testMethod(_this: TestInt32ArrayInterface, xs: Array[Int32, Static]): Array[Int32, Static] =
        xs |> Array.map(Static, x -> x + 1)
    };
    runTest(anon)

  @Test
  def testInt64ArrayInterface01(): Bool \ IO =
    import static dev.flix.test.TestInt64ArrayInterface.runTest(TestInt64ArrayInterface): Bool;
    let anon = new TestInt64ArrayInterface {
      def testMethod(_this: TestInt64ArrayInterface, xs: Array[Int64, Static]): Array[Int64, Static] =
        xs |> Array.map(Static, x -> x + 1i64)
    };
    runTest(anon)

  @Test
  def testFloat32ArrayInterface01(): Bool \ IO =
    import static dev.flix.test.TestFloat32ArrayInterface.runTest(TestFloat32ArrayInterface): Bool;
    let anon = new TestFloat32ArrayInterface {
      def testMethod(_this: TestFloat32ArrayInterface, xs: Array[Float32, Static]): Array[Float32, Static] =
        xs |> Array.map(Static, x -> x + 0.5f32)
    };
    runTest(anon)

  @Test
  def testFloat64ArrayInterface01(): Bool \ IO =
    import static dev.flix.test.TestFloat64ArrayInterface.runTest(TestFloat64ArrayInterface): Bool;
    let anon = new TestFloat64ArrayInterface {
      def testMethod(_this: TestFloat64ArrayInterface, xs: Array[Float64, Static]): Array[Float64, Static] =
        xs |> Array.map(Static, x -> x + 0.5f64)
    };
    runTest(anon)

  @Test
  def testStackOffsets01(): Bool \ IO =
    import static dev.flix.test.TestStackOffsets.runTest(TestStackOffsets): Bool;
    let anon = new TestStackOffsets {
      def testMethod(_this: TestStackOffsets, a: Bool, b: Char, c: Int8, d: Int16, e: Int32, f: Int64, g: Float32, h: Float64): String =
        "${a}, ${b}, ${c}, ${d}, ${e}, ${f}, ${g}, ${h}"
    };
    runTest(anon)

  @Test
  def testOverloadedMethods01(): Bool \ IO =
    import static dev.flix.test.TestOverloadedMethods.runTest(TestOverloadedMethods): Bool;
    let anon = new TestOverloadedMethods {
      def overloadedMethod(_this: TestOverloadedMethods): Int32 = 42
      def overloadedMethod(_this: TestOverloadedMethods, x: Int32): Int32 = x + 1
      def overloadedMethod(_this: TestOverloadedMethods, x: String, y: Float64, z: Float64): String = "${x}${y / z}"
    };
    runTest(anon)

  @Test
  def testDefaultMethods01(): Bool \ IO =
    import dev.flix.test.TestDefaultMethods.methodWithNoImplementation(Int32): Int32;
    import dev.flix.test.TestDefaultMethods.methodWithDefaultImplementation(Int32): Int32;
    let anon = new TestDefaultMethods {
      def methodWithNoImplementation(_this: TestDefaultMethods, x: Int32): Int32 = x + 1
    };
    methodWithNoImplementation(anon, 1) == 2 and methodWithDefaultImplementation(anon, 1) == 43

  @Test
  def testDefaultMethods02(): Bool \ IO =
    import dev.flix.test.TestDefaultMethods.methodWithNoImplementation(Int32): Int32;
    import dev.flix.test.TestDefaultMethods.methodWithDefaultImplementation(Int32): Int32;
    let anon = new TestDefaultMethods {
      def methodWithNoImplementation(_this: TestDefaultMethods, x: Int32): Int32 = x + 1
      def methodWithDefaultImplementation(_this: TestDefaultMethods, x: Int32): Int32 = x + 2
    };
    methodWithNoImplementation(anon, 1) == 2 and methodWithDefaultImplementation(anon, 1) == 3

  @Test
  def testGenericInterface01(): Bool \ IO =
    import static dev.flix.test.TestGenericInterface.runTest(TestGenericInterface): Bool;
    let anon = new TestGenericInterface {
      def testMethod(_this: TestGenericInterface, x: ##java.lang.Object): ##java.lang.Object =
        let str = unchecked_cast(x as String);
        checked_cast("${str}, ${str}")
    };
    runTest(anon)

  @Test
  def testGenericMethod01(): Bool \ IO =
    import static dev.flix.test.TestGenericMethod.runTest(TestGenericMethod): Bool;
    let anon = new TestGenericMethod {
      def testMethod(_this: TestGenericMethod, x: ##java.lang.Object): ##java.lang.Object =
        let str = unchecked_cast(x as String);
        checked_cast("${str}, ${str}")
    };
    runTest(anon)

  @Test
  def testAliasesInObject01(): Bool \ IO =
    import static dev.flix.test.TestGenericInterface.runTest(TestGenericInterface): Bool;
    let anon = new TestGenericInterface {
      def testMethod(_this: TestGenericInterface, x: Object): Object =
        let str = unchecked_cast(x as String);
        checked_cast("${str}, ${str}")
    };
    runTest(anon)

  @Test
  def testVarargsInterface01(): Bool \ IO =
    import static dev.flix.test.TestVarargsInterface.runTest(TestVarargsInterface): Bool;
    let anon = new TestVarargsInterface {
      def testMethod(_this: TestVarargsInterface, xs: Array[Int32, Static]): Int32 =
        xs |> Array.sum
    };
    runTest(anon)

  @Test
  def testThrowingInterface01(): Bool \ IO =
    import static dev.flix.test.TestThrowingInterface.runTest(TestThrowingInterface): Bool;
    let anon = new TestThrowingInterface {
      def testMethod(_this: TestThrowingInterface): Unit = ()
    };
    runTest(anon)

  @Test
  def testFunctionalInterface01(): Bool \ IO =
    import static dev.flix.test.TestFunctionalInterface.runTest(TestFunctionalInterface): Bool;
    let anon = new TestFunctionalInterface {
      def testMethod(_this: TestFunctionalInterface, x: Int32): Int32 = x + 1
    };
    runTest(anon)

  @Test
  def testFunctionalInterface02(): Bool \ IO =
    import java.util.Comparator.compare(##java.lang.Object, ##java.lang.Object): Int32;
    let anon = new ##java.util.Comparator {
      def compare(_this: ##java.util.Comparator, _t: ##java.lang.Object, _u: ##java.lang.Object): Int32 = 0
    };
    compare(anon, checked_cast("foo"), checked_cast("bar")) == 0

  @Test
  def testClassWithDefaultConstructor01(): Bool \ IO =
    import dev.flix.test.TestClassWithDefaultConstructor.abstractMethod(Int32): Int32;
    import dev.flix.test.TestClassWithDefaultConstructor.concreteMethod(String): String;
    let anon = new TestClassWithDefaultConstructor {
      def abstractMethod(_this: TestClassWithDefaultConstructor, x: Int32): Int32 = x + 1
    };
    abstractMethod(anon, 1) == 2 and
      concreteMethod(anon, "bar") == "foobar"

  @Test
  def testClassWithDefaultConstructor02(): Bool \ IO =
    import dev.flix.test.TestClassWithDefaultConstructor.abstractMethod(Int32): Int32;
    import dev.flix.test.TestClassWithDefaultConstructor.concreteMethod(String): String;
    let anon = new TestClassWithDefaultConstructor {
      def abstractMethod(_this: TestClassWithDefaultConstructor, x: Int32): Int32 = x + 1
      def concreteMethod(_this: TestClassWithDefaultConstructor, y: String): String = "flix: ${y}"
    };
    abstractMethod(anon, 1) == 2 and
      concreteMethod(anon, "bar") == "flix: bar"

  @Test
  def testMethodClosure01(): Bool \ IO =
    import java.lang.Object.toString(): String \ {};
    let x = Array#{1, 2, 3} @ Static;
    let y = Array#{4, 5, 6} @ Static;
    let anon = new ##java.lang.Object {
      def toString(_this: ##java.lang.Object): String = Array.toString(Array.append(Static, x, y))
    };
    toString(anon) == "Array#{1, 2, 3, 4, 5, 6}"

  @Test
  def testStaticNestedClass01(): ##dev.flix.test.TestClassWithStaticNestedClass$NestedClass \ IO =
    new ##dev.flix.test.TestClassWithStaticNestedClass$NestedClass {}

  @Test
  def testClassWithProtectedConstructor01(): ##dev.flix.test.TestClassWithProtectedConstructor \ IO =
    new ##dev.flix.test.TestClassWithProtectedConstructor {}
}
