import java.lang.Object
import flix.test.{TestBoolArrayInterface, TestBoolInterface, TestCharArrayInterface, TestCharInterface, 
                  TestClass, TestClassWithDefaultConstructor, TestClassWithNonDefaultConstructor, TestDefaultMethods, 
                  TestFloat32ArrayInterface, TestFloat32Interface, TestFloat64ArrayInterface, TestFloat64Interface, 
                  TestFunctionalInterface, TestGenericInterface, TestGenericMethod, TestInt8ArrayInterface, 
                  TestInt16ArrayInterface, TestInt16Interface, TestInt32ArrayInterface, TestInt32Interface, 
                  TestInt64ArrayInterface, TestInt64Interface, TestNonPublicInterface, TestOverloadedMethods, 
                  TestStackOffsets, TestThrowingInterface, TestVarargsInterface, TestVoidInterface}

namespace Test/Exp/Jvm/NewObject {

  def implementSerializable(): ##java.io.Serializable \ IO =
    new ##java.io.Serializable { }

  def implementSerializableAgain(): ##java.io.Serializable \ IO =
    new ##java.io.Serializable { }

  @test
  def testImplementInterface01(): Bool \ IO =
    import java.lang.Object.toString(): String \ {};
    let anon = implementSerializable() as ##java.lang.Object;
    toString(anon) |> String.startsWith(prefix = "Anon")

  @test
  def testImplementInterface02(): Bool \ IO =
    import java.lang.Object.hashCode(): Int32 \ {};
    let anon = implementSerializable() as ##java.lang.Object;
    let anon2 = implementSerializable() as ##java.lang.Object;
    hashCode(anon) != hashCode(anon2)

  @test
  def testImplementInterface03(): Bool \ IO =
    import java.lang.Object.equals(##java.lang.Object): Bool \ {};
    let anon = implementSerializable() as ##java.lang.Object;
    let anon2 = implementSerializable() as ##java.lang.Object;
    not equals(anon, anon2)

  @test
  def testImplementInterface04(): Bool \ IO =
    import java.lang.Object.getClass(): ##java.lang.Class \ {};
    import java.lang.Object.equals(##java.lang.Object): Bool \ {};
    let anon = implementSerializable() as ##java.lang.Object;
    let anon2 = implementSerializable() as ##java.lang.Object;
    equals(getClass(anon) as ##java.lang.Object, getClass(anon2) as ##java.lang.Object)

  @test
  def testImplementInterface05(): Bool \ IO =
    import java.lang.Object.getClass(): ##java.lang.Class \ {};
    import java.lang.Object.equals(##java.lang.Object): Bool \ {};
    let anon = implementSerializable() as ##java.lang.Object;
    let anon2 = implementSerializableAgain() as ##java.lang.Object;
    not equals(getClass(anon) as ##java.lang.Object, getClass(anon2) as ##java.lang.Object)

  @test
  def testVoidInterface01(): Bool \ IO =
    import static flix.test.TestVoidInterface.runTest(TestVoidInterface): Bool;
    let anon = new TestVoidInterface {
      def testMethod(_this: TestVoidInterface): Unit = ()
    };
    runTest(anon)

  @test
  def testBoolInterface01(): Bool \ IO =
    import static flix.test.TestBoolInterface.runTest(TestBoolInterface): Bool;
    let anon = new TestBoolInterface {
      def testMethod(_this: TestBoolInterface, x: Bool): Bool = not x
    };
    runTest(anon)

  @test
  def testCharInterface01(): Bool \ IO =
    import static flix.test.TestCharInterface.runTest(TestCharInterface): Bool;
    let anon = new TestCharInterface {
      def testMethod(_this: TestCharInterface, x: Char): Char = Char.toUpperCase(x)
    };
    runTest(anon)

  @test
  def testInt16Interface01(): Bool \ IO =
    import static flix.test.TestInt16Interface.runTest(TestInt16Interface): Bool;
    let anon = new TestInt16Interface {
      def testMethod(_this: TestInt16Interface, x: Int16): Int16 = x + 1i16
    };
    runTest(anon)

  @test
  def testInt32Interface01(): Bool \ IO =
    import static flix.test.TestInt32Interface.runTest(TestInt32Interface): Bool;
    let anon = new TestInt32Interface {
      def testMethod(_this: TestInt32Interface, x: Int32): Int32 = x + 1
    };
    runTest(anon)

  @test
  def testInt64Interface01(): Bool \ IO =
    import static flix.test.TestInt64Interface.runTest(TestInt64Interface): Bool;
    let anon = new TestInt64Interface {
      def testMethod(_this: TestInt64Interface, x: Int64): Int64 = x + 1i64
    };
    runTest(anon)

  @test
  def testFloat32Interface01(): Bool \ IO =
    import static flix.test.TestFloat32Interface.runTest(TestFloat32Interface): Bool;
    let anon = new TestFloat32Interface {
      def testMethod(_this: TestFloat32Interface, x: Float32): Float32 = x + 0.23f32
    };
    runTest(anon)

  @test
  def testFloat64Interface01(): Bool \ IO =
    import static flix.test.TestFloat64Interface.runTest(TestFloat64Interface): Bool;
    let anon = new TestFloat64Interface {
      def testMethod(_this: TestFloat64Interface, x: Float64): Float64 = x + 0.23f64
    };
    runTest(anon)

  @test
  def testBoolArrayInterface01(): Bool \ IO =
    import static flix.test.TestBoolArrayInterface.runTest(TestBoolArrayInterface): Bool;
    let anon = new TestBoolArrayInterface {
      def testMethod(_this: TestBoolArrayInterface, xs: Array[Bool, Static]): Array[Bool, Static] =
        xs |> Array.map(x -> not x)
    };
    runTest(anon)

  @test
  def testCharArrayInterface01(): Bool \ IO =
    import static flix.test.TestCharArrayInterface.runTest(TestCharArrayInterface): Bool;
    let anon = new TestCharArrayInterface {
      def testMethod(_this: TestCharArrayInterface, xs: Array[Char, Static]): Array[Char, Static] =
        xs |> Array.map(Char.toUpperCase)
    };
    runTest(anon)

  @test
  def testInt8ArrayInterface01(): Bool \ IO =
    import static flix.test.TestInt8ArrayInterface.runTest(TestInt8ArrayInterface): Bool;
    let anon = new TestInt8ArrayInterface {
      def testMethod(_this: TestInt8ArrayInterface, xs: Array[Int8, Static]): Array[Int8, Static] =
        xs |> Array.map(x -> x + 1i8)
    };
    runTest(anon)

  @test
  def testInt16ArrayInterface01(): Bool \ IO =
    import static flix.test.TestInt16ArrayInterface.runTest(TestInt16ArrayInterface): Bool;
    let anon = new TestInt16ArrayInterface {
      def testMethod(_this: TestInt16ArrayInterface, xs: Array[Int16, Static]): Array[Int16, Static] =
        xs |> Array.map(x -> x + 1i16)
    };
    runTest(anon)

  @test
  def testInt32ArrayInterface01(): Bool \ IO =
    import static flix.test.TestInt32ArrayInterface.runTest(TestInt32ArrayInterface): Bool;
    let anon = new TestInt32ArrayInterface {
      def testMethod(_this: TestInt32ArrayInterface, xs: Array[Int32, Static]): Array[Int32, Static] =
        xs |> Array.map(x -> x + 1)
    };
    runTest(anon)

  @test
  def testInt64ArrayInterface01(): Bool \ IO =
    import static flix.test.TestInt64ArrayInterface.runTest(TestInt64ArrayInterface): Bool;
    let anon = new TestInt64ArrayInterface {
      def testMethod(_this: TestInt64ArrayInterface, xs: Array[Int64, Static]): Array[Int64, Static] =
        xs |> Array.map(x -> x + 1i64)
    };
    runTest(anon)

  @test
  def testFloat32ArrayInterface01(): Bool \ IO =
    import static flix.test.TestFloat32ArrayInterface.runTest(TestFloat32ArrayInterface): Bool;
    let anon = new TestFloat32ArrayInterface {
      def testMethod(_this: TestFloat32ArrayInterface, xs: Array[Float32, Static]): Array[Float32, Static] =
        xs |> Array.map(x -> x + 0.5f32)
    };
    runTest(anon)

  @test
  def testFloat64ArrayInterface01(): Bool \ IO =
    import static flix.test.TestFloat64ArrayInterface.runTest(TestFloat64ArrayInterface): Bool;
    let anon = new TestFloat64ArrayInterface {
      def testMethod(_this: TestFloat64ArrayInterface, xs: Array[Float64, Static]): Array[Float64, Static] =
        xs |> Array.map(x -> x + 0.5f64)
    };
    runTest(anon)

  @test
  def testStackOffsets01(): Bool \ IO =
    import static flix.test.TestStackOffsets.runTest(TestStackOffsets): Bool;
    let anon = new TestStackOffsets {
      def testMethod(_this: TestStackOffsets, a: Bool, b: Char, c: Int8, d: Int16, e: Int32, f: Int64, g: Float32, h: Float64): String =
        "${a}, ${b}, ${c}, ${d}, ${e}, ${f}, ${g}, ${h}"
    };
    runTest(anon)

  @test
  def testOverloadedMethods01(): Bool \ IO =
    import static flix.test.TestOverloadedMethods.runTest(TestOverloadedMethods): Bool;
    let anon = new TestOverloadedMethods {
      def overloadedMethod(_this: TestOverloadedMethods): Int32 = 42
      def overloadedMethod(_this: TestOverloadedMethods, x: Int32): Int32 = x + 1
      def overloadedMethod(_this: TestOverloadedMethods, x: String, y: Float64, z: Float64): String = "${x}${y / z}"
    };
    runTest(anon)

  @test
  def testDefaultMethods01(): Bool \ IO =
    import flix.test.TestDefaultMethods.methodWithNoImplementation(Int32): Int32;
    import flix.test.TestDefaultMethods.methodWithDefaultImplementation(Int32): Int32;
    let anon = new TestDefaultMethods {
      def methodWithNoImplementation(_this: TestDefaultMethods, x: Int32): Int32 = x + 1
    };
    methodWithNoImplementation(anon, 1) == 2 and methodWithDefaultImplementation(anon, 1) == 43

  @test
  def testDefaultMethods02(): Bool \ IO =
    import flix.test.TestDefaultMethods.methodWithNoImplementation(Int32): Int32;
    import flix.test.TestDefaultMethods.methodWithDefaultImplementation(Int32): Int32;
    let anon = new TestDefaultMethods {
      def methodWithNoImplementation(_this: TestDefaultMethods, x: Int32): Int32 = x + 1
      def methodWithDefaultImplementation(_this: TestDefaultMethods, x: Int32): Int32 = x + 2
    };
    methodWithNoImplementation(anon, 1) == 2 and methodWithDefaultImplementation(anon, 1) == 3

  @test
  def testGenericInterface01(): Bool \ IO =
    import static flix.test.TestGenericInterface.runTest(TestGenericInterface): Bool;
    let anon = new TestGenericInterface {
      def testMethod(_this: TestGenericInterface, x: ##java.lang.Object): ##java.lang.Object =
        let str = x as String;
        "${str}, ${str}" as ##java.lang.Object
    };
    runTest(anon)

  @test
  def testGenericMethod01(): Bool \ IO =
    import static flix.test.TestGenericMethod.runTest(TestGenericMethod): Bool;
    let anon = new TestGenericMethod {
      def testMethod(_this: TestGenericMethod, x: ##java.lang.Object): ##java.lang.Object =
        let str = x as String;
        "${str}, ${str}" as ##java.lang.Object
    };
    runTest(anon)

  @test
  def testAliasesInObject01(): Bool \ IO =
    import static flix.test.TestGenericInterface.runTest(TestGenericInterface): Bool;
    let anon = new TestGenericInterface {
      def testMethod(_this: TestGenericInterface, x: Object): Object =
        let str = x as String;
        "${str}, ${str}" as Object
    };
    runTest(anon)

  @test
  def testVarargsInterface01(): Bool \ IO =
    import static flix.test.TestVarargsInterface.runTest(TestVarargsInterface): Bool;
    let anon = new TestVarargsInterface {
      def testMethod(_this: TestVarargsInterface, xs: Array[Int32, Static]): Int32 =
        xs |> Array.sum
    };
    runTest(anon)

  @test
  def testThrowingInterface01(): Bool \ IO =
    import static flix.test.TestThrowingInterface.runTest(TestThrowingInterface): Bool;
    let anon = new TestThrowingInterface {
      def testMethod(_this: TestThrowingInterface): Unit = ()
    };
    runTest(anon)

  @test
  def testFunctionalInterface01(): Bool \ IO =
    import static flix.test.TestFunctionalInterface.runTest(TestFunctionalInterface): Bool;
    let anon = new TestFunctionalInterface {
      def testMethod(_this: TestFunctionalInterface, x: Int32): Int32 = x + 1
    };
    runTest(anon)

  @test
  def testClassWithDefaultConstructor01(): Bool \ IO =
    import flix.test.TestClassWithDefaultConstructor.abstractMethod(Int32): Int32;
    import flix.test.TestClassWithDefaultConstructor.concreteMethod(String): String;
    let anon = new TestClassWithDefaultConstructor {
      def abstractMethod(_this: TestClassWithDefaultConstructor, x: Int32): Int32 = x + 1
    };
    abstractMethod(anon, 1) == 2 and
      concreteMethod(anon, "bar") == "foobar"

  @test
  def testClassWithDefaultConstructor02(): Bool \ IO =
    import flix.test.TestClassWithDefaultConstructor.abstractMethod(Int32): Int32;
    import flix.test.TestClassWithDefaultConstructor.concreteMethod(String): String;
    let anon = new TestClassWithDefaultConstructor {
      def abstractMethod(_this: TestClassWithDefaultConstructor, x: Int32): Int32 = x + 1
      def concreteMethod(_this: TestClassWithDefaultConstructor, y: String): String = "flix: ${y}"
    };
    abstractMethod(anon, 1) == 2 and
      concreteMethod(anon, "bar") == "flix: bar"

  @test
  def testMethodClosure01(): Bool \ IO =
    import java.lang.Object.toString(): String \ {};
    let x = [1, 2, 3];
    let y = [4, 5, 6];
    let anon = new ##java.lang.Object {
      def toString(_this: ##java.lang.Object): String = Array.toString(Array.append(x, y))
    };
    toString(anon) == "[1, 2, 3, 4, 5, 6]"

  @test
  def testStaticNestedClass01(): ##flix.test.TestClassWithStaticNestedClass$NestedClass \ IO =
    new ##flix.test.TestClassWithStaticNestedClass$NestedClass {}

}
