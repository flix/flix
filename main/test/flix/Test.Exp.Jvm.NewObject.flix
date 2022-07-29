import java.io.Serializable;
import java.lang.Class;
import java.lang.Object;

import flix.test.TestBoolArrayInterface;
import flix.test.TestBoolInterface;
import flix.test.TestCharArrayInterface;
import flix.test.TestCharInterface;
import flix.test.TestClass;
import flix.test.TestClassWithDefaultConstructor;
import flix.test.TestDefaultMethods;
import flix.test.TestFloat32ArrayInterface;
import flix.test.TestFloat32Interface;
import flix.test.TestFloat64ArrayInterface;
import flix.test.TestFloat64Interface;
import flix.test.TestFunctionalInterface;
import flix.test.TestGenericInterface;
import flix.test.TestGenericMethod;
import flix.test.TestInt8ArrayInterface;
import flix.test.TestInt16ArrayInterface;
import flix.test.TestInt16Interface;
import flix.test.TestInt32ArrayInterface;
import flix.test.TestInt32Interface;
import flix.test.TestInt64ArrayInterface;
import flix.test.TestInt64Interface;
import flix.test.TestOverloadedMethods;
import flix.test.TestStackOffsets;
import flix.test.TestThrowingInterface;
import flix.test.TestVarargsInterface;
import flix.test.TestVoidInterface;

namespace Test/Exp/Jvm/NewObject {

  def implementSerializable(): Serializable & Impure =
    object Serializable { }

  def implementSerializableAgain(): Serializable & Impure =
    object Serializable { }

  @test
  def testImplementInterface01(): Bool & Impure =
    import java.lang.Object.toString(): String;
    let anon = implementSerializable() as Object;
    toString(anon) |> String.startsWith(prefix = "Anon")

  @test
  def testImplementInterface02(): Bool & Impure =
    import java.lang.Object.hashCode(): Int32 & Pure;
    let anon = implementSerializable() as Object;
    let anon2 = implementSerializable() as Object;
    hashCode(anon) != hashCode(anon2)

  @test
  def testImplementInterface03(): Bool & Impure =
    import java.lang.Object.equals(Object): Bool & Pure;
    let anon = implementSerializable() as Object;
    let anon2 = implementSerializable() as Object;
    not equals(anon, anon2)

  @test
  def testImplementInterface04(): Bool & Impure =
    import java.lang.Object.getClass(): Class & Pure;
    import java.lang.Object.equals(Object): Bool & Pure;
    let anon = implementSerializable() as Object;
    let anon2 = implementSerializable() as Object;
    equals(getClass(anon) as Object, getClass(anon2) as Object)

  @test
  def testImplementInterface05(): Bool & Impure =
    import java.lang.Object.getClass(): Class & Pure;
    import java.lang.Object.equals(Object): Bool & Pure;
    let anon = implementSerializable() as Object;
    let anon2 = implementSerializableAgain() as Object;
    not equals(getClass(anon) as Object, getClass(anon2) as Object)

  @test
  def testVoidInterface01(): Bool & Impure =
    import static flix.test.TestVoidInterface.runTest(TestVoidInterface): Bool;
    let anon = object TestVoidInterface {
      def testMethod(_this: TestVoidInterface): Unit = ()
    };
    runTest(anon)

  @test
  def testBoolInterface01(): Bool & Impure =
    import static flix.test.TestBoolInterface.runTest(TestBoolInterface): Bool;
    let anon = object TestBoolInterface {
      def testMethod(_this: TestBoolInterface, x: Bool): Bool = not x
    };
    runTest(anon)

  @test
  def testCharInterface01(): Bool & Impure =
    import static flix.test.TestCharInterface.runTest(TestCharInterface): Bool;
    let anon = object TestCharInterface {
      def testMethod(_this: TestCharInterface, x: Char): Char = Char.toUpperCase(x)
    };
    runTest(anon)

  @test
  def testInt16Interface01(): Bool & Impure =
    import static flix.test.TestInt16Interface.runTest(TestInt16Interface): Bool;
    let anon = object TestInt16Interface {
      def testMethod(_this: TestInt16Interface, x: Int16): Int16 = x + 1i16
    };
    runTest(anon)

  @test
  def testInt32Interface01(): Bool & Impure =
    import static flix.test.TestInt32Interface.runTest(TestInt32Interface): Bool;
    let anon = object TestInt32Interface {
      def testMethod(_this: TestInt32Interface, x: Int32): Int32 = x + 1
    };
    runTest(anon)

  @test
  def testInt64Interface01(): Bool & Impure =
    import static flix.test.TestInt64Interface.runTest(TestInt64Interface): Bool;
    let anon = object TestInt64Interface {
      def testMethod(_this: TestInt64Interface, x: Int64): Int64 = x + 1i64
    };
    runTest(anon)

  @test
  def testFloat32Interface01(): Bool & Impure =
    import static flix.test.TestFloat32Interface.runTest(TestFloat32Interface): Bool;
    let anon = object TestFloat32Interface {
      def testMethod(_this: TestFloat32Interface, x: Float32): Float32 = x + 0.23f32
    };
    runTest(anon)

  @test
  def testFloat64Interface01(): Bool & Impure =
    import static flix.test.TestFloat64Interface.runTest(TestFloat64Interface): Bool;
    let anon = object TestFloat64Interface {
      def testMethod(_this: TestFloat64Interface, x: Float64): Float64 = x + 0.23f64
    };
    runTest(anon)

  @test
  def testBoolArrayInterface01(): Bool & Impure =
    import static flix.test.TestBoolArrayInterface.runTest(TestBoolArrayInterface): Bool;
    let anon = object TestBoolArrayInterface {
      def testMethod(_this: TestBoolArrayInterface, xs: Array[Bool, Static]): Array[Bool, Static] = 
        xs |> Array.map(x -> not x)
    };
    runTest(anon)

  @test
  def testCharArrayInterface01(): Bool & Impure =
    import static flix.test.TestCharArrayInterface.runTest(TestCharArrayInterface): Bool;
    let anon = object TestCharArrayInterface {
      def testMethod(_this: TestCharArrayInterface, xs: Array[Char, Static]): Array[Char, Static] = 
        xs |> Array.map(Char.toUpperCase)
    };
    runTest(anon)

  @test
  def testInt8ArrayInterface01(): Bool & Impure =
    import static flix.test.TestInt8ArrayInterface.runTest(TestInt8ArrayInterface): Bool;
    let anon = object TestInt8ArrayInterface {
      def testMethod(_this: TestInt8ArrayInterface, xs: Array[Int8, Static]): Array[Int8, Static] = 
        xs |> Array.map(x -> x + 1i8)
    };
    runTest(anon)

  @test
  def testInt16ArrayInterface01(): Bool & Impure =
    import static flix.test.TestInt16ArrayInterface.runTest(TestInt16ArrayInterface): Bool;
    let anon = object TestInt16ArrayInterface {
      def testMethod(_this: TestInt16ArrayInterface, xs: Array[Int16, Static]): Array[Int16, Static] = 
        xs |> Array.map(x -> x + 1i16)
    };
    runTest(anon)

  @test
  def testInt32ArrayInterface01(): Bool & Impure =
    import static flix.test.TestInt32ArrayInterface.runTest(TestInt32ArrayInterface): Bool;
    let anon = object TestInt32ArrayInterface {
      def testMethod(_this: TestInt32ArrayInterface, xs: Array[Int32, Static]): Array[Int32, Static] = 
        xs |> Array.map(x -> x + 1)
    };
    runTest(anon)

  @test
  def testInt64ArrayInterface01(): Bool & Impure =
    import static flix.test.TestInt64ArrayInterface.runTest(TestInt64ArrayInterface): Bool;
    let anon = object TestInt64ArrayInterface {
      def testMethod(_this: TestInt64ArrayInterface, xs: Array[Int64, Static]): Array[Int64, Static] = 
        xs |> Array.map(x -> x + 1i64)
    };
    runTest(anon)

  @test
  def testFloat32ArrayInterface01(): Bool & Impure =
    import static flix.test.TestFloat32ArrayInterface.runTest(TestFloat32ArrayInterface): Bool;
    let anon = object TestFloat32ArrayInterface {
      def testMethod(_this: TestFloat32ArrayInterface, xs: Array[Float32, Static]): Array[Float32, Static] = 
        xs |> Array.map(x -> x + 0.5f32)
    };
    runTest(anon)

  @test
  def testFloat64ArrayInterface01(): Bool & Impure =
    import static flix.test.TestFloat64ArrayInterface.runTest(TestFloat64ArrayInterface): Bool;
    let anon = object TestFloat64ArrayInterface {
      def testMethod(_this: TestFloat64ArrayInterface, xs: Array[Float64, Static]): Array[Float64, Static] = 
        xs |> Array.map(x -> x + 0.5f64)
    };
    runTest(anon)

  @test 
  def testStackOffsets01(): Bool & Impure =
    import static flix.test.TestStackOffsets.runTest(TestStackOffsets): Bool;
    let anon = object TestStackOffsets {
      def testMethod(_this: TestStackOffsets, a: Bool, b: Char, c: Int8, d: Int16, e: Int32, f: Int64, g: Float32, h: Float64): String = 
        "${a}, ${b}, ${c}, ${d}, ${e}, ${f}, ${g}, ${h}"
    };
    runTest(anon)

  @test
  def testOverloadedMethods01(): Bool & Impure =
    import static flix.test.TestOverloadedMethods.runTest(TestOverloadedMethods): Bool;
    let anon = object TestOverloadedMethods {
      def overloadedMethod(_this: TestOverloadedMethods): Int32 = 42
      def overloadedMethod(_this: TestOverloadedMethods, x: Int32): Int32 = x + 1
      def overloadedMethod(_this: TestOverloadedMethods, x: String, y: Float64, z: Float64): String = "${x}${y / z}"
    };
    runTest(anon)

  @test
  def testDefaultMethods01(): Bool & Impure =
    import flix.test.TestDefaultMethods.methodWithNoImplementation(Int32): Int32;
    import flix.test.TestDefaultMethods.methodWithDefaultImplementation(Int32): Int32;
    let anon = object TestDefaultMethods {
      def methodWithNoImplementation(_this: TestDefaultMethods, x: Int32): Int32 = x + 1
    };
    methodWithNoImplementation(anon, 1) == 2 and methodWithDefaultImplementation(anon, 1) == 43

  @test
  def testDefaultMethods02(): Bool & Impure =
    import flix.test.TestDefaultMethods.methodWithNoImplementation(Int32): Int32;
    import flix.test.TestDefaultMethods.methodWithDefaultImplementation(Int32): Int32;
    let anon = object TestDefaultMethods {
      def methodWithNoImplementation(_this: TestDefaultMethods, x: Int32): Int32 = x + 1
      def methodWithDefaultImplementation(_this: TestDefaultMethods, x: Int32): Int32 = x + 2
    };
    methodWithNoImplementation(anon, 1) == 2 and methodWithDefaultImplementation(anon, 1) == 3

  @test
  def testGenericInterface01(): Bool & Impure =
    import static flix.test.TestGenericInterface.runTest(TestGenericInterface): Bool;
    let anon = object TestGenericInterface {
      def testMethod(_this: TestGenericInterface, x: Object): Object = 
        let str = x as String;
        "${str}, ${str}" as Object
    };
    runTest(anon)

  @test
  def testGenericMethod01(): Bool & Impure =
    import static flix.test.TestGenericMethod.runTest(TestGenericMethod): Bool;
    let anon = object TestGenericMethod {
      def testMethod(_this: TestGenericMethod, x: Object): Object = 
        let str = x as String;
        "${str}, ${str}" as Object
    };
    runTest(anon)

  @test
  def testAliasesInObject01(): Bool & Impure =
    import static flix.test.TestGenericInterface.runTest(TestGenericInterface): Bool;
    let anon = object TestGenericInterface {
      def testMethod(_this: TestGenericInterface, x: Object): Object = 
        let str = x as String;
        "${str}, ${str}" as Object
    };
    runTest(anon)

  @test
  def testVarargsInterface01(): Bool & Impure =
    import static flix.test.TestVarargsInterface.runTest(TestVarargsInterface): Bool;
    let anon = object TestVarargsInterface {
      def testMethod(_this: TestVarargsInterface, xs: Array[Int32, Static]): Int32 = 
        xs |> Array.sum
    };
    runTest(anon)

  @test
  def testThrowingInterface01(): Bool & Impure =
    import static flix.test.TestThrowingInterface.runTest(TestThrowingInterface): Bool;
    let anon = object TestThrowingInterface {
      def testMethod(_this: TestThrowingInterface): Unit = ()
    };
    runTest(anon)

  @test
  def testFunctionalInterface01(): Bool & Impure =
    import static flix.test.TestFunctionalInterface.runTest(TestFunctionalInterface): Bool;
    let anon = object TestFunctionalInterface {
      def testMethod(_this: TestFunctionalInterface, x: Int32): Int32 = x + 1
    };
    runTest(anon)

  @test
  def testClassWithDefaultConstructor01(): Bool & Impure =
    import flix.test.TestClassWithDefaultConstructor.abstractMethod(Int32): Int32;
    import flix.test.TestClassWithDefaultConstructor.concreteMethod(String): String;
    let anon = object TestClassWithDefaultConstructor {
      def abstractMethod(_this: TestClassWithDefaultConstructor, x: Int32): Int32 = x + 1
    };
    abstractMethod(anon, 1) == 2 and
      concreteMethod(anon, "bar") == "foobar"

  @test
  def testClassWithDefaultConstructor02(): Bool & Impure =
    import flix.test.TestClassWithDefaultConstructor.abstractMethod(Int32): Int32;
    import flix.test.TestClassWithDefaultConstructor.concreteMethod(String): String;
    let anon = object TestClassWithDefaultConstructor {
      def abstractMethod(_this: TestClassWithDefaultConstructor, x: Int32): Int32 = x + 1
      def concreteMethod(_this: TestClassWithDefaultConstructor, y: String): String = "flix: ${y}"
    };
    abstractMethod(anon, 1) == 2 and
      concreteMethod(anon, "bar") == "flix: bar"
}
