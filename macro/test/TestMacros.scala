import org.scalatest.FunSuite

import impl.logic.Value
import Macros._

class TestMacros extends FunSuite {

  class Foo(val n: Int) {
    def canEqual(other: Any): Boolean = other.isInstanceOf[Foo]

    override def equals(other: Any): Boolean = other match {
      case that: Foo => (that canEqual this) && n == that.n
      case _ => false
    }

    override def hashCode(): Int = {
      val state = Seq(n)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }
  }

  test("void => ()") {
    def f() = ()
    val r1 = Value.Unit
    val r2 = valueWrapperFunc(f _)(Value.Unit)
    assertResult(r1)(r2)
  }

  test("() => ()") {
    def f(u: Unit) = u
    val r1 = Value.Unit
    val r2 = valueWrapperFunc(f _)(Value.Unit)
    assertResult(r1)(r2)
  }

  test("Boolean => Boolean") {
    def f(b: Boolean) = !b
    val r1 = Value.Bool(false)
    val r2 = valueWrapperFunc(f _)(Value.Bool(true))
    assertResult(r1)(r2)
  }

  test("Int => Int") {
    def f(n: Int) = n + 40
    val r1 = Value.Int(42)
    val r2 = valueWrapperFunc(f _)(Value.Int(2))
    assertResult(r1)(r2)
  }

  test("String => String") {
    def f(s: String) = s + "World"
    val r1 = Value.Str("HelloWorld")
    val r2 = valueWrapperFunc(f _)(Value.Str("Hello"))
    assertResult(r1)(r2)
  }

  test("String => Set[Int]") {
    def f(s: String) = Set(s.length, 1, 2, 3)
    val r1 = Value.Set(Set(5, 1, 2, 3).map(Value.Int))
    val r2 = valueWrapperFunc(f _)(Value.Str("12345"))
    assertResult(r1)(r2)
  }

  test("Set[Int] => String") {
    def f(s: Set[Int]) = s.mkString
    val r1 = Value.Str(Set(1, 2, 3, 4, 5).mkString)
    val r2 = valueWrapperFunc(f _)(Value.Set(Set(1, 2, 3, 4, 5).map(Value.Int)))
    assertResult(r1)(r2)
  }

  test("() => empty Set") {
    def f() = Set()
    val r1 = Value.Set(Set())
    val r2 = valueWrapperFunc(f _)(Value.Unit)
    assertResult(r1)(r2)
  }

  test("empty Set => ()") {
    def f(s: Set[Int]) = ()
    val r1 = Value.Unit
    val r2 = valueWrapperFunc(f _)(Value.Set(Set()))
    assertResult(r1)(r2)
  }

  //TODO(mhyee): Abs, Tag

  test("(Tuple2[String, String], Int) => Tuple2[Int, Int]") {
    def f(t: (String, String), n: Int) =
      (t._1.length + n, t._2.length + n)
    val r1 = Value.Tuple2(Value.Int(11), Value.Int(12))
    val r2 = valueWrapperFunc(f _)(Value.Tuple2(
      Value.Tuple2(Value.Str("a"), Value.Str("bc")),
      Value.Int(10)))
    assertResult(r1)(r2)
  }

  test("(Tuple3[String, String, String], Int) => Tuple3[Int, Int, Int]") {
    def f(t: (String, String, String), n: Int) =
      (t._1.length + n, t._2.length + n, t._3.length + n)
    val r1 = Value.Tuple3(Value.Int(11), Value.Int(12), Value.Int(13))
    val r2 = valueWrapperFunc(f _)(Value.Tuple2(
      Value.Tuple3(Value.Str("a"), Value.Str("bc"), Value.Str("def")),
      Value.Int(10)))
    assertResult(r1)(r2)
  }

  test("(Tuple4[String, String, String, String], Int) => Tuple4[Int, Int, Int, Int]") {
    def f(t: (String, String, String, String), n: Int) =
      (t._1.length + n, t._2.length + n, t._3.length + n, t._4.length + n)
    val r1 = Value.Tuple4(Value.Int(11), Value.Int(12), Value.Int(13), Value.Int(14))
    val r2 = valueWrapperFunc(f _)(Value.Tuple2(
      Value.Tuple4(Value.Str("a"), Value.Str("bc"), Value.Str("def"), Value.Str("ghij")),
      Value.Int(10)))
    assertResult(r1)(r2)
  }

  test("(Tuple5[String, String, String, String, String], Int) => Tuple5[Int, Int, Int, Int, Int]") {
    def f(t: (String, String, String, String, String), n: Int) =
      (t._1.length + n, t._2.length + n, t._3.length + n, t._4.length + n, t._5.length + n)
    val r1 = Value.Tuple5(Value.Int(11), Value.Int(12), Value.Int(13), Value.Int(14), Value.Int(15))
    val r2 = valueWrapperFunc(f _)(Value.Tuple2(
      Value.Tuple5(Value.Str("a"), Value.Str("bc"), Value.Str("def"), Value.Str("ghij"), Value.Str("klmno")),
      Value.Int(10)))
    assertResult(r1)(r2)
  }

  test("Foo => Foo") {
    def f(foo: Foo) = new Foo(foo.n * 2)
    val r1 = Value.Native(new Foo(42))
    val r2 = valueWrapperFunc(f _)(Value.Native(new Foo(21)))
    assertResult(r1)(r2)
  }

  test("(Foo, Int) => Foo") {
    def f(foo: Foo, n: Int) = new Foo(foo.n + n)
    val r1 = Value.Native(new Foo(42))
    val r2 = valueWrapperFunc(f _)(Value.Tuple2(Value.Native(new Foo(40)), Value.Int(2)))
    assertResult(r1)(r2)
  }

  test("(Tuple3[Int, Int, Int], Tuple3[Int, Int, Int]) => Tuple3[Int, Int, Int]") {
    def f(s: (Int, Int, Int), t: (Int, Int, Int)) = (s._1 + t._1, s._2 * t._2, s._3 - t._3)
    val r1 = Value.Tuple3(Value.Int(6), Value.Int(10), Value.Int(-1))
    val r2 = valueWrapperFunc(f _)(Value.Tuple2(
      Value.Tuple3(Value.Int(1), Value.Int(2), Value.Int(3)),
      Value.Tuple3(Value.Int(5), Value.Int(5), Value.Int(4))))
    assertResult(r1)(r2)
  }

  test("(Set[Int], Int) => Set[Int]") {
    def f(s: Set[Int], n: Int) = s.map(_ + n)
    val r1 = Value.Set(Set(10, 11, 12, 13).map(Value.Int))
    val r2 = valueWrapperFunc(f _)(Value.Tuple2(
      Value.Set(Set(0, 1, 2, 3).map(Value.Int)),
      Value.Int(10)))
    assertResult(r1)(r2)
  }

  test("String => (Set[String], Boolean)") {
    def f(s: String) = (Set(s, s * 2, s * 3), true)
    val r1 = Value.Tuple2(Value.Set(Set("a", "aa", "aaa").map(Value.Str)), Value.Bool(true))
    val r2 = valueWrapperFunc(f _)(Value.Str("a"))
    assertResult(r1)(r2)
  }

  test("Tuple2[Set[Int], Set[Int]] => Int") {
    def f(t: (Set[Int], Set[Int])) = t._1.sum * t._2.sum
    val r1 = Value.Int(90)
    val r2 = valueWrapperFunc(f _)(Value.Tuple2(
      Value.Set(Set(1, 2, 3, 4, 5).map(Value.Int)),
      Value.Set(Set(1, 2, 3).map(Value.Int))))
    assertResult(r1)(r2)
  }

  test("() => Tuple2[Set[Int], Set[Int]]") {
    def f() = (Set(1, 2, 3), Set(10, 11, 12))
    val r1 = Value.Tuple2(
      Value.Set(Set(1, 2, 3).map(Value.Int)),
      Value.Set(Set(10, 11, 12).map(Value.Int)))
    val r2 = valueWrapperFunc(f _)(Value.Unit)
    assertResult(r1)(r2)
  }

  test("Set[Tuple2[Int, Int]] => Set[Int]") {
    def f(s: Set[(Int, Int)]) = s.map(t => t._1 + t._2)
    val r1 = Value.Set(Set(17, 21, 4, 7).map(Value.Int))
    val r2 = valueWrapperFunc(f _)(Value.Set(
      Set((10, 7), (20, 1), (2, 2), (3, 4)).map(t => Value.Tuple2(Value.Int(t._1), Value.Int(t._2)))))
    assertResult(r1)(r2)
  }

  test("() => Set[Tuple2[Int, Int]]") {
    def f() = Set((10, 7), (20, 1), (2, 2), (3, 4))
    val r1 = Value.Set(
      Set((10, 7), (20, 1), (2, 2), (3, 4)).map(t => Value.Tuple2(Value.Int(t._1), Value.Int(t._2))))
    val r2 = valueWrapperFunc(f _)(Value.Unit)
    assertResult(r1)(r2)
  }

  test("Set[Set[String]] => Set[Int]") {
    def f(sss: Set[Set[String]]) = sss.map(ss => ss.map(s => s.length).sum)
    val r1 = Value.Set(Set(8, 6, 19).map(Value.Int))
    val r2 = valueWrapperFunc(f _)(Value.Set(Set(
      Value.Set(Set("a", "bcd", "asdf").map(Value.Str)),
      Value.Set(Set("123456").map(Value.Str)),
      Value.Set(Set("qwertyuiop", "asdfasdf", "a").map(Value.Str)))))
    assertResult(r1)(r2)
  }

  test("() => Set[Set[String]]") {
    def f() = Set(Set("a", "bcd", "asdf"), Set("123456"), Set("qwertyuiop", "asdfasdf", "a"))
    def r1 = Value.Set(Set(
      Value.Set(Set("a", "bcd", "asdf").map(Value.Str)),
      Value.Set(Set("123456").map(Value.Str)),
      Value.Set(Set("qwertyuiop", "asdfasdf", "a").map(Value.Str))))
    def r2 = valueWrapperFunc(f _)(Value.Unit)
    assertResult(r1)(r2)
  }
}
