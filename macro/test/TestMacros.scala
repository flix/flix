import flix.Macros._
import org.scalatest.FunSuite

import impl.logic.{Symbol, Type, Value}

object Definitions {
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

  // Represents the Flix type:
  // (def-type FooTag (variant ((FZero) (FOne Int) (FTwo Int Str))))
  sealed trait FooTag
  case object FZero extends FooTag
  case class FOne(n: Int) extends FooTag
  case class FTwo(m: Int, n: String) extends FooTag

  // Internal Flix representation of FooTag
  val fooTagTyp = Type.Sum(List(
    Type.Tag(Symbol.NamedSymbol("FOne"), Type.Int),
    Type.Tag(Symbol.NamedSymbol("FTwo"), Type.Tuple2(Type.Int, Type.Str)),
    Type.Tag(Symbol.NamedSymbol("FZero"), Type.Unit)))
}

class TestMacros extends FunSuite {
  import Definitions._

  test("void => ()") {
    def f() = ()
    val r1 = Value.Unit
    val r2 = valueWrapperFunc(f _)(Value.Unit)
    assertResult(r1)(r2)
  }

  test("() => ()") {
    def f(u: Unit) = ()
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

  test("Tag: () => FZero") {
    def f(): FZero.type = FZero
    val r1 = Value.Tag(Symbol.NamedSymbol("FZero"), Value.Unit, fooTagTyp)
    val r2 = valueWrapperFunc(f _)(Value.Unit)
    assertResult(r1)(r2)
  }

  test("Tag: Int => FOne") {
    def f(n: Int): FOne = FOne(n)
    val r1 = Value.Tag(Symbol.NamedSymbol("FOne"), Value.Int(42), fooTagTyp)
    val r2 = valueWrapperFunc(f _)(Value.Int(42))
    assertResult(r1)(r2)
  }

  test("Tag: (Int, String) => FTwo") {
    def f(n: Int, s: String): FTwo = FTwo(n, s)
    val r1 = Value.Tag(Symbol.NamedSymbol("FTwo"), Value.Tuple2(Value.Int(5), Value.Str("hi")), fooTagTyp)
    val r2 = valueWrapperFunc(f _)(Value.Tuple2(Value.Int(5), Value.Str("hi")))
    assertResult(r1)(r2)
  }

  test("Tag (ascribed): (Int, String) => FooTag") {
    def f(n: Int, s: String): FooTag = n match {
      case 0 => FZero
      case 1 => FOne(s.length)
      case 2 => FTwo(n, s)
    }

    val r01 = Value.Tag(Symbol.NamedSymbol("FZero"), Value.Unit, fooTagTyp)
    val r02 = valueWrapperFunc(f _)(Value.Tuple2(Value.Int(0), Value.Str("abc")))
    assertResult(r01)(r02)

    val r11 = Value.Tag(Symbol.NamedSymbol("FOne"), Value.Int(3), fooTagTyp)
    val r12 = valueWrapperFunc(f _)(Value.Tuple2(Value.Int(1), Value.Str("abc")))
    assertResult(r11)(r12)

    val r21 = Value.Tag(Symbol.NamedSymbol("FTwo"), Value.Tuple2(Value.Int(2), Value.Str("abc")), fooTagTyp)
    val r22 = valueWrapperFunc(f _)(Value.Tuple2(Value.Int(2), Value.Str("abc")))
    assertResult(r21)(r22)
  }

  test("Tag (inferred): (Int, String) => FooTag") {
    def f(n: Int, s: String) = n match {
      case 0 => FZero
      case 1 => FOne(s.length)
      case 2 => FTwo(n, s)
    }

    val r01 = Value.Tag(Symbol.NamedSymbol("FZero"), Value.Unit, fooTagTyp)
    val r02 = valueWrapperFunc(f _)(Value.Tuple2(Value.Int(0), Value.Str("abc")))
    assertResult(r01)(r02)

    val r11 = Value.Tag(Symbol.NamedSymbol("FOne"), Value.Int(3), fooTagTyp)
    val r12 = valueWrapperFunc(f _)(Value.Tuple2(Value.Int(1), Value.Str("abc")))
    assertResult(r11)(r12)

    val r21 = Value.Tag(Symbol.NamedSymbol("FTwo"), Value.Tuple2(Value.Int(2), Value.Str("abc")), fooTagTyp)
    val r22 = valueWrapperFunc(f _)(Value.Tuple2(Value.Int(2), Value.Str("abc")))
    assertResult(r21)(r22)
  }

  test("Tag: (Int, String) => (FOne, FTwo)") {
    def f(n: Int, s: String) = (FOne(n + s.length), FTwo(n, s))

    val r1 = Value.Tuple2(Value.Tag(Symbol.NamedSymbol("FOne"), Value.Int(43), fooTagTyp),
      Value.Tag(Symbol.NamedSymbol("FTwo"), Value.Tuple2(Value.Int(40), Value.Str("abc")), fooTagTyp))
    val r2 = valueWrapperFunc(f _)(Value.Tuple2(Value.Int(40), Value.Str("abc")))
    assertResult(r1)(r2)
  }

  test("Tag (inferred): Int => (FZero, FooTag)") {
    def f(n: Int) = n match {
      case 0 => (FZero, FZero)
      case 1 => (FZero, FOne(n))
      case 2 => (FZero, FTwo(n, "hello"))
    }

    val r01 = Value.Tuple2(Value.Tag(Symbol.NamedSymbol("FZero"), Value.Unit, fooTagTyp),
      Value.Tag(Symbol.NamedSymbol("FZero"), Value.Unit, fooTagTyp))
    val r02 = valueWrapperFunc(f _)(Value.Int(0))
    assertResult(r01)(r02)

    val r11 = Value.Tuple2(Value.Tag(Symbol.NamedSymbol("FZero"), Value.Unit, fooTagTyp),
      Value.Tag(Symbol.NamedSymbol("FOne"), Value.Int(1), fooTagTyp))
    val r12 = valueWrapperFunc(f _)(Value.Int(1))
    assertResult(r11)(r12)

    val r21 = Value.Tuple2(Value.Tag(Symbol.NamedSymbol("FZero"), Value.Unit, fooTagTyp),
      Value.Tag(Symbol.NamedSymbol("FTwo"), Value.Tuple2(Value.Int(2), Value.Str("hello")), fooTagTyp))
    val r22 = valueWrapperFunc(f _)(Value.Int(2))
    assertResult(r21)(r22)
  }

  test("Tag (ascribed): Int => (FZero, FooTag)") {
    def f(n: Int): (FZero.type, FooTag) = n match {
      case 0 => (FZero, FZero)
      case 1 => (FZero, FOne(n))
      case 2 => (FZero, FTwo(n, "hello"))
    }

    val r01 = Value.Tuple2(Value.Tag(Symbol.NamedSymbol("FZero"), Value.Unit, fooTagTyp),
      Value.Tag(Symbol.NamedSymbol("FZero"), Value.Unit, fooTagTyp))
    val r02 = valueWrapperFunc(f _)(Value.Int(0))
    assertResult(r01)(r02)

    val r11 = Value.Tuple2(Value.Tag(Symbol.NamedSymbol("FZero"), Value.Unit, fooTagTyp),
      Value.Tag(Symbol.NamedSymbol("FOne"), Value.Int(1), fooTagTyp))
    val r12 = valueWrapperFunc(f _)(Value.Int(1))
    assertResult(r11)(r12)

    val r21 = Value.Tuple2(Value.Tag(Symbol.NamedSymbol("FZero"), Value.Unit, fooTagTyp),
      Value.Tag(Symbol.NamedSymbol("FTwo"), Value.Tuple2(Value.Int(2), Value.Str("hello")), fooTagTyp))
    val r22 = valueWrapperFunc(f _)(Value.Int(2))
    assertResult(r21)(r22)
  }

  test("Tag: Int => Set[FOne]") {
    def f(n: Int) = {
      var s = Set(FOne(0))
      for (i <- 1 until n) s += FOne(i)
      s
    }

    val r1 = Value.Set(Set(0, 1, 2).map(x => Value.Tag(Symbol.NamedSymbol("FOne"), Value.Int(x), fooTagTyp)))
    val r2 = valueWrapperFunc(f _)(Value.Int(3))
    assertResult(r1)(r2)
  }

  test("Tag (inferred): Int => Set[FooTag]") {
    def f(n: Int) = {
      var s = Set(FZero, FOne(1), FTwo(2, "two"))
      n % 3 match {
        case 0 => s += FZero
        case 1 => s += FOne(1)
        case 2 => s += FTwo(2, "two")
      }
      s
    }

    val r1 = Value.Set(Set(Value.Tag(Symbol.NamedSymbol("FZero"), Value.Unit, fooTagTyp),
      Value.Tag(Symbol.NamedSymbol("FOne"), Value.Int(1), fooTagTyp),
      Value.Tag(Symbol.NamedSymbol("FTwo"), Value.Tuple2(Value.Int(2), Value.Str("two")), fooTagTyp),
      Value.Tag(Symbol.NamedSymbol("FOne"), Value.Int(1), fooTagTyp)))
    val r2 = valueWrapperFunc(f _)(Value.Int(3))
    assertResult(r1)(r2)
  }

  test("Tag (ascribed): Int => Set[FooTag]") {
    def f(n: Int): Set[FooTag] = {
      var s: Set[FooTag] = Set(FZero, FOne(1), FTwo(2, "two"))
      n % 3 match {
        case 0 => s += FZero
        case 1 => s += FOne(1)
        case 2 => s += FTwo(2, "two")
      }
      s
    }

    val r1 = Value.Set(Set(Value.Tag(Symbol.NamedSymbol("FZero"), Value.Unit, fooTagTyp),
      Value.Tag(Symbol.NamedSymbol("FOne"), Value.Int(1), fooTagTyp),
      Value.Tag(Symbol.NamedSymbol("FTwo"), Value.Tuple2(Value.Int(2), Value.Str("two")), fooTagTyp),
      Value.Tag(Symbol.NamedSymbol("FOne"), Value.Int(1), fooTagTyp)))
    val r2 = valueWrapperFunc(f _)(Value.Int(3))
    assertResult(r1)(r2)
  }

  test("Tag: FZero => Int") {
    def f(v: FZero.type) = 42
    val r1 = Value.Int(42)
    val r2 = valueWrapperFunc(f _)(Value.Tag(Symbol.NamedSymbol("FZero"), Value.Unit, fooTagTyp))
    assertResult(r1)(r2)
  }

  test("Tag: FOne => Int") {
    def f(v: FOne) = { val FOne(n: Int) = v; -n }
    val r1 = Value.Int(-5)
    val r2 = valueWrapperFunc(f _)(Value.Tag(Symbol.NamedSymbol("FOne"), Value.Int(5), fooTagTyp))
    assertResult(r1)(r2)
  }

  test("Tag: FTwo => Int") {
    def f(v: FTwo) = { val FTwo(n: Int, s: String) = v; n + s.length }
    val r1 = Value.Int(5)
    val r2 = valueWrapperFunc(f _)(Value.Tag(Symbol.NamedSymbol("Dbl"),
      Value.Tuple2(Value.Int(2), Value.Str("abc")), fooTagTyp))
    assertResult(r1)(r2)
  }

  test("Tag: FooTag => Int") {
    def f(v: FooTag) = v match {
      case FOne(n: Int) => n
      case FTwo(n: Int, s: String) => n + s.length
      case _ => 42
    }

    val r01 = Value.Int(-1)
    val r02 = valueWrapperFunc(f _)(Value.Tag(Symbol.NamedSymbol("FOne"), Value.Int(-1), fooTagTyp))
    assertResult(r01)(r02)

    val r11 = Value.Int(5)
    val r12 = valueWrapperFunc(f _)(Value.Tag(Symbol.NamedSymbol("FTwo"),
      Value.Tuple2(Value.Int(2), Value.Str("abc")), fooTagTyp))
    assertResult(r11)(r12)

    val r21 = Value.Int(42)
    val r22 = valueWrapperFunc(f _)(Value.Tag(Symbol.NamedSymbol("FZero"), Value.Unit, fooTagTyp))
    assertResult(r21)(r22)
  }

  test("Tag: (FOne, FTwo) => (Int, Int, String)") {
    def f(a: FOne, b: FTwo) = {
      val FOne(n: Int) = a
      val FTwo(m: Int, s: String) = b
      (n, m, s)
    }

    val r1 = Value.Tuple3(Value.Int(4), Value.Int(0), Value.Str("hello world"))
    val r2 = valueWrapperFunc(f _)(Value.Tuple2(Value.Tag(Symbol.NamedSymbol("FOne"), Value.Int(4), fooTagTyp),
      Value.Tag(Symbol.NamedSymbol("FTwo"), Value.Tuple2(Value.Int(0), Value.Str("hello world")), fooTagTyp)))
    assertResult(r1)(r2)
  }

  test("Tag: (FZero, FooTag) => Int") {
    def f(a: FZero.type, b: FooTag) = b match {
      case FZero => 0
      case FOne(n: Int) => 1 + n * 10
      case FTwo(m: Int, s: String) => 2 + m * 100 + s.length * 10
    }

    val r01 = Value.Int(0)
    val r02 = valueWrapperFunc(f _)(Value.Tuple2(Value.Tag(Symbol.NamedSymbol("FZero"), Value.Unit, fooTagTyp),
      Value.Tag(Symbol.NamedSymbol("FZero"), Value.Unit, fooTagTyp)))
    assertResult(r01)(r02)

    val r11 = Value.Int(421)
    val r12 = valueWrapperFunc(f _)(Value.Tuple2(Value.Tag(Symbol.NamedSymbol("FZero"), Value.Unit, fooTagTyp),
      Value.Tag(Symbol.NamedSymbol("FOne"), Value.Int(42), fooTagTyp)))
    assertResult(r11)(r12)

    val r21 = Value.Int(332)
    val r22 = valueWrapperFunc(f _)(Value.Tuple2(Value.Tag(Symbol.NamedSymbol("FZero"), Value.Unit, fooTagTyp),
      Value.Tag(Symbol.NamedSymbol("FTwo"), Value.Tuple2(Value.Int(3), Value.Str("abc")), fooTagTyp)))
    assertResult(r21)(r22)
  }

  test("Tag: Set[FOne] => Set[Int]") {
    def f(s: Set[FOne]) = s.map(x => { val FOne(n: Int) = x; n })

    val r1 = Value.Set(Set(1, 2, 3).map(Value.Int))
    val r2 = valueWrapperFunc(f _)(Value.Set(Set(Value.Tag(Symbol.NamedSymbol("FOne"), Value.Int(1), fooTagTyp),
      Value.Tag(Symbol.NamedSymbol("FOne"), Value.Int(2), fooTagTyp),
      Value.Tag(Symbol.NamedSymbol("FOne"), Value.Int(3), fooTagTyp))))
    assertResult(r1)(r2)
  }

  test("Tag: Set[FooTag] => (Int, Int, Int)") {
    def f(s: Set[FooTag]) = {
      s.toList.map({
        case FZero => (1, 0, 0)
        case FOne(_) => (0, 1, 0)
        case FTwo(_, _) => (0, 0, 1)
      }).fold(0,0,0)((t1, t2) => (t1._1 + t2._1, t1._2 + t2._2, t1._3 + t2._3))
    }

    val a = Value.Set(Set(Value.Tag(Symbol.NamedSymbol("FZero"), Value.Unit, fooTagTyp),
      Value.Tag(Symbol.NamedSymbol("FOne"), Value.Int(1), fooTagTyp),
      Value.Tag(Symbol.NamedSymbol("FOne"), Value.Int(3), fooTagTyp)
    ))

    val r1 = Value.Tuple3(Value.Int(1), Value.Int(2), Value.Int(0))
    val r2 = valueWrapperFunc(f _)(a)
    assertResult(r1)(r2)
  }

  test("scala2flix: Unit") {
    def f() = ()
    val v = ()

    assertResult(Value.Unit)(scala2flix(f()))
    assertResult(Value.Unit)(scala2flix(v))
    assertResult(Value.Unit)(scala2flix(()))
  }

  test("scala2flix: Boolean") {
    def f(b: Boolean) = !b
    val v = false

    assertResult(Value.Bool(true))(scala2flix(f(false)))
    assertResult(Value.Bool(false))(scala2flix(v))
    assertResult(Value.Bool(true))(scala2flix(true))
  }

  test("scala2flix: Int") {
    def f(n: Int) = -n
    val v = 42

    assertResult(Value.Int(-56))(scala2flix(f(56)))
    assertResult(Value.Int(42))(scala2flix(v))
    assertResult(Value.Int(3))(scala2flix(3))
  }

  test("scala2flix: Str") {
    def f(s: String) = "Hello " + s
    val v = "World"

    assertResult(Value.Str("Hello World!"))(scala2flix(f("World!")))
    assertResult(Value.Str("World"))(scala2flix(v))
    assertResult(Value.Str("abc"))(scala2flix("abc"))
  }

  test("scala2flix: Set[Int]") {
    def f(ss: Set[String]) = ss.map(_.length)
    val v = Set(4, 3, 5, 1)

    assertResult(Value.Set(Set(3, 2, 1).map(Value.Int)))(scala2flix(f(Set("abc", "de", "f"))))
    assertResult(Value.Set(Set(4, 3, 5, 1).map(Value.Int)))(scala2flix(v))
    assertResult(Value.Set(Set(4, 2).map(Value.Int)))(scala2flix(Set(4, 2)))
  }

  test("scala2flix: (Int, Set[String])") {
    def f(ss: Set[Int]) = (ss.size, ss.map(_.toString))
    val v = (1, Set("a", "c"))

    val r01 = Value.Tuple2(Value.Int(3), Value.Set(Set("1", "2", "3").map(Value.Str)))
    val r02 = scala2flix(f(Set(1, 2, 3)))
    val r11 = Value.Tuple2(Value.Int(1), Value.Set(Set("a", "c").map(Value.Str)))
    val r12 = scala2flix(v)
    val r21 = Value.Tuple2(Value.Int(-3), Value.Set(Set("f", "r").map(Value.Str)))
    val r22 = scala2flix(-3, Set("f", "r"))

    assertResult(r01)(r02)
    assertResult(r11)(r12)
    assertResult(r21)(r22)
  }

  test("scala2flix: Set[(Int, Int, Int)]") {
    def f(ss: Set[Int]) = ss.map(x => (x, x * 2, x * 3))
    val v = Set((1, 2, 3), (4, 5, 6))

    val r01 = Value.Set(Set((1, 2, 3), (2, 4, 6), (3, 6, 9)).map(
      x => Value.Tuple3(Value.Int(x._1), Value.Int(x._2), Value.Int(x._3))))
    val r02 = scala2flix(f(Set(1, 2, 3)))
    val r11 = Value.Set(Set(Value.Tuple3(Value.Int(1), Value.Int(2), Value.Int(3)),
      Value.Tuple3(Value.Int(4), Value.Int(5), Value.Int(6))))
    val r12 = scala2flix(v)
    val r21 = Value.Set(Set(Value.Tuple3(Value.Int(42), Value.Int(56), Value.Int(100))))
    val r22 = scala2flix(Set((42, 56, 100)))

    assertResult(r01)(r02)
    assertResult(r11)(r12)
    assertResult(r21)(r22)
  }

  test("scala2flix: (Int, String, Boolean, Boolean)") {
    def f(n: Int) = (n, n.toString, n > 10, n > 100)
    val v = (5, "hi", true, true)

    val r01 = Value.Tuple4(Value.Int(14), Value.Str("14"), Value.Bool(true), Value.Bool(false))
    val r02 = scala2flix(f(14))
    val r11 = Value.Tuple4(Value.Int(5), Value.Str("hi"), Value.Bool(true), Value.Bool(true))
    val r12 = scala2flix(v)
    val r21 = Value.Tuple4(Value.Int(2), Value.Str("abc"), Value.Bool(false), Value.Bool(true))
    val r22 = scala2flix((2, "abc", false, true))

    assertResult(r01)(r02)
    assertResult(r11)(r12)
    assertResult(r21)(r22)
  }

  test("scala2flix: (Int, Int, Int, Int, Int)") {
    def f(n: Int) = (n, n + 1, n + 2, n + 3, n + 4)
    val v = (5, 98, 232, -41, 0)

    val r01 = Value.Tuple5(Value.Int(4), Value.Int(5), Value.Int(6), Value.Int(7), Value.Int(8))
    val r02 = scala2flix(f(4))
    val r11 = Value.Tuple5(Value.Int(5), Value.Int(98), Value.Int(232), Value.Int(-41), Value.Int(0))
    val r12 = scala2flix(v)
    val r21 = Value.Tuple5(Value.Int(0), Value.Int(1), Value.Int(-1), Value.Int(2), Value.Int(-2))
    val r22 = scala2flix((0, 1, -1, 2, -2))

    assertResult(r01)(r02)
    assertResult(r11)(r12)
    assertResult(r21)(r22)
  }

  test("scala2flix: Foo") {
    def f(n: Int) = new Foo(n)
    val v = new Foo(42)

    assertResult(Value.Native(new Foo(4)))(scala2flix(f(4)))
    assertResult(Value.Native(new Foo(42)))(scala2flix(v))
    assertResult(Value.Native(new Foo(111)))(scala2flix(new Foo(111)))
  }

  test("scala2flix (tag): FZero") {
    def f() = FZero
    val v = FZero

    val r = Value.Tag(Symbol.NamedSymbol("FZero"), Value.Unit, fooTagTyp)

    assertResult(r)(scala2flix(f()))
    assertResult(r)(scala2flix(v))
    assertResult(r)(scala2flix(FZero))
  }

  test("scala2flix (tag): FOne") {
    def f(n: Int) = FOne(n)
    val v = FOne(3)

    val r01 = Value.Tag(Symbol.NamedSymbol("FOne"), Value.Int(4), fooTagTyp)
    val r02 = scala2flix(f(4))
    val r11 = Value.Tag(Symbol.NamedSymbol("FOne"), Value.Int(3), fooTagTyp)
    val r12 = scala2flix(v)
    val r21 = Value.Tag(Symbol.NamedSymbol("FOne"), Value.Int(42), fooTagTyp)
    val r22 = scala2flix(FOne(42))

    assertResult(r01)(r02)
    assertResult(r11)(r12)
    assertResult(r21)(r22)
  }

  test("scala2flix (tag): FTwo") {
    def f(n: Int, s: String) = FTwo(n, s)
    val v = FTwo(8, "hi")

    val r01 = Value.Tag(Symbol.NamedSymbol("FTwo"), Value.Tuple2(Value.Int(4), Value.Str("abc")), fooTagTyp)
    val r02 = scala2flix(f(4, "abc"))
    val r11 = Value.Tag(Symbol.NamedSymbol("FTwo"), Value.Tuple2(Value.Int(8), Value.Str("hi")), fooTagTyp)
    val r12 = scala2flix(v)
    val r21 = Value.Tag(Symbol.NamedSymbol("FTwo"), Value.Tuple2(Value.Int(42), Value.Str("42")), fooTagTyp)
    val r22 = scala2flix(FTwo(42, "42"))

    assertResult(r01)(r02)
    assertResult(r11)(r12)
    assertResult(r21)(r22)
  }

  test("scala2flix (tag, ascribed): FooTag") {
    def f(n: Int, s: String): FooTag = n match {
      case 0 => FZero
      case 1 => FOne(s.length)
      case 2 => FTwo(n, s)
    }
    val v1: FooTag = FZero
    val v2: FooTag = FOne(5)
    val v3: FooTag = FTwo(2, "xyz")

    val r1 = Value.Tag(Symbol.NamedSymbol("FZero"), Value.Unit, fooTagTyp)
    val r2 = Value.Tag(Symbol.NamedSymbol("FOne"), Value.Int(5), fooTagTyp)
    val r3 = Value.Tag(Symbol.NamedSymbol("FTwo"), Value.Tuple2(Value.Int(2), Value.Str("xyz")), fooTagTyp)

    assertResult(r1)(scala2flix(f(0, "foo")))
    assertResult(r1)(scala2flix(v1))
    assertResult(r2)(scala2flix(f(1, "hello")))
    assertResult(r2)(scala2flix(v2))
    assertResult(r3)(scala2flix(f(2, "xyz")))
    assertResult(r3)(scala2flix(v3))
  }

  test("scala2flix (tag, inferred): FooTag") {
    def f(n: Int, s: String) = n match {
      case 0 => FZero
      case 1 => FOne(s.length)
      case 2 => FTwo(n, s)
    }

    val r1 = Value.Tag(Symbol.NamedSymbol("FZero"), Value.Unit, fooTagTyp)
    val r2 = Value.Tag(Symbol.NamedSymbol("FOne"), Value.Int(5), fooTagTyp)
    val r3 = Value.Tag(Symbol.NamedSymbol("FTwo"), Value.Tuple2(Value.Int(2), Value.Str("xyz")), fooTagTyp)

    assertResult(r1)(scala2flix(f(0, "foo")))
    assertResult(r2)(scala2flix(f(1, "hello")))
    assertResult(r3)(scala2flix(f(2, "xyz")))
  }




  test("flix2scala: Unit") {
    def f() = Value.Unit
    val v = Value.Unit

    assertResult(())(flix2scala[Unit](f()))
    assertResult(())(flix2scala[Unit](v))
    assertResult(())(flix2scala[Unit](Value.Unit))
  }

  test("flix2scala: Boolean") {
    def f(v: Value) = {
      val Value.Bool(b) = v
      Value.Bool(!b)
    }
    val v = Value.Bool(false)

    assertResult(true)(flix2scala[Boolean](f(Value.Bool(false))))
    assertResult(false)(flix2scala[Boolean](v))
    assertResult(true)(flix2scala[Boolean](Value.Bool(true)))
  }

  test("flix2scala: Int") {
    def f(v: Value) = {
      val Value.Int(n) = v
      Value.Int(-n)
    }
    val v = Value.Int(42)

    assertResult(-56)(flix2scala[Int](f(Value.Int(56))))
    assertResult(42)(flix2scala[Int](v))
    assertResult(3)(flix2scala[Int](Value.Int(3)))
  }

  test("flix2scala: Str") {
    def f(v: Value) = {
      val Value.Str(s) = v
      Value.Str("Hello " + s)
    }
    val v = Value.Str("World")

    assertResult("Hello World!")(flix2scala[String](f(Value.Str("World!"))))
    assertResult("World")(flix2scala[String](v))
    assertResult("abc")(flix2scala[String](Value.Str("abc")))
  }

  test("flix2scala: Set[Int]") {
    def f(v: Value) = {
      val Value.Set(ss) = v
      Value.Set(ss.map { v =>
        val Value.Str(s) = v
        Value.Int(s.length)
      })
    }
    val v = Value.Set(Set(4, 3, 5, 1).map(Value.Int))

    assertResult(Set(3, 2, 1))(flix2scala[Set[Int]](f(Value.Set(Set("abc", "de", "f").map(Value.Str)))))
    assertResult(Set(4, 3, 5, 1))(flix2scala[Set[Int]](v))
    assertResult(Set(4, 2))(flix2scala[Set[Int]](Value.Set(Set(4, 2).map(Value.Int))))
  }

  test("flix2scala: (Int, Set[String])") {
    def f(v: Value) = {
      val Value.Set(ss) = v
      val fst = Value.Int(ss.size)
      val snd: Set[Value] = ss.map { v =>
        val Value.Int(n) = v
        Value.Str(n.toString)
      }
      Value.Tuple2(fst, Value.Set(snd))
    }
    val v = Value.Tuple2(Value.Int(1), Value.Set(Set("a", "c").map(Value.Str)))

    val r01 = (3, Set("1", "2", "3"))
    val r02 = flix2scala[(Int, Set[String])](f(Value.Set(Set(1, 2, 3).map(Value.Int))))
    val r11 = (1, Set("a", "c"))
    val r12 = flix2scala[(Int, Set[String])](v)
    val r21 = (-3, Set("f", "r"))
    val r22 = flix2scala[(Int, Set[String])](Value.Tuple2(Value.Int(-3), Value.Set(Set("f", "r").map(Value.Str))))

    assertResult(r01)(r02)
    assertResult(r11)(r12)
    assertResult(r21)(r22)
  }

  test("flix2scala: Set[(Int, Int, Int)]") {
    def f(v: Value) = {
      val Value.Set(ss) = v
      Value.Set(ss.map{ v =>
        val Value.Int(n) = v
        Value.Tuple3(Value.Int(n), Value.Int(n * 2), Value.Int(n * 3))
      })
    }
    val v = Value.Set(Set((1, 2, 3), (4, 5, 6)).map(
      x => Value.Tuple3(Value.Int(x._1), Value.Int(x._2), Value.Int(x._3))
    ))

    val r01 = Set((1, 2, 3), (2, 4, 6), (3, 6, 9))
    val r02 = flix2scala[Set[(Int, Int, Int)]](f(Value.Set(Set(1, 2, 3).map(Value.Int))))
    val r11 = Set((1, 2, 3), (4, 5, 6))
    val r12 = flix2scala[Set[(Int, Int, Int)]](v)
    val r21 = Set((42, 56, 100))
    val r22 = flix2scala[Set[(Int, Int, Int)]](Value.Set(Set(
      Value.Tuple3(Value.Int(42), Value.Int(56), Value.Int(100)))
    ))

    assertResult(r01)(r02)
    assertResult(r11)(r12)
    assertResult(r21)(r22)
  }

  test("flix2scala: (Int, String, Boolean, Boolean)") {
    def f(v: Value) = {
      val Value.Int(n) = v
      Value.Tuple4(Value.Int(n), Value.Str(n.toString), Value.Bool(n > 10), Value.Bool(n > 100))
    }
    val v = Value.Tuple4(Value.Int(5), Value.Str("hi"), Value.Bool(true), Value.Bool(true))

    val r01 = (14, "14", true, false)
    val r02 = flix2scala[(Int, String, Boolean, Boolean)](f(Value.Int(14)))
    val r11 = (5, "hi", true, true)
    val r12 = flix2scala[(Int, String, Boolean, Boolean)](v)
    val r21 = (2, "abc", false, true)
    val r22 = flix2scala[(Int, String, Boolean, Boolean)](Value.Tuple4(
      Value.Int(2), Value.Str("abc"), Value.Bool(false), Value.Bool(true)
    ))

    assertResult(r01)(r02)
    assertResult(r11)(r12)
    assertResult(r21)(r22)
  }

  test("flix2scala: (Int, Int, Int, Int, Int)") {
    def f(v: Value) = {
      val Value.Int(n) = v
      Value.Tuple5(Value.Int(n), Value.Int(n + 1), Value.Int(n + 2), Value.Int(n + 3), Value.Int(n + 4))
    }
    val v = Value.Tuple5(Value.Int(5), Value.Int(98), Value.Int(232), Value.Int(-41), Value.Int(0))

    val r01 = (4, 5, 6, 7, 8)
    val r02 = flix2scala[(Int, Int, Int, Int, Int)](f(Value.Int(4)))
    val r11 = (5, 98, 232, -41, 0)
    val r12 = flix2scala[(Int, Int, Int, Int, Int)](v)
    val r21 = (0, 1, -1, 2, -2)
    val r22 = flix2scala[(Int, Int, Int, Int, Int)](Value.Tuple5(
      Value.Int(0), Value.Int(1), Value.Int(-1), Value.Int(2), Value.Int(-2)
    ))

    assertResult(r01)(r02)
    assertResult(r11)(r12)
    assertResult(r21)(r22)
  }

  test("flix2scala: Foo") {
    def f(v: Value) = {
      val Value.Int(n) = v
      Value.Native(new Foo(n))
    }
    val v = Value.Native(new Foo(42))

    assertResult(new Foo(4))(flix2scala[Foo](f(Value.Int(4))))
    assertResult(new Foo(42))(flix2scala[Foo](v))
    assertResult(new Foo(111))(flix2scala[Foo](Value.Native(new Foo(111))))
  }

  test("flix2scala (tag): FZero") {
    def f() = Value.Tag(Symbol.NamedSymbol("FZero"), Value.Unit, fooTagTyp)
    val v = Value.Tag(Symbol.NamedSymbol("FZero"), Value.Unit, fooTagTyp)

    assertResult(FZero)(flix2scala[FZero.type](f()))
    assertResult(FZero)(flix2scala[FZero.type](v))
    assertResult(FZero)(flix2scala[FZero.type](Value.Tag(Symbol.NamedSymbol("FZero"), Value.Unit, fooTagTyp)))
  }

  test("flix2scala (tag): FOne") {
    def f(v: Value) = Value.Tag(Symbol.NamedSymbol("FOne"), v, fooTagTyp)
    val v = Value.Tag(Symbol.NamedSymbol("FOne"), Value.Int(3), fooTagTyp)

    val r01 = FOne(4)
    val r02 = flix2scala[FOne](f(Value.Int(4)))
    val r11 = FOne(3)
    val r12 = flix2scala[FOne](v)
    val r21 = FOne(42)
    val r22 = flix2scala[FOne](Value.Tag(Symbol.NamedSymbol("FOne"), Value.Int(42), fooTagTyp))

    assertResult(r01)(r02)
    assertResult(r11)(r12)
    assertResult(r21)(r22)
  }

  test("flix2scala (tag): FTwo") {
    def f(v1: Value, v2: Value) = Value.Tag(Symbol.NamedSymbol("FTwo"), Value.Tuple2(v1, v2), fooTagTyp)
    val v = Value.Tag(Symbol.NamedSymbol("FTwo"), Value.Tuple2(Value.Int(8), Value.Str("hi")), fooTagTyp)

    val r01 = FTwo(4, "abc")
    val r02 = flix2scala[FTwo](f(Value.Int(4), Value.Str("abc")))
    val r11 = FTwo(8, "hi")
    val r12 = flix2scala[FTwo](v)
    val r21 = FTwo(42, "42")
    val r22 = flix2scala[FTwo](Value.Tag(
      Symbol.NamedSymbol("FTwo"), Value.Tuple2(Value.Int(42), Value.Str("42")), fooTagTyp
    ))

    assertResult(r01)(r02)
    assertResult(r11)(r12)
    assertResult(r21)(r22)
  }

  test("flix2scala (tag): FooTag") {
    def f(v1: Value, v2: Value) = {
      val Value.Int(n) = v1
      val Value.Str(s) = v2
      n match {
        case 0 => Value.Tag(Symbol.NamedSymbol("FZero"), Value.Unit, fooTagTyp)
        case 1 => Value.Tag(Symbol.NamedSymbol("FOne"), Value.Int(s.length), fooTagTyp)
        case 2 => Value.Tag(Symbol.NamedSymbol("FTwo"), Value.Tuple2(v1, v2), fooTagTyp)
      }
    }

    val r1 = FZero
    val r2 = FOne(5)
    val r3 = FTwo(2, "xyz")

    assertResult(r1)(flix2scala[FooTag](f(Value.Int(0), Value.Str("foo"))))
    assertResult(r2)(flix2scala[FooTag](f(Value.Int(1), Value.Str("hello"))))
    assertResult(r3)(flix2scala[FooTag](f(Value.Int(2), Value.Str("xyz"))))
  }
}
