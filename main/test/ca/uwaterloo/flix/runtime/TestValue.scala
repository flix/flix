package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast.{Symbol, Name}

import scala.collection.mutable
import org.scalatest.FunSuite

class TestValue extends FunSuite {
  test("Value.Bool equality") {
    val b1 = Value.True
    val b2 = Value.True
    val b3 = Value.False
    val b4 = Value.True

    assert(b1 == b1)
    assert(b1 == b2)
    assert(b2 == b1)
    assert(b1 != b3)
    assert(b2 != b3)
    assert(b1 == b4)
    assert(b2 == b4)
    assert(b3 != b4)
  }

  test("Value.Int equality") {
    val i1 = Value.mkInt32(0)
    val i2 = Value.mkInt32(-1337)
    val i3 = Value.mkInt32(0)
    val i4 = Value.mkInt32(-1337)
    val i5 = Value.mkInt32(0)

    assert(i1 == i1)
    assert(i1 != i2)
    assert(i1 == i3)
    assert(i3 == i1)
    assert(i1 == i5)
    assert(i3 == i5)
    assert(i1 != i4)
    assert(i2 != i3)
    assert(i2 == i4)
    assert(i3 != i4)
  }

  test("Value.Str equality") {
    val s1 = Value.mkStr("foo")
    val s2 = Value.mkStr("bar")
    val s3 = Value.mkStr("foo")
    val s4 = Value.mkStr("bar")
    val s5 = Value.mkStr("foo")

    assert(s1 == s1)
    assert(s1 != s2)
    assert(s1 == s3)
    assert(s3 == s1)
    assert(s1 == s5)
    assert(s3 == s5)
    assert(s1 != s4)
    assert(s2 != s3)
    assert(s2 == s4)
    assert(s3 != s4)
  }

  test("Value.Tag equality") {
    val name1 = Symbol.Resolved.mk(List("foo", "bar"))
    val name2 = Symbol.Resolved.mk(List("abc", "def"))
    val t1 = Value.mkTag(name1, "aaa", Value.mkInt32(42))
    val t2 = Value.mkTag(name1, "aaa", Value.mkInt32(10))
    val t3 = Value.mkTag(name1, "zzz", Value.mkInt32(42))
    val t4 = Value.mkTag(name1, "zzz", Value.mkInt32(10))
    val t5 = Value.mkTag(name2, "aaa", Value.mkInt32(42))
    val t6 = Value.mkTag(name2, "aaa", Value.mkInt32(10))
    val t7 = Value.mkTag(name2, "zzz", Value.mkInt32(42))
    val t8 = Value.mkTag(name2, "zzz", Value.mkInt32(10))
    val t9 = Value.mkTag(name1, "aaa", Value.mkInt32(42))
    val t10 = Value.mkTag(name1, "aaa", Value.mkInt32(42))

    assert(t1 == t1)
    assert(t1 == t9)
    assert(t9 == t1)
    assert(t1 == t10)
    assert(t9 == t10)
    assert(t1 != t2)
    assert(t1 != t3)
    assert(t1 != t4)
    assert(t1 != t5)
    assert(t1 != t6)
    assert(t1 != t7)
    assert(t1 != t8)
    assert(t2 != t3)
    assert(t2 != t4)
    assert(t2 != t5)
    assert(t2 != t6)
    assert(t2 != t7)
    assert(t2 != t8)
    assert(t3 != t4)
    assert(t3 != t5)
    assert(t3 != t6)
    assert(t3 != t7)
    assert(t3 != t8)
    assert(t4 != t5)
    assert(t4 != t6)
    assert(t4 != t7)
    assert(t4 != t8)
    assert(t5 != t6)
    assert(t5 != t7)
    assert(t5 != t8)
    assert(t6 != t7)
    assert(t6 != t8)
    assert(t7 != t8)
  }

  test("Value.Tag hashing") {
    val set: mutable.Set[Value.Tag] = mutable.Set()
    val name1 = Symbol.Resolved.mk(List("foo", "bar"))
    val name2 = Symbol.Resolved.mk(List("abc", "def"))
    val t1 = Value.mkTag(name1, "aaa", Value.mkInt32(42))
    val t2 = Value.mkTag(name1, "aaa", Value.mkInt32(10))
    val t3 = Value.mkTag(name1, "zzz", Value.mkInt32(42))
    val t4 = Value.mkTag(name1, "zzz", Value.mkInt32(10))
    val t5 = Value.mkTag(name2, "aaa", Value.mkInt32(42))
    val t6 = Value.mkTag(name2, "aaa", Value.mkInt32(10))
    val t7 = Value.mkTag(name2, "zzz", Value.mkInt32(42))
    val t8 = Value.mkTag(name2, "zzz", Value.mkInt32(10))
    val t9 = Value.mkTag(name1, "aaa", Value.mkInt32(42))

    set += t1
    assert(set.contains(t1))
    assert(set.contains(t9))
    assert(!set.contains(t2))
    assert(!set.contains(t3))
    assert(!set.contains(t4))
    assert(!set.contains(t5))
    assert(!set.contains(t6))
    assert(!set.contains(t7))
    assert(!set.contains(t8))

    set -= t9
    assert(set.isEmpty)

    set += t1
    set += t2
    set += t9
    assert(set.size == 2)
  }

  test("Value.Tag pattern matching") {
    val v = Value.mkTag(Symbol.Resolved.mk(List("foo", "bar")), "aaa", Value.mkInt32(42))

    val r1 = v match {
      case v: Value.Tag => v.enum
      case _ => Symbol.Resolved.mk(List("hi"))
    }
    assert(r1 == Symbol.Resolved.mk(List("foo", "bar")))

    val r2 = v match {
      case v: Value.Tag => v.tag
      case _ => "???"
    }
    assert(r2 == "aaa")

    val r3 = v match {
      case v: Value.Tag => v.value
      case _ => Value.Unit
    }
    assert(r3 == Value.mkInt32(42))
  }

}
