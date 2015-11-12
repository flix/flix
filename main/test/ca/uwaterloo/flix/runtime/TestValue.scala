package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast.Name

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

  test("Value.Bool hashing") {
    val set: mutable.Set[Value.Bool] = mutable.Set()
    val b1 = Value.True
    val b2 = Value.True
    val b3 = Value.False

    set += b1
    assert(set.contains(b1))
    assert(set.contains(b2))
    assert(!set.contains(b3))

    set -= b2
    assert(set.isEmpty)

    set += b1
    set += b2
    set += b3
    assert(set.size == 2)
  }

  test("Value.Bool pattern matching") {
    val v: Value = Value.True

    val result = v match {
      case v: Value.Bool => s"${v.b}"
      case _ => "unknown"
    }

    assert(result == "true")
  }

  test("Value.Int equality") {
    val i1 = Value.mkInt(0)
    val i2 = Value.mkInt(-1337)
    val i3 = Value.mkInt(0)
    val i4 = Value.mkInt(-1337)
    val i5 = Value.mkInt(0)

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

  test("Value.Int hashing") {
    val set: mutable.Set[Value.Int] = mutable.Set()
    val i1 = Value.mkInt(42)
    val i2 = Value.mkInt(42)
    val i3 = Value.mkInt(0xdeadbeef)

    set += i1
    assert(set.contains(i1))
    assert(set.contains(i2))
    assert(!set.contains(i3))

    set -= i2
    assert(set.isEmpty)

    set += i1
    set += i2
    set += i3
    assert(set.size == 2)
  }

  test("Value.Int pattern matching") {
    val v: Value = Value.mkInt(123456789)

    val result = v match {
      case v: Value.Int => s"${v.i}"
      case _ => "unknown"
    }

    assert(result == "123456789")
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

  test("Value.Str hashing") {
    val set: mutable.Set[Value.Str] = mutable.Set()
    val s1 = Value.mkStr("hello")
    val s2 = Value.mkStr("hello")
    val s3 = Value.mkStr("goodbye")

    set += s1
    assert(set.contains(s1))
    assert(set.contains(s2))
    assert(!set.contains(s3))

    set -= s2
    assert(set.isEmpty)

    set += s1
    set += s2
    set += s3
    assert(set.size == 2)
  }

  test("Value.Str pattern matching") {
    val v: Value = Value.mkStr("ABCDEFGHIJKLMNOPQRSTUVWXYZ")

    val result = v match {
      case v: Value.Str => s"${v.s}!!!"
      case _ => "unknown"
    }

    assert(result == "ABCDEFGHIJKLMNOPQRSTUVWXYZ!!!")
  }

  test("Value.Tag equality") {
    val name1 = Name.Resolved.mk(List("foo", "bar"))
    val name2 = Name.Resolved.mk(List("abc", "def"))
    val t1 = Value.mkTag(name1, "aaa", Value.mkInt(42))
    val t2 = Value.mkTag(name1, "aaa", Value.mkInt(10))
    val t3 = Value.mkTag(name1, "zzz", Value.mkInt(42))
    val t4 = Value.mkTag(name1, "zzz", Value.mkInt(10))
    val t5 = Value.mkTag(name2, "aaa", Value.mkInt(42))
    val t6 = Value.mkTag(name2, "aaa", Value.mkInt(10))
    val t7 = Value.mkTag(name2, "zzz", Value.mkInt(42))
    val t8 = Value.mkTag(name2, "zzz", Value.mkInt(10))
    val t9 = Value.mkTag(name1, "aaa", Value.mkInt(42))
    val t10 = Value.mkTag(name1, "aaa", Value.mkInt(42))

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
    val name1 = Name.Resolved.mk(List("foo", "bar"))
    val name2 = Name.Resolved.mk(List("abc", "def"))
    val t1 = Value.mkTag(name1, "aaa", Value.mkInt(42))
    val t2 = Value.mkTag(name1, "aaa", Value.mkInt(10))
    val t3 = Value.mkTag(name1, "zzz", Value.mkInt(42))
    val t4 = Value.mkTag(name1, "zzz", Value.mkInt(10))
    val t5 = Value.mkTag(name2, "aaa", Value.mkInt(42))
    val t6 = Value.mkTag(name2, "aaa", Value.mkInt(10))
    val t7 = Value.mkTag(name2, "zzz", Value.mkInt(42))
    val t8 = Value.mkTag(name2, "zzz", Value.mkInt(10))
    val t9 = Value.mkTag(name1, "aaa", Value.mkInt(42))

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
    val v: Value = Value.mkTag(Name.Resolved.mk(List("foo", "bar")), "aaa", Value.mkInt(42))

    val r1 = v match {
      case v: Value.Tag => v.enum
      case _ => Name.Resolved.mk(List("hi"))
    }
    assert(r1 == Name.Resolved.mk(List("foo", "bar")))

    val r2 = v match {
      case v: Value.Tag => v.tag
      case _ => "???"
    }
    assert(r2 == "aaa")

    val r3 = v match {
      case v: Value.Tag => v.value
      case _ => Value.Unit
    }
    assert(r3 == Value.mkInt(42))
  }

  // TODO(mhyee): toJava

  // TODO(mhyee): java2flix
}
