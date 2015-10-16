package ca.uwaterloo.flix.runtime

import scala.collection.mutable
import org.scalatest.FunSuite

class TestValue extends FunSuite {

  test("Value.Bool equality") {
    val b1 = Value.mkBool(true)
    val b2 = Value.mkBool(true)
    val b3 = Value.mkBool(false)
    val b4 = Value.mkBool(true)

    assert(b1 == b2)
    assert(b1 != b3)
    assert(b2 != b3)
    assert(b1 == b4)
    assert(b2 == b4)
    assert(b3 != b4)
  }

  test("Value.Bool hashing") {
    val set: mutable.Set[Value.Bool] = mutable.Set()
    val b1 = Value.mkBool(true)
    val b2 = Value.mkBool(true)
    val b3 = Value.mkBool(false)

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
    val v: Value = Value.mkBool(true)

    val result = v match {
      case Value.Bool(b) => s"$b"
      case _ => "unknown"
    }

    assert(result == "true")
  }

  test("Value.Int equality") {
    val i1 = Value.mkInt(0)
    val i2 = Value.mkInt(-1337)
    val i3 = Value.mkInt(0)
    val i4 = Value.mkInt(-1337)

    assert(i1 != i2)
    assert(i1 == i3)
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
      case Value.Int(i) => s"$i"
      case _ => "unknown"
    }

    assert(result == "123456789")
  }
}
