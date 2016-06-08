/*
 * Copyright 2015-2016 Ming-Ho Yee
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
    val t1 = Value.mkTag("a", Value.mkInt32(42))
    val t2 = Value.mkTag("a", Value.mkInt32(21))
    val t3 = Value.mkTag("a", Value.mkInt32(42))

    val t4 = Value.mkTag("b", Value.mkInt32(42))
    val t5 = Value.mkTag("b", Value.mkInt32(21))
    val t6 = Value.mkTag("b", Value.mkInt32(42))

    assert(t1 == t1)
    assert(t2 == t2)
    assert(t3 == t3)
    assert(t4 == t4)
    assert(t5 == t5)
    assert(t6 == t6)

    assert(t1 == t3)
    assert(t3 == t1)
    assert(t4 == t6)
    assert(t6 == t4)

    assert(t1 != t2)
    assert(t2 != t1)
    assert(t1 != t4)
    assert(t4 != t1)
    assert(t1 != t5)
    assert(t5 != t1)
    assert(t1 != t6)
    assert(t6 != t1)
  }

  test("Value.Tag hashing") {
    val set: mutable.Set[Value.Tag] = mutable.Set()

    val t1 = Value.mkTag("a", Value.mkInt32(42))
    val t2 = Value.mkTag("a", Value.mkInt32(21))
    val t3 = Value.mkTag("a", Value.mkInt32(42))

    val t4 = Value.mkTag("b", Value.mkInt32(42))
    val t5 = Value.mkTag("b", Value.mkInt32(21))
    val t6 = Value.mkTag("b", Value.mkInt32(42))

    set += t1
    assert(set.contains(t1))
    assert(set.contains(t3))
    assert(!set.contains(t2))
    assert(!set.contains(t4))
    assert(!set.contains(t5))
    assert(!set.contains(t6))

    set += t4
    assert(set.contains(t1))
    assert(set.contains(t3))
    assert(set.contains(t4))
    assert(set.contains(t6))
    assert(!set.contains(t2))
    assert(!set.contains(t5))

    assert(set.size == 2)
  }

  test("Value.Tag pattern matching") {
    val v = Value.mkTag("aaa", Value.mkInt32(42))

    val r1 = v match {
      case v: Value.Tag => v.tag
      case _ => "???"
    }
    assert(r1 == "aaa")

    val r2 = v match {
      case v: Value.Tag => v.value
      case _ => Value.Unit
    }
    assert(r2 == Value.mkInt32(42))
  }

}
