/*
 * Copyright 2026 Magnus Madsen
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

package ca.uwaterloo.flix.language.jvm

import org.scalatest.funsuite.AnyFunSuite

import java.lang.constant.ClassDesc
import java.lang.constant.ConstantDescs.{CD_String, CD_int}

class TestClassDescs extends AnyFunSuite {

  private val Entry: ClassDesc = ClassDesc.of("java.util.Map$Entry")
  private val Local: ClassDesc = ClassDesc.of("Outer$1Local")
  private val Anonymous: ClassDesc = ClassDesc.of("Outer$1")
  private val Dollar: ClassDesc = ClassDesc.of("Foo$")

  test("internalNameOf.01") {
    assert(ClassDescs.internalNameOf(CD_String) == "java/lang/String")
  }

  test("internalNameOf.02") {
    assert(ClassDescs.internalNameOf(Entry) == "java/util/Map$Entry")
  }

  test("internalNameOf.03") {
    assert(ClassDescs.internalNameOf(CD_String.arrayType()) == "[Ljava/lang/String;")
  }

  test("classFileNameOf.01") {
    assert(ClassDescs.classFileNameOf(Entry) == "java/util/Map$Entry.class")
  }

  test("binaryNameOf.01") {
    assert(ClassDescs.binaryNameOf(CD_String) == classOf[String].getName)
  }

  test("binaryNameOf.02") {
    assert(ClassDescs.binaryNameOf(Entry) == classOf[java.util.Map.Entry[?, ?]].getName)
  }

  test("binaryNameOf.03") {
    assert(ClassDescs.binaryNameOf(CD_int) == classOf[Int].getName)
  }

  test("binaryNameOf.04") {
    assert(ClassDescs.binaryNameOf(CD_int.arrayType()) == classOf[Array[Int]].getName)
  }

  test("binaryNameOf.05") {
    assert(ClassDescs.binaryNameOf(CD_String.arrayType(2)) == classOf[Array[Array[String]]].getName)
  }

  test("binaryNameOf.06") {
    assert(ClassDescs.binaryNameOf(Entry.arrayType()) == classOf[Array[java.util.Map.Entry[?, ?]]].getName)
  }

  test("canonicalNameOf.01") {
    assert(ClassDescs.canonicalNameOf(CD_String) == classOf[String].getCanonicalName)
  }

  test("canonicalNameOf.02") {
    assert(ClassDescs.canonicalNameOf(Entry) == classOf[java.util.Map.Entry[?, ?]].getCanonicalName)
  }

  test("canonicalNameOf.03") {
    assert(ClassDescs.canonicalNameOf(CD_int.arrayType()) == classOf[Array[Int]].getCanonicalName)
  }

  test("canonicalNameOf.04") {
    assert(ClassDescs.canonicalNameOf(CD_String.arrayType(2)) == classOf[Array[Array[String]]].getCanonicalName)
  }

  test("canonicalNameOf.05") {
    assert(ClassDescs.canonicalNameOf(Entry.arrayType()) == classOf[Array[java.util.Map.Entry[?, ?]]].getCanonicalName)
  }

  test("simpleNameOf.01") {
    assert(ClassDescs.simpleNameOf(CD_String) == classOf[String].getSimpleName)
  }

  test("simpleNameOf.02") {
    assert(ClassDescs.simpleNameOf(Entry) == classOf[java.util.Map.Entry[?, ?]].getSimpleName)
  }

  test("simpleNameOf.03") {
    assert(ClassDescs.simpleNameOf(CD_int) == classOf[Int].getSimpleName)
  }

  test("simpleNameOf.04") {
    assert(ClassDescs.simpleNameOf(CD_int.arrayType()) == classOf[Array[Int]].getSimpleName)
  }

  test("simpleNameOf.05") {
    assert(ClassDescs.simpleNameOf(CD_String.arrayType(2)) == classOf[Array[Array[String]]].getSimpleName)
  }

  test("simpleNameOf.06") {
    assert(ClassDescs.simpleNameOf(Entry.arrayType()) == classOf[Array[java.util.Map.Entry[?, ?]]].getSimpleName)
  }

  test("simpleNameOf.07") {
    // A local class `Outer$1Local` has the simple name `Local`.
    assert(ClassDescs.simpleNameOf(Local) == "Local")
  }

  test("simpleNameOf.08") {
    // An anonymous class `Outer$1` has the empty simple name.
    assert(ClassDescs.simpleNameOf(Anonymous) == "")
  }

  test("simpleNameOf.09") {
    // A trailing `$` is part of the name of a top-level class.
    assert(ClassDescs.simpleNameOf(Dollar) == "Foo$")
  }

}
