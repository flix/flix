/*
 * Copyright 2026 Flix Authors
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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.jvm.JavaLookupError.MissingClass
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import org.scalatest.funsuite.AnyFunSuite

import java.lang.constant.ClassDesc
import java.lang.constant.ConstantDescs.*

class TestJavaHierarchy extends AnyFunSuite {

  test("isSubtype.ReferenceTypes") {
    implicit val flix: Flix = new Flix
    try {
      assert(JavaHierarchy.isSubtype(CD_String, CD_Object) == Ok(true))
      assert(JavaHierarchy.isSubtype(CD_String, ClassDesc.of("java.lang.CharSequence")) == Ok(true))
      assert(JavaHierarchy.isSubtype(CD_Object, CD_String) == Ok(false))
      assert(JavaHierarchy.isSubtype(CD_String, CD_Integer) == Ok(false))
    } finally flix.javaTypeProvider.close()
  }

  test("isSubtype.PrimitiveTypes") {
    implicit val flix: Flix = new Flix
    try {
      assert(JavaHierarchy.isSubtype(CD_int, CD_int) == Ok(true))
      assert(JavaHierarchy.isSubtype(CD_int, CD_long) == Ok(false))
      assert(JavaHierarchy.isSubtype(CD_int, CD_Object) == Ok(false))
      assert(JavaHierarchy.isSubtype(CD_Integer, CD_int) == Ok(false))
    } finally flix.javaTypeProvider.close()
  }

  test("isSubtype.ArrayTypes") {
    implicit val flix: Flix = new Flix
    try {
      val cloneable = ClassDesc.of("java.lang.Cloneable")
      val serializable = ClassDesc.of("java.io.Serializable")
      assert(JavaHierarchy.isSubtype(CD_String.arrayType(), CD_Object) == Ok(true))
      assert(JavaHierarchy.isSubtype(CD_String.arrayType(), cloneable) == Ok(true))
      assert(JavaHierarchy.isSubtype(CD_String.arrayType(), serializable) == Ok(true))
      assert(JavaHierarchy.isSubtype(CD_String.arrayType(), CD_Object.arrayType()) == Ok(true))
      assert(JavaHierarchy.isSubtype(CD_int.arrayType(), CD_Object) == Ok(true))
      assert(JavaHierarchy.isSubtype(CD_int.arrayType(), CD_int.arrayType()) == Ok(true))
      assert(JavaHierarchy.isSubtype(CD_int.arrayType(), CD_long.arrayType()) == Ok(false))
      assert(JavaHierarchy.isSubtype(CD_int.arrayType(), CD_Object.arrayType()) == Ok(false))
      assert(JavaHierarchy.isSubtype(CD_Object.arrayType(), CD_String.arrayType()) == Ok(false))
      assert(JavaHierarchy.isSubtype(CD_Object, CD_Object.arrayType()) == Ok(false))
    } finally flix.javaTypeProvider.close()
  }

  test("isSubtype.ReportsMissingClass") {
    implicit val flix: Flix = new Flix
    try {
      val missing = ClassDesc.of("java.lang.DoesNotExist")
      assert(JavaHierarchy.isSubtype(missing, CD_Object) == Err(MissingClass(missing)))
    } finally flix.javaTypeProvider.close()
  }

}
