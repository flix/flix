/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
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
