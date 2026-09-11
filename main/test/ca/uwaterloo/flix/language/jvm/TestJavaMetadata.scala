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
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.util.InternalCompilerException
import org.scalatest.funsuite.AnyFunSuite

import java.lang.constant.ClassDesc
import java.lang.constant.ConstantDescs.*

class TestJavaMetadata extends AnyFunSuite {

  private val loc = SourceLocation.Unknown

  test("lookupClass") {
    implicit val flix: Flix = new Flix
    try {
      assert(JavaMetadata.lookupClass(CD_String, loc).desc == CD_String)
      assert(JavaMetadata.lookupClass(ClassDesc.of("java.util.List"), loc).isInterface)
    } finally flix.javaTypeProvider.close()
  }

  test("lookupClass.ThrowsOnMissingClass") {
    implicit val flix: Flix = new Flix
    try {
      intercept[InternalCompilerException](JavaMetadata.lookupClass(ClassDesc.of("java.lang.DoesNotExist"), loc))
    } finally flix.javaTypeProvider.close()
  }

  test("isSubtype") {
    implicit val flix: Flix = new Flix
    try {
      assert(JavaMetadata.isSubtype(CD_String, CD_Object, loc))
      assert(!JavaMetadata.isSubtype(CD_Object, CD_String, loc))
      intercept[InternalCompilerException](JavaMetadata.isSubtype(ClassDesc.of("java.lang.DoesNotExist"), CD_Object, loc))
    } finally flix.javaTypeProvider.close()
  }

  test("isThrowable") {
    implicit val flix: Flix = new Flix
    try {
      assert(JavaMetadata.isThrowable(CD_Throwable, loc))
      assert(JavaMetadata.isThrowable(ClassDesc.of("java.lang.RuntimeException"), loc))
      assert(!JavaMetadata.isThrowable(CD_String, loc))
      assert(!JavaMetadata.isThrowable(CD_int, loc))
    } finally flix.javaTypeProvider.close()
  }

  test("isObjectMethod") {
    implicit val flix: Flix = new Flix
    try {
      // Comparator redeclares equals(Object), which Object also declares, but compare is Comparator's own.
      val methods = JavaMetadata.overridableMethods(ClassDesc.of("java.util.Comparator"), loc)
      val equals = methods.find(_.ref.name == "equals").get
      val compare = methods.find(_.ref.name == "compare").get
      assert(JavaMetadata.isObjectMethod(equals, loc))
      assert(!JavaMetadata.isObjectMethod(compare, loc))
    } finally flix.javaTypeProvider.close()
  }

}
