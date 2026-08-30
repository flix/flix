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
package ca.uwaterloo.flix.language.phase.typer.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.phase.typer.jvm.JavaArgument.*
import ca.uwaterloo.flix.language.phase.typer.jvm.JavaLookupError.MissingClass
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import org.scalatest.funsuite.AnyFunSuite

import java.lang.constant.ConstantDescs.*
import java.lang.constant.{ClassDesc, MethodTypeDesc}

class TestJavaMemberResolver extends AnyFunSuite {

  test("constructors.SelectsExactMatch") {
    val flix = new Flix()
    implicit val implicitFlix: Flix = flix
    try {
      val owner = ClassDesc.of("java.lang.StringBuilder")
      JavaMemberResolver.constructors(owner, List(Typed(CD_int))) match {
        case Ok(constructors) =>
          assert(constructors.map(_.ref.descriptor) == List(MethodTypeDesc.ofDescriptor("(I)V")))
        case Err(error) => fail(error.toString)
      }
    } finally flix.javaTypeProvider.close()
  }

  test("constructors.SelectsReferenceSubtypeMatch") {
    val flix = new Flix()
    implicit val implicitFlix: Flix = flix
    try {
      val owner = ClassDesc.of("java.util.ArrayList")
      val linkedList = ClassDesc.of("java.util.LinkedList")
      JavaMemberResolver.constructors(owner, List(Typed(linkedList))) match {
        case Ok(constructors) =>
          val collectionConstructor = MethodTypeDesc.ofDescriptor("(Ljava/util/Collection;)V")
          assert(constructors.map(_.ref.descriptor) == List(collectionConstructor))
        case Err(error) => fail(error.toString)
      }
    } finally flix.javaTypeProvider.close()
  }

  test("constructors.SelectsPrimitiveWideningMatch") {
    val flix = new Flix()
    implicit val implicitFlix: Flix = flix
    try {
      val owner = ClassDesc.of("java.math.BigDecimal")
      JavaMemberResolver.constructors(owner, List(Typed(CD_byte))) match {
        case Ok(constructors) =>
          assert(constructors.map(_.ref.descriptor) == List(MethodTypeDesc.ofDescriptor("(I)V")))
        case Err(error) => fail(error.toString)
      }
    } finally flix.javaTypeProvider.close()
  }

  test("constructors.SelectsPrimitiveBoxingMatch") {
    val flix = new Flix()
    implicit val implicitFlix: Flix = flix
    try {
      val owner = ClassDesc.of("java.util.concurrent.atomic.AtomicReference")
      JavaMemberResolver.constructors(owner, List(Typed(CD_int))) match {
        case Ok(constructors) =>
          val objectConstructor = MethodTypeDesc.ofDescriptor("(Ljava/lang/Object;)V")
          assert(constructors.map(_.ref.descriptor) == List(objectConstructor))
        case Err(error) => fail(error.toString)
      }
    } finally flix.javaTypeProvider.close()
  }

  test("constructors.RejectsUnsupportedUnboxing") {
    val flix = new Flix()
    implicit val implicitFlix: Flix = flix
    try {
      val owner = ClassDesc.of("java.lang.StringBuilder")
      JavaMemberResolver.constructors(owner, List(Typed(CD_Integer))) match {
        case Ok(constructors) => assert(constructors.isEmpty)
        case Err(error) => fail(error.toString)
      }
    } finally flix.javaTypeProvider.close()
  }

  test("constructors.ReturnsTiedNullMatches") {
    val flix = new Flix()
    implicit val implicitFlix: Flix = flix
    try {
      val owner = ClassDesc.of("java.lang.StringBuilder")
      JavaMemberResolver.constructors(owner, List(Null)) match {
        case Ok(constructors) =>
          val expected = Set(
            MethodTypeDesc.ofDescriptor("(Ljava/lang/String;)V"),
            MethodTypeDesc.ofDescriptor("(Ljava/lang/CharSequence;)V")
          )
          assert(constructors.map(_.ref.descriptor).toSet == expected)
        case Err(error) => fail(error.toString)
      }
    } finally flix.javaTypeProvider.close()
  }

  test("constructors.SelectsFixedArityVarArgsMatch") {
    val flix = new Flix()
    implicit val implicitFlix: Flix = flix
    try {
      val owner = ClassDesc.of("java.lang.ProcessBuilder")
      val strings = CD_String.arrayType()
      JavaMemberResolver.constructors(owner, List(Typed(strings))) match {
        case Ok(constructors) =>
          val varArgsConstructor = MethodTypeDesc.ofDescriptor("([Ljava/lang/String;)V")
          assert(constructors.map(_.ref.descriptor) == List(varArgsConstructor))
        case Err(error) => fail(error.toString)
      }
    } finally flix.javaTypeProvider.close()
  }

  test("constructors.SelectsExpandedVarArgsMatch") {
    val flix = new Flix()
    implicit val implicitFlix: Flix = flix
    try {
      val owner = ClassDesc.of("java.lang.ProcessBuilder")
      val arguments = List(Typed(CD_String), Typed(CD_String))
      JavaMemberResolver.constructors(owner, arguments) match {
        case Ok(constructors) =>
          val varArgsConstructor = MethodTypeDesc.ofDescriptor("([Ljava/lang/String;)V")
          assert(constructors.map(_.ref.descriptor) == List(varArgsConstructor))
        case Err(error) => fail(error.toString)
      }
    } finally flix.javaTypeProvider.close()
  }

  test("constructors.ReturnsNoMatch") {
    val flix = new Flix()
    implicit val implicitFlix: Flix = flix
    try {
      val owner = ClassDesc.of("java.lang.StringBuilder")
      JavaMemberResolver.constructors(owner, List(Typed(CD_boolean))) match {
        case Ok(constructors) => assert(constructors.isEmpty)
        case Err(error) => fail(error.toString)
      }
    } finally flix.javaTypeProvider.close()
  }

  test("constructors.ReportsMissingClass") {
    val flix = new Flix()
    implicit val implicitFlix: Flix = flix
    try {
      val missing = ClassDesc.of("dev.flix.prototype.DoesNotExist")
      assert(JavaMemberResolver.constructors(missing, Nil) == Err(MissingClass(missing)))
    } finally flix.javaTypeProvider.close()
  }

}
