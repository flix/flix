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
    implicit val flix: Flix = new Flix
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
    implicit val flix: Flix = new Flix
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
    implicit val flix: Flix = new Flix
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
    implicit val flix: Flix = new Flix
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
    implicit val flix: Flix = new Flix
    try {
      val owner = ClassDesc.of("java.lang.StringBuilder")
      JavaMemberResolver.constructors(owner, List(Typed(CD_Integer))) match {
        case Ok(constructors) => assert(constructors.isEmpty)
        case Err(error) => fail(error.toString)
      }
    } finally flix.javaTypeProvider.close()
  }

  test("constructors.ReturnsTiedNullMatches") {
    implicit val flix: Flix = new Flix
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
    implicit val flix: Flix = new Flix
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
    implicit val flix: Flix = new Flix
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
    implicit val flix: Flix = new Flix
    try {
      val owner = ClassDesc.of("java.lang.StringBuilder")
      JavaMemberResolver.constructors(owner, List(Typed(CD_boolean))) match {
        case Ok(constructors) => assert(constructors.isEmpty)
        case Err(error) => fail(error.toString)
      }
    } finally flix.javaTypeProvider.close()
  }

  test("constructors.ReportsMissingClass") {
    implicit val flix: Flix = new Flix
    try {
      val missing = ClassDesc.of("dev.flix.prototype.DoesNotExist")
      assert(JavaMemberResolver.constructors(missing, Nil) == Err(MissingClass(missing)))
    } finally flix.javaTypeProvider.close()
  }

  test("methods.SelectsExactInstanceOverload") {
    implicit val flix: Flix = new Flix
    try {
      val owner = ClassDesc.of("java.util.ArrayList")
      JavaMemberResolver.methods(owner, "remove", List(Typed(CD_int)), static = false) match {
        case Ok(methods) =>
          assert(methods.map(_.ref.owner) == List(owner))
          assert(methods.map(_.ref.descriptor) == List(MethodTypeDesc.ofDescriptor("(I)Ljava/lang/Object;")))
        case Err(error) => fail(error.toString)
      }
    } finally flix.javaTypeProvider.close()
  }

  test("methods.PreservesSyntheticBridge") {
    implicit val flix: Flix = new Flix
    try {
      val owner = ClassDesc.of("dev.flix.test.TestJavaMethodResolution$Bridge")
      JavaMemberResolver.methods(owner, "apply", List(Typed(CD_Object)), static = false) match {
        case Ok(methods) =>
          assert(methods.map(_.ref.owner) == List(owner))
          assert(methods.map(_.ref.descriptor) == List(MethodTypeDesc.ofDescriptor("(Ljava/lang/Object;)Ljava/lang/Object;")))
        case Err(error) => fail(error.toString)
      }

      val accessibleBridge = ClassDesc.of("dev.flix.test.TestJavaMethodResolution$Accessible")
      JavaMemberResolver.methods(accessibleBridge, "apply", List(Typed(CD_Object)), static = false) match {
        case Ok(methods) => assert(methods.map(_.ref.owner) == List(accessibleBridge))
        case Err(error) => fail(error.toString)
      }
    } finally flix.javaTypeProvider.close()
  }

  test("methods.NormalizesNonPublicDeclaringOwner") {
    implicit val flix: Flix = new Flix
    try {
      val owner = ClassDesc.of("dev.flix.test.PackagePrivateFunction")
      val function = ClassDesc.of("java.util.function.Function")
      JavaMemberResolver.methods(owner, "apply", List(Typed(CD_String)), static = false) match {
        case Ok(methods) =>
          assert(methods.map(_.ref.owner) == List(function))
          assert(methods.map(_.ref.descriptor) == List(MethodTypeDesc.ofDescriptor("(Ljava/lang/Object;)Ljava/lang/Object;")))
        case Err(error) => fail(error.toString)
      }
    } finally flix.javaTypeProvider.close()
  }

  test("methods.FallsBackToObject") {
    implicit val flix: Flix = new Flix
    try {
      val owner = ClassDesc.of("java.util.List")
      JavaMemberResolver.methods(owner, "toString", Nil, static = false) match {
        case Ok(methods) =>
          assert(methods.map(_.ref.owner) == List(CD_Object))
          assert(methods.map(_.ref.descriptor) == List(MethodTypeDesc.ofDescriptor("()Ljava/lang/String;")))
        case Err(error) => fail(error.toString)
      }
    } finally flix.javaTypeProvider.close()
  }

  test("methods.ResolvesArrayObjectMethod") {
    implicit val flix: Flix = new Flix
    try {
      val owner = CD_String.arrayType()
      JavaMemberResolver.methods(owner, "toString", Nil, static = false) match {
        case Ok(methods) =>
          assert(methods.map(_.ref.owner) == List(CD_Object))
          assert(methods.map(_.ref.descriptor) == List(MethodTypeDesc.ofDescriptor("()Ljava/lang/String;")))
        case Err(error) => fail(error.toString)
      }
    } finally flix.javaTypeProvider.close()
  }

  test("methods.ResolvesStaticHierarchy") {
    implicit val flix: Flix = new Flix
    try {
      val base = ClassDesc.of("dev.flix.test.TestJavaMethodResolution$StaticBase")
      val child = ClassDesc.of("dev.flix.test.TestJavaMethodResolution$StaticChild")
      val childInterface = ClassDesc.of("dev.flix.test.TestJavaMethodResolution$StaticChildInterface")

      JavaMemberResolver.methods(child, "inherited", List(Typed(CD_Integer)), static = true) match {
        case Ok(methods) =>
          assert(methods.map(_.ref.owner) == List(base))
          assert(methods.map(_.ref.descriptor) == List(MethodTypeDesc.ofDescriptor("(Ljava/lang/Number;)Ljava/lang/Object;")))
        case Err(error) => fail(error.toString)
      }

      JavaMemberResolver.methods(child, "hidden", List(Typed(CD_Object)), static = true) match {
        case Ok(methods) => assert(methods.map(_.ref.owner) == List(child))
        case Err(error) => fail(error.toString)
      }

      JavaMemberResolver.methods(child, "overloaded", List(Typed(CD_String)), static = true) match {
        case Ok(methods) =>
          assert(methods.map(_.ref.owner) == List(child))
          assert(methods.map(_.ref.descriptor) == List(MethodTypeDesc.ofDescriptor("(Ljava/lang/String;)Ljava/lang/String;")))
        case Err(error) => fail(error.toString)
      }

      JavaMemberResolver.methods(childInterface, "notInherited", List(Typed(CD_Object)), static = true) match {
        case Ok(methods) => assert(methods.isEmpty)
        case Err(error) => fail(error.toString)
      }

      JavaMemberResolver.methods(child, "exactStatic", List(Typed(ClassDesc.of("java.lang.StringBuilder"))), static = false) match {
        case Ok(methods) => assert(methods.isEmpty)
        case Err(error) => fail(error.toString)
      }

      JavaMemberResolver.methods(child, "exactInstance", List(Typed(ClassDesc.of("java.lang.StringBuffer"))), static = true) match {
        case Ok(methods) => assert(methods.isEmpty)
        case Err(error) => fail(error.toString)
      }
    } finally flix.javaTypeProvider.close()
  }

  test("methods.SelectsExpandedVarArgsMatch") {
    implicit val flix: Flix = new Flix
    try {
      val owner = ClassDesc.of("java.util.Arrays")
      val arguments = List(Typed(CD_String), Typed(CD_String))
      JavaMemberResolver.methods(owner, "asList", arguments, static = true) match {
        case Ok(methods) =>
          assert(methods.map(_.ref.owner) == List(owner))
          assert(methods.map(_.ref.descriptor) == List(MethodTypeDesc.ofDescriptor("([Ljava/lang/Object;)Ljava/util/List;")))
        case Err(error) => fail(error.toString)
      }
    } finally flix.javaTypeProvider.close()
  }

  test("methods.ReportsMissingClass") {
    implicit val flix: Flix = new Flix
    try {
      val missing = ClassDesc.of("dev.flix.prototype.DoesNotExist")
      assert(JavaMemberResolver.methods(missing, "method", Nil, static = false) == Err(MissingClass(missing)))
    } finally flix.javaTypeProvider.close()
  }

}
