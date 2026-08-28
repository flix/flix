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

import ca.uwaterloo.flix.util.Result.{Err, Ok}
import org.scalatest.funsuite.AnyFunSuite

import java.lang.constant.ClassDesc

class TestJavaMemberResolver extends AnyFunSuite {

  test("field.DeclaredInstanceField") {
    val provider = ByteBuddyJavaTypeProvider.fromClassLoader(getClass.getClassLoader)
    val resolver = JavaMemberResolver(provider)
    try {
      val child = ClassDesc.of("dev.flix.test.TestClassWithFields$Child")
      resolver.field(child, "declaredField", static = false) match {
        case Ok(Some(field)) =>
          val hasExpectedOwner = field.ref.owner == child
          val hasExpectedName = field.ref.name == "declaredField"
          val hasExpectedDescriptor = field.ref.descriptor == ClassDesc.ofDescriptor("I")
          assert(hasExpectedOwner)
          assert(hasExpectedName)
          assert(hasExpectedDescriptor)
        case Ok(None) => fail("Expected declared field.")
        case Err(error) => fail(error.toString)
      }
    } finally provider.close()
  }

  test("field.InheritedSuperclassField") {
    val provider = ByteBuddyJavaTypeProvider.fromClassLoader(getClass.getClassLoader)
    val resolver = JavaMemberResolver(provider)
    try {
      val child = ClassDesc.of("dev.flix.test.TestClassWithFields$Child")
      resolver.field(child, "inheritedField", static = false) match {
        case Ok(Some(field)) =>
          val hasExpectedOwner = field.ref.owner == ClassDesc.of("dev.flix.test.TestClassWithFields$Parent")
          val hasExpectedName = field.ref.name == "inheritedField"
          val hasExpectedDescriptor = field.ref.descriptor == ClassDesc.ofDescriptor("I")
          assert(hasExpectedOwner)
          assert(hasExpectedName)
          assert(hasExpectedDescriptor)
        case Ok(None) => fail("Expected inherited superclass field.")
        case Err(error) => fail(error.toString)
      }
    } finally provider.close()
  }

  test("field.InheritedInterfaceField") {
    val provider = ByteBuddyJavaTypeProvider.fromClassLoader(getClass.getClassLoader)
    val resolver = JavaMemberResolver(provider)
    try {
      val child = ClassDesc.of("dev.flix.test.TestClassWithFields$Child")
      resolver.field(child, "interfaceField", static = true) match {
        case Ok(Some(field)) =>
          val hasExpectedOwner = field.ref.owner == ClassDesc.of("dev.flix.test.TestClassWithFields$ParentInterface")
          val hasExpectedName = field.ref.name == "interfaceField"
          val hasExpectedDescriptor = field.ref.descriptor == ClassDesc.ofDescriptor("I")
          assert(hasExpectedOwner)
          assert(hasExpectedName)
          assert(hasExpectedDescriptor)
        case Ok(None) => fail("Expected inherited interface field.")
        case Err(error) => fail(error.toString)
      }
    } finally provider.close()
  }

  test("field.ReportsMissingField") {
    val provider = ByteBuddyJavaTypeProvider.fromClassLoader(getClass.getClassLoader)
    val resolver = JavaMemberResolver(provider)
    try {
      val child = ClassDesc.of("dev.flix.test.TestClassWithFields$Child")
      resolver.field(child, "missingField", static = false) match {
        case Ok(field) => assert(field.isEmpty)
        case Err(error) => fail(error.toString)
      }
    } finally provider.close()
  }

  test("field.ReportsInaccessibleField") {
    val provider = ByteBuddyJavaTypeProvider.fromClassLoader(getClass.getClassLoader)
    val resolver = JavaMemberResolver(provider)
    try {
      val child = ClassDesc.of("dev.flix.test.TestClassWithFields$Child")
      resolver.field(child, "privateField", static = false) match {
        case Ok(field) => assert(field.isEmpty)
        case Err(error) => fail(error.toString)
      }
    } finally provider.close()
  }

  test("field.ReportsStaticMismatch") {
    val provider = ByteBuddyJavaTypeProvider.fromClassLoader(getClass.getClassLoader)
    val resolver = JavaMemberResolver(provider)
    try {
      val child = ClassDesc.of("dev.flix.test.TestClassWithFields$Child")
      resolver.field(child, "declaredField", static = true) match {
        case Ok(field) => assert(field.isEmpty)
        case Err(error) => fail(error.toString)
      }
    } finally provider.close()
  }

  test("field.DoesNotFallBackAfterStaticMismatch") {
    val provider = ByteBuddyJavaTypeProvider.fromClassLoader(getClass.getClassLoader)
    val resolver = JavaMemberResolver(provider)
    try {
      val child = ClassDesc.of("dev.flix.test.TestClassWithFields$Child")
      resolver.field(child, "hiddenField", static = true) match {
        case Ok(field) => assert(field.isEmpty)
        case Err(error) => fail(error.toString)
      }
    } finally provider.close()
  }

}
