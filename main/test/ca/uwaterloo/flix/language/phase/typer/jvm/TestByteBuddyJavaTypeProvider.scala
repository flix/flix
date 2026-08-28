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

import ca.uwaterloo.flix.language.ast.jvm.JavaType.{Parameterized, Variable}
import ca.uwaterloo.flix.language.ast.jvm.JavaTypeVariable
import ca.uwaterloo.flix.language.ast.jvm.JavaTypeVariableOwner.Class
import ca.uwaterloo.flix.language.phase.typer.jvm.JavaLookupError.{InvalidClass, MissingClass, UnsupportedDescriptor}
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import net.bytebuddy.dynamic.ClassFileLocator
import net.bytebuddy.pool.TypePool
import org.scalatest.funsuite.AnyFunSuite

import java.lang.constant.{ClassDesc, MethodTypeDesc}

class TestByteBuddyJavaTypeProvider extends AnyFunSuite {

  test("lookupClass.PlatformClass.Descriptor") {
    val provider = ByteBuddyJavaTypeProvider.platform()
    try {
      val arrayListDesc = ClassDesc.of("java.util.ArrayList")
      provider.lookupClass(arrayListDesc) match {
        case Ok(arrayList) => assert(arrayList.desc == arrayListDesc)
        case Err(error) => fail(error.toString)
      }
    } finally provider.close()
  }

  test("lookupClass.PlatformClass.TypeParameters") {
    val provider = ByteBuddyJavaTypeProvider.platform()
    try {
      val arrayListDesc = ClassDesc.of("java.util.ArrayList")
      provider.lookupClass(arrayListDesc) match {
        case Ok(arrayList) =>
          assert(arrayList.typeParameters.map(_.variable) == List(JavaTypeVariable(Class(arrayListDesc), "E")))
        case Err(error) => fail(error.toString)
      }
    } finally provider.close()
  }

  test("lookupClass.PlatformClass.GenericMethod") {
    val provider = ByteBuddyJavaTypeProvider.platform()
    try {
      val arrayListDesc = ClassDesc.of("java.util.ArrayList")
      provider.lookupClass(arrayListDesc) match {
        case Ok(arrayList) =>
          val get = arrayList.declaredMethods.find(m =>
            m.ref.name == "get" && m.ref.descriptor == MethodTypeDesc.ofDescriptor("(I)Ljava/lang/Object;"))
          assert(get.exists(_.returnType == Variable(
            JavaTypeVariable(Class(arrayListDesc), "E"),
            ClassDesc.of("java.lang.Object")
          )))
        case Err(error) => fail(error.toString)
      }
    } finally provider.close()
  }

  test("lookupClass.PlatformClass.GenericInterface") {
    val provider = ByteBuddyJavaTypeProvider.platform()
    try {
      val arrayListDesc = ClassDesc.of("java.util.ArrayList")
      provider.lookupClass(arrayListDesc) match {
        case Ok(arrayList) =>
          assert(arrayList.interfaces.exists {
            case Parameterized(desc, List(Variable(variable, _))) =>
              desc == ClassDesc.of("java.util.List") && variable == JavaTypeVariable(Class(arrayListDesc), "E")
            case _ => false
          })
        case Err(error) => fail(error.toString)
      }
    } finally provider.close()
  }

  test("lookupClass.ReportsMissingClass") {
    val provider = ByteBuddyJavaTypeProvider.platform()
    try {
      val missing = ClassDesc.of("dev.flix.prototype.DoesNotExist")
      assert(provider.lookupClass(missing) == Err(MissingClass(missing)))
    } finally provider.close()
  }

  test("lookupClass.ReportsUnsupportedDescriptor") {
    val provider = ByteBuddyJavaTypeProvider.platform()
    try {
      val array = ClassDesc.ofDescriptor("[Ljava/lang/String;")
      assert(provider.lookupClass(array) == Err(UnsupportedDescriptor(array)))
    } finally provider.close()
  }

  test("lookupClass.ReportsInvalidClass") {
    val className = "dev.flix.prototype.Invalid"
    val classDesc = ClassDesc.of(className)
    val classBytes = Array[Byte](
      0xca.toByte, 0xfe.toByte, 0xba.toByte, 0xbe.toByte,
      0x00.toByte, 0x00.toByte, 0x00.toByte, 0x41.toByte,
      0x00.toByte, 0x02.toByte, 0x00.toByte
    )
    val locator = ClassFileLocator.Simple.of(className, classBytes)
    val pool = new TypePool.Default.WithLazyResolution(
      new TypePool.CacheProvider.Simple(),
      locator,
      TypePool.Default.ReaderMode.FAST
    )
    val provider = ByteBuddyJavaTypeProvider(locator, pool)

    try {
      provider.lookupClass(classDesc) match {
        case Err(InvalidClass(`classDesc`, _)) => succeed
        case result => fail(s"Expected InvalidClass, got: $result")
      }
    } finally provider.close()
  }

  test("isSubtype.ClassFileHierarchy.Subtype") {
    val provider = ByteBuddyJavaTypeProvider.platform()
    try {
      provider.isSubtype(ClassDesc.of("java.util.ArrayList"), ClassDesc.of("java.util.List")) match {
        case Ok(arrayListIsList) => assert(arrayListIsList)
        case Err(error) => fail(error.toString)
      }
    } finally provider.close()
  }

  test("isSubtype.ClassFileHierarchy.NotSubtype") {
    val provider = ByteBuddyJavaTypeProvider.platform()
    try {
      provider.isSubtype(ClassDesc.of("java.util.List"), ClassDesc.of("java.util.ArrayList")) match {
        case Ok(listIsArrayList) => assert(!listIsArrayList)
        case Err(error) => fail(error.toString)
      }
    } finally provider.close()
  }

  test("virtualMethods.IncludesInheritedMethods") {
    val provider = ByteBuddyJavaTypeProvider.platform()
    try {
      provider.virtualMethods(ClassDesc.of("java.util.function.UnaryOperator")) match {
        case Ok(methods) =>
          val applyDescriptor = MethodTypeDesc.ofDescriptor("(Ljava/lang/Object;)Ljava/lang/Object;")
          val hasInheritedApply = methods.exists { m =>
            val hasExpectedOwner = m.ref.owner == ClassDesc.of("java.util.function.Function")
            val hasExpectedName = m.ref.name == "apply"
            val hasExpectedDescriptor = m.ref.descriptor == applyDescriptor
            hasExpectedOwner && hasExpectedName && hasExpectedDescriptor
          }
          assert(hasInheritedApply)
        case Err(error) => fail(error.toString)
      }
    } finally provider.close()
  }

  test("virtualMethods.PreservesDeclaredErasureAfterGenericSubstitution") {
    val provider = ByteBuddyJavaTypeProvider.platform()
    try {
      provider.virtualMethods(ClassDesc.of("java.util.concurrent.Delayed")) match {
        case Ok(methods) =>
          val compareToDescriptor = MethodTypeDesc.ofDescriptor("(Ljava/lang/Object;)I")
          val hasDeclaredCompareTo = methods.exists { m =>
            val hasExpectedOwner = m.ref.owner == ClassDesc.of("java.lang.Comparable")
            val hasExpectedName = m.ref.name == "compareTo"
            val hasExpectedDescriptor = m.ref.descriptor == compareToDescriptor
            hasExpectedOwner && hasExpectedName && hasExpectedDescriptor
          }
          assert(hasDeclaredCompareTo)
        case Err(error) => fail(error.toString)
      }
    } finally provider.close()
  }

}
