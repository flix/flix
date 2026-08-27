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

import ca.uwaterloo.flix.language.ast.jvm.JavaType.{NonGeneric, Parameterized, Variable}
import ca.uwaterloo.flix.language.ast.jvm.JavaTypeVariableOwner.Class
import ca.uwaterloo.flix.language.ast.jvm.{JavaMethodRef, JavaTypeParameter, JavaTypeVariable, JavaTypeVariableOwner}
import ca.uwaterloo.flix.language.phase.typer.jvm.JavaLookupError.{MissingClass, UnsupportedDescriptor}
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import net.bytebuddy.dynamic.ClassFileLocator
import org.objectweb.asm.{ClassWriter, Opcodes}
import org.scalatest.funsuite.AnyFunSuite

import java.lang.constant.{ClassDesc, MethodTypeDesc}

class TestByteBuddyJavaTypeProvider extends AnyFunSuite {

  test("lookupClass.PlatformClass") {
    withProvider(ByteBuddyJavaTypeProvider.platform()) { provider =>
      val arrayListDesc = ClassDesc.of("java.util.ArrayList")
      val arrayList = resolve(provider.lookupClass(arrayListDesc))

      assert(arrayList.desc == arrayListDesc)
      assert(arrayList.typeParameters.map(_.variable) ==
        List(JavaTypeVariable(Class(arrayListDesc), "E")))

      val get = arrayList.declaredMethods.find(m =>
        m.ref.name == "get" && m.ref.descriptor == MethodTypeDesc.ofDescriptor("(I)Ljava/lang/Object;"))
      assert(get.exists(_.returnType == Variable(
        JavaTypeVariable(Class(arrayListDesc), "E"),
        ClassDesc.of("java.lang.Object")
      )))

      assert(arrayList.interfaces.exists {
        case Parameterized(desc, List(Variable(variable, _))) =>
          desc == ClassDesc.of("java.util.List") && variable == JavaTypeVariable(Class(arrayListDesc), "E")
        case _ => false
      })
    }
  }

  test("lookupClass.InMemoryClassFile") {
    val className = "dev.flix.prototype.Unloaded"
    val classDesc = ClassDesc.of(className)
    val classBytes = mkGenericInterface(className.replace('.', '/'))
    val locator = new ClassFileLocator.Compound(
      ClassFileLocator.Simple.of(className, classBytes),
      ClassFileLocator.ForClassLoader.ofPlatformLoader()
    )

    withProvider(ByteBuddyJavaTypeProvider.fromLocator(locator)) { provider =>
      val clazz = resolve(provider.lookupClass(classDesc))
      val variable = JavaTypeVariable(Class(classDesc), "T")

      assert(clazz.typeParameters == List(JavaTypeParameter(
        variable,
        List(NonGeneric(ClassDesc.of("java.lang.Object")))
      )))

      val id = clazz.declaredMethods.find(_.ref.name == "id").get
      assert(id.ref == JavaMethodRef(
        classDesc,
        "id",
        MethodTypeDesc.ofDescriptor("(Ljava/lang/Object;)Ljava/lang/Object;")
      ))
      assert(id.parameterTypes == List(Variable(variable, ClassDesc.of("java.lang.Object"))))
      assert(id.returnType == Variable(variable, ClassDesc.of("java.lang.Object")))

      val convert = clazz.declaredMethods.find(_.ref.name == "convert").get
      val methodVariable = JavaTypeVariable(JavaTypeVariableOwner.Method(convert.ref), "U")
      assert(convert.typeParameters.map(_.variable) == List(methodVariable))
      assert(convert.parameterTypes == List(Variable(methodVariable, ClassDesc.of("java.lang.Object"))))
      assert(convert.returnType == Variable(variable, ClassDesc.of("java.lang.Object")))
    }
  }

  test("lookupClass.ReportsMissingAndUnsupportedDescriptors") {
    withProvider(ByteBuddyJavaTypeProvider.platform()) { provider =>
      val missing = ClassDesc.of("dev.flix.prototype.DoesNotExist")
      assert(provider.lookupClass(missing) == Err(MissingClass(missing)))

      val array = ClassDesc.ofDescriptor("[Ljava/lang/String;")
      assert(provider.lookupClass(array) == Err(UnsupportedDescriptor(array)))
    }
  }

  private def mkGenericInterface(internalName: String): Array[Byte] = {
    val writer = new ClassWriter(0)
    writer.visit(
      Opcodes.V21,
      Opcodes.ACC_PUBLIC | Opcodes.ACC_ABSTRACT | Opcodes.ACC_INTERFACE,
      internalName,
      "<T:Ljava/lang/Object;>Ljava/lang/Object;",
      "java/lang/Object",
      null
    )
    writer.visitMethod(
      Opcodes.ACC_PUBLIC | Opcodes.ACC_ABSTRACT,
      "id",
      "(Ljava/lang/Object;)Ljava/lang/Object;",
      "(TT;)TT;",
      null
    ).visitEnd()
    writer.visitMethod(
      Opcodes.ACC_PUBLIC | Opcodes.ACC_ABSTRACT,
      "convert",
      "(Ljava/lang/Object;)Ljava/lang/Object;",
      "<U:Ljava/lang/Object;>(TU;)TT;",
      null
    ).visitEnd()
    writer.visitEnd()
    writer.toByteArray
  }

  private def withProvider[A](provider: ByteBuddyJavaTypeProvider)(f: ByteBuddyJavaTypeProvider => A): A =
    try f(provider)
    finally provider.close()

  private def resolve[A](result: Result[A, JavaLookupError]): A = result match {
    case Ok(value) => value
    case Err(error) => fail(error.toString)
  }

}
