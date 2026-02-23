/*
 * Copyright 2022 Paul Butcher
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

package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.SimpleType
import ca.uwaterloo.flix.language.ast.JvmAst.*
import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions.*
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.{IsFinal, NotFinal}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Volatility.NotVolatile
import ca.uwaterloo.flix.language.phase.jvm.JvmName.{MethodDescriptor, RootPackage}
import org.objectweb.asm.MethodVisitor

/** Generates bytecode for anonymous classes (created through NewObject). */
object GenAnonymousClasses {

  /** Returns the generated classes of `objs`. */
  def gen(objs: List[AnonClass])(implicit root: Root, flix: Flix): List[JvmClass] = {
    for (obj <- objs) yield {
      val className = JvmName(RootPackage, obj.name)
      JvmClass(className, genByteCode(className, obj))
    }
  }

  private def genByteCode(className: JvmName, obj: AnonClass)(implicit root: Root, flix: Flix): Array[Byte] = {
    val superClass = if (obj.clazz.isInterface)
      JvmName.Object
    else
      JvmName.ofClass(obj.clazz)

    val interfaces = if (obj.clazz.isInterface)
      List(JvmName.ofClass(obj.clazz))
    else
      Nil

    val cm = ClassMaker.mkClass(className, IsFinal, superClass = superClass, interfaces = interfaces)

    // Create fields for constructor closures.
    val cnsFields = obj.constructors.zipWithIndex.map { case (c, i) =>
      val abstractClass = erasedArrowType(c.fparams.map(_.tpe), c.tpe)
      val cnsField = ClassMaker.InstanceField(className, s"cns$i", abstractClass.toTpe)
      cm.mkField(cnsField, IsPublic, NotFinal, NotVolatile)
      (c, cnsField, abstractClass)
    }

    // Generate constructor: if user-defined constructors exist, invoke the first one; otherwise default no-arg super().
    if (obj.constructors.nonEmpty) {
      val (c, cnsField, abstractClass) = cnsFields.head
      cm.mkConstructor(ClassMaker.ConstructorMethod(className, Nil), IsPublic, constructorInsWithClosure(superClass, c, cnsField, abstractClass)(_, root))
    } else {
      cm.mkConstructor(ClassMaker.ConstructorMethod(className, Nil), IsPublic, constructorIns(superClass)(_))
    }

    for ((m, i) <- obj.methods.zipWithIndex) {
      val abstractClass = erasedArrowType(m.fparams.map(_.tpe), m.tpe)
      // Create the field that will store the closure implementing the body of the method.
      val cloField = ClassMaker.InstanceField(className, s"clo$i", abstractClass.toTpe)
      cm.mkField(cloField, IsPublic, NotFinal, NotVolatile)
      // Drop the first formal parameter (which always represents `this`).
      val actualArgs = m.fparams.tail.map(_.tpe).map(BackendType.toBackendType)
      val actualres = if (m.tpe == SimpleType.Unit) VoidableType.Void else BackendType.toBackendType(m.tpe)
      cm.mkMethod(ClassMaker.InstanceMethod(className, m.ident.name, MethodDescriptor(actualArgs, actualres)), IsPublic, NotFinal, methodIns(abstractClass, cloField, m)(_, root))
    }

    cm.closeClassMaker()
  }

  private def constructorIns(superClass: JvmName)(implicit mv: MethodVisitor): Unit = {
    import BytecodeInstructions.*
    ALOAD(0)
    INVOKESPECIAL(ClassMaker.ConstructorMethod(superClass, Nil))
    RETURN()
  }

  /** Creates constructor bytecode that invokes the user-defined constructor closure, which will call super(...). */
  private def constructorInsWithClosure(superClass: JvmName, c: JvmConstructor, cnsField: ClassMaker.InstanceField, abstractClass: BackendObjType.AbstractArrow)(implicit mv: MethodVisitor, root: Root): Unit = {
    val functionAbstractClass = abstractClass.superClass

    // Load `this` and get the closure field
    thisLoad()
    GETFIELD(cnsField)
    INVOKEVIRTUAL(abstractClass.GetUniqueThreadClosureMethod)

    // Load `this` as first arg into arg0 of the closure
    withNames(0, c.fparams.map(_.tpe).map(BackendType.toBackendType)) {
      case (_, args) =>
        for ((arg, i) <- args.zipWithIndex) {
          DUP()
          arg.load()
          PUTFIELD(functionAbstractClass.ArgField(i))
        }
    }

    // Invoke the closure (which should call super(...) internally)
    val returnType = BackendType.toBackendType(c.tpe)
    BackendObjType.Result.unwindSuspensionFreeThunkToType(returnType, s"in anonymous class constructor", c.loc)

    // Pop the result (constructor returns void)
    POP()
    RETURN()
  }

  /** Returns the erased abstract arrow class for the given parameter types and return type. */
  private def erasedArrowType(paramTypes: List[SimpleType], retTpe: SimpleType): BackendObjType.AbstractArrow = {
    val boxedResult = BackendType.Object
    BackendObjType.AbstractArrow(paramTypes.map(BackendType.toErasedBackendType), boxedResult)
  }

  /** Creates code to read the arguments, load it into the `cloField` closure, call that function, and returns. */
  private def methodIns(abstractClass: BackendObjType.AbstractArrow, cloField: ClassMaker.InstanceField, m: JvmMethod)(implicit mv: MethodVisitor, root: Root): Unit = {
    val functionAbstractClass = abstractClass.superClass
    val returnType = BackendType.toBackendType(m.tpe)

    thisLoad()
    GETFIELD(cloField)
    INVOKEVIRTUAL(abstractClass.GetUniqueThreadClosureMethod)
    // Load the actual arguments into the erased closure arguments.
    withNames(0, m.fparams.map(_.tpe).map(BackendType.toBackendType)) {
      case (_, args) =>
        for ((arg, i) <- args.zipWithIndex) {
          DUP()
          arg.load()
          PUTFIELD(functionAbstractClass.ArgField(i))
        }
    }
    // Invoke the closure.
    BackendObjType.Result.unwindSuspensionFreeThunkToType(returnType, s"in anonymous class method ${m.ident.name}", m.loc)

    m.tpe match {
      case SimpleType.Unit => RETURN()
      case _ => xReturn(returnType)
    }
  }

}
