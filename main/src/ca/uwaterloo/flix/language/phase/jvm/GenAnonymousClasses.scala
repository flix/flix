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
import ca.uwaterloo.flix.language.ast.shared.{JConstructor, JMethod}
import ca.uwaterloo.flix.language.ast.{AtomicOp, SimpleType}
import ca.uwaterloo.flix.language.ast.JvmAst.*
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.classes.GenResult
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.{IsFinal, NotFinal}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Volatility.NotVolatile
import ca.uwaterloo.flix.util.InternalCompilerException
import org.objectweb.asm.{MethodVisitor, Opcodes}

import java.lang.constant.{ClassDesc, MethodTypeDesc}
import java.lang.constant.ConstantDescs.{CD_Object, CD_void}
import scala.jdk.CollectionConverters.*

/** Generates bytecode for anonymous classes (created through NewObject). */
object GenAnonymousClasses {

  /** Returns the generated classes of `objs`. */
  def gen(objs: List[AnonClass])(implicit root: Root, flix: Flix): List[JvmClass] = {
    for (obj <- objs) yield {
      val className = ClassDesc.ofInternalName(obj.name)
      JvmClass(className, genByteCode(className, obj))
    }
  }

  private def genByteCode(className: ClassDesc, obj: AnonClass)(implicit root: Root, flix: Flix): Array[Byte] = {
    val superClass = if (obj.clazz.isInterface)
      JavaClasses.Object
    else
      obj.clazz.desc

    val interfaces = if (obj.clazz.isInterface)
      List(obj.clazz.desc)
    else
      Nil

    val cm = ClassMaker.mkClass(className, IsFinal, superClass = superClass, interfaces = interfaces)

    // Generate constructor: if user-defined constructors exist, invoke the first one; otherwise default no-arg super().
    // Safety guarantees there is at most one constructor.
    if (obj.constructors.nonEmpty) {
      val c = obj.constructors.head
      c.exp match {
        case Expr.ApplyAtomic(AtomicOp.InvokeSuperConstructor(constructor), _, _, _, _) =>
          // Super-only: no closure field needed, parameterized <init>
          val argTypes = constructor.descriptor.parameterList.asScala.toList
          cm.mkConstructor(ClassMaker.ConstructorMethod(className, argTypes), IsPublic, constructorInsWithSuperCall(superClass, constructor)(_))
        case _ => throw InternalCompilerException(s"Unexpected non-super constructor body.", c.loc)
      }
    } else {
      cm.mkConstructor(ClassMaker.ConstructorMethod(className, Nil), IsPublic, constructorIns(superClass)(_))
    }

    for ((m, i) <- obj.methods.zipWithIndex) {
      val abstractClass = erasedArrowType(m.fparams.map(_.tpe), m.tpe)
      // Create the field that will store the closure implementing the body of the method.
      val cloField = ClassMaker.InstanceField(className, s"clo$i", abstractClass.desc)
      cm.mkField(cloField, IsPublic, NotFinal, NotVolatile)
      // Use the Java interface's erased method signature (resolved during lowering) for the
      // JVM descriptor. This ensures the generated method matches the interface even when the
      // user declares generic parameter types (e.g., String instead of Object).
      val descriptor = m.javaSig match {
        case Some(jm) => jm.descriptor
        case None =>
          val ret = if (m.tpe == SimpleType.Unit) CD_void else TypeDescs.toClassDesc(m.tpe)
          MethodTypeDesc.of(ret, m.fparams.tail.map(fp => TypeDescs.toClassDesc(fp.tpe)) *)
      }
      cm.mkMethod(m.ann, ClassMaker.InstanceMethod(className, m.ident.name, descriptor), IsPublic, NotFinal, methodIns(abstractClass, cloField, descriptor.returnType(), m)(_, root))
    }

    // Generate bridge methods for super method calls.
    val superMethods = obj.superMethods
    for (method <- superMethods) {
      val bridgeName = s"super$$${method.name}"
      cm.mkMethod(Nil, ClassMaker.InstanceMethod(className, bridgeName, method.descriptor), IsPublic, NotFinal, superBridgeIns(superClass, method)(_))
    }

    cm.closeClassMaker()
  }

  private def constructorIns(superClass: ClassDesc)(implicit mv: MethodVisitor): Unit = {
    ALOAD(0)
    INVOKESPECIAL(ClassMaker.ConstructorMethod(superClass, Nil))
    RETURN()
  }

  /** Creates constructor bytecode that forwards parameters directly to the super constructor. */
  private def constructorInsWithSuperCall(superClass: ClassDesc, constructor: JConstructor)(implicit mv: MethodVisitor): Unit = {
    // ALOAD 0 (this)
    thisLoad()
    // Load each <init> parameter (starting at slot 1)
    withNames(1, constructor.descriptor.parameterList.asScala.toList) { case (_, args) =>
      for (arg <- args) arg.load()
    }
    // INVOKESPECIAL superClass.<init>(paramTypes...)
    INVOKESPECIAL(superClass, ClassMaker.ConstructorMethodName, constructor.descriptor)
    RETURN()
  }

  /** Returns the erased abstract arrow class for the given parameter types and return type. */
  private def erasedArrowType(paramTypes: List[SimpleType], retTpe: SimpleType): BackendObjType.AbstractArrow = {
    val boxedResult = CD_Object
    BackendObjType.AbstractArrow(paramTypes.map(TypeDescs.toErasedClassDesc), boxedResult)
  }

  /**
    * Generates bytecode for a bridge method that delegates to the superclass via `INVOKESPECIAL`.
    *
    * This is needed because Flix closures run in a separate class from the anonymous class, so they
    * cannot issue `INVOKESPECIAL` on the anonymous class's superclass — the JVM restricts that
    * instruction to the class that owns the method. We work around this by generating a public
    * bridge method on the anonymous class itself. The closure calls the bridge via `INVOKEVIRTUAL`,
    * and the bridge forwards to the superclass via `INVOKESPECIAL`.
    *
    * For example, given `new Object { def hashCode(_this: ...) = super.hashCode() }`, we generate:
    * {{{
    *   public int super$hashCode() {
    *       ALOAD 0
    *       INVOKESPECIAL java/lang/Object.hashCode ()I
    *       IRETURN
    *   }
    * }}}
    */
  private def superBridgeIns(superClass: ClassDesc, method: JMethod)(implicit mv: MethodVisitor): Unit = {
    val isVoid = method.descriptor.returnType() == CD_void

    // ALOAD 0 (this)
    thisLoad()
    // Load each parameter (starting at slot 1)
    withNames(1, method.descriptor.parameterList.asScala.toList) { case (_, args) =>
      for (arg <- args) arg.load()
    }
    // INVOKESPECIAL superClass.methodName(descriptor)
    INVOKESPECIAL(superClass, method.name, method.descriptor)

    // Return
    if (isVoid) {
      RETURN()
    } else {
      xReturn(method.descriptor.returnType())
    }
  }

  /** Creates code to read the arguments, load it into the `cloField` closure, call that function, and returns. */
  private def methodIns(abstractClass: BackendObjType.AbstractArrow, cloField: ClassMaker.InstanceField, actualRes: ClassDesc, m: JvmMethod)(implicit mv: MethodVisitor, root: Root): Unit = {
    val functionAbstractClass = abstractClass.superClass
    val returnType = TypeDescs.toClassDesc(m.tpe)

    thisLoad()
    GETFIELD(cloField)
    INVOKEVIRTUAL(abstractClass.GetUniqueThreadClosureMethod)
    // Load the actual arguments into the erased closure arguments.
    withNames(0, m.fparams.map(_.tpe).map(TypeDescs.toClassDesc)) {
      case (_, args) =>
        for ((arg, i) <- args.zipWithIndex) {
          DUP()
          arg.load()
          PUTFIELD(functionAbstractClass.ArgField(i))
        }
    }
    // Invoke the closure, leaving its result on the stack in the representation of `m.tpe`.
    GenResult.unwindSuspensionFreeThunkToType(returnType, s"in anonymous class method ${m.ident.name}", m.loc)

    // Return the value using the method's erased JVM return type (`actualRes`). Any boxing
    // needed to feed a primitive result into a reference (e.g. `Object`) return has already
    // been applied in Lowering, so the value on the stack already matches `actualRes`.
    if (actualRes == CD_void) {
      RETURN()
    } else {
      xReturn(actualRes)
    }
  }

}
