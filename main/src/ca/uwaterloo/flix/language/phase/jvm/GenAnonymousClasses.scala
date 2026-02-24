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
import ca.uwaterloo.flix.language.ast.{AtomicOp, SimpleType}
import ca.uwaterloo.flix.language.ast.JvmAst.*
import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions.*
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.{IsFinal, NotFinal}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Volatility.NotVolatile
import ca.uwaterloo.flix.language.phase.jvm.JvmName.{MethodDescriptor, RootPackage}
import ca.uwaterloo.flix.util.InternalCompilerException
import org.objectweb.asm.{MethodVisitor, Opcodes}

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

    // Generate constructor: if user-defined constructors exist, invoke the first one; otherwise default no-arg super().
    // Safety guarantees there is at most one constructor.
    if (obj.constructors.nonEmpty) {
      val c = obj.constructors.head
      c.exp match {
        case Expr.ApplyAtomic(AtomicOp.InvokeSuperConstructor(constructor), _, _, _, _) =>
          // Super-only: no closure field needed, parameterized <init>
          val argTypes = constructor.getParameterTypes.toList.map(javaClassToBackendType)
          cm.mkConstructor(ClassMaker.ConstructorMethod(className, argTypes), IsPublic, constructorInsWithSuperCall(superClass, constructor)(_))
        case _ => throw InternalCompilerException(s"Unexpected non-super constructor body.", c.loc)
      }
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

    // Generate bridge methods for super method calls.
    val superMethods = collectSuperMethods(obj.name, root)
    for (method <- superMethods) {
      val bridgeName = s"super$$${method.getName}"
      val paramTypes = method.getParameterTypes.toList.map(javaClassToBackendType)
      val returnTpe = if (method.getReturnType == java.lang.Void.TYPE) VoidableType.Void else javaClassToBackendType(method.getReturnType)
      val descriptor = MethodDescriptor(paramTypes, returnTpe)
      cm.mkMethod(ClassMaker.InstanceMethod(className, bridgeName, descriptor), IsPublic, NotFinal, superBridgeIns(superClass, method)(_))
    }

    cm.closeClassMaker()
  }

  private def constructorIns(superClass: JvmName)(implicit mv: MethodVisitor): Unit = {
    import BytecodeInstructions.*
    ALOAD(0)
    INVOKESPECIAL(ClassMaker.ConstructorMethod(superClass, Nil))
    RETURN()
  }

  /** Creates constructor bytecode that forwards parameters directly to the super constructor. */
  private def constructorInsWithSuperCall(superClass: JvmName, constructor: java.lang.reflect.Constructor[?])(implicit mv: MethodVisitor): Unit = {
    import BytecodeInstructions.*
    val paramTypes = constructor.getParameterTypes.toList.map(javaClassToBackendType)
    // ALOAD 0 (this)
    thisLoad()
    // Load each <init> parameter (starting at slot 1)
    withNames(1, paramTypes) { case (_, args) =>
      for (arg <- args) arg.load()
    }
    // INVOKESPECIAL superClass.<init>(paramTypes...)
    INVOKESPECIAL(ClassMaker.ConstructorMethod(superClass, paramTypes))
    RETURN()
  }

  /** Maps a Java `Class[?]` to a `BackendType`. */
  private def javaClassToBackendType(clazz: Class[?]): BackendType = {
    if      (clazz == java.lang.Boolean.TYPE)   BackendType.Bool
    else if (clazz == java.lang.Byte.TYPE)      BackendType.Int8
    else if (clazz == java.lang.Short.TYPE)     BackendType.Int16
    else if (clazz == java.lang.Integer.TYPE)   BackendType.Int32
    else if (clazz == java.lang.Long.TYPE)      BackendType.Int64
    else if (clazz == java.lang.Float.TYPE)     BackendType.Float32
    else if (clazz == java.lang.Double.TYPE)    BackendType.Float64
    else if (clazz == java.lang.Character.TYPE) BackendType.Char
    else BackendType.Reference(BackendObjType.Native(JvmName.ofClass(clazz)))
  }

  /** Returns the erased abstract arrow class for the given parameter types and return type. */
  private def erasedArrowType(paramTypes: List[SimpleType], retTpe: SimpleType): BackendObjType.AbstractArrow = {
    val boxedResult = BackendType.Object
    BackendObjType.AbstractArrow(paramTypes.map(BackendType.toErasedBackendType), boxedResult)
  }

  /** Collects all unique `java.lang.reflect.Method`s referenced by `InvokeSuperMethod` ops targeting the given className. */
  private def collectSuperMethods(className: String, root: Root): List[java.lang.reflect.Method] = {
    val methods = scala.collection.mutable.LinkedHashSet.empty[java.lang.reflect.Method]
    def scanExpr(expr: Expr): Unit = expr match {
      case Expr.ApplyAtomic(AtomicOp.InvokeSuperMethod(method, cn), exps, _, _, _) if cn == className =>
        methods += method
        exps.foreach(scanExpr)
      case Expr.ApplyAtomic(_, exps, _, _, _) => exps.foreach(scanExpr)
      case Expr.ApplyClo(e1, e2, _, _, _, _) => scanExpr(e1); scanExpr(e2)
      case Expr.ApplyDef(_, exps, _, _, _, _) => exps.foreach(scanExpr)
      case Expr.ApplyOp(_, exps, _, _, _) => exps.foreach(scanExpr)
      case Expr.ApplySelfTail(_, exps, _, _, _) => exps.foreach(scanExpr)
      case Expr.IfThenElse(e1, e2, e3, _, _, _) => scanExpr(e1); scanExpr(e2); scanExpr(e3)
      case Expr.Branch(e, branches, _, _, _) => scanExpr(e); branches.values.foreach(scanExpr)
      case Expr.Let(_, _, e1, e2, _) => scanExpr(e1); scanExpr(e2)
      case Expr.Stmt(e1, e2, _) => scanExpr(e1); scanExpr(e2)
      case Expr.Region(_, _, e, _, _, _) => scanExpr(e)
      case Expr.TryCatch(e, rules, _, _, _) => scanExpr(e); rules.foreach(r => scanExpr(r.exp))
      case Expr.RunWith(e, _, rules, _, _, _, _) => scanExpr(e); rules.foreach(r => scanExpr(r.exp))
      case Expr.NewObject(_, _, _, _, cs, ms, _) =>
        cs.foreach(c => scanExpr(c.exp))
        ms.foreach(m => scanExpr(m.exp))
      case _: Expr.Cst | _: Expr.Var | _: Expr.JumpTo => ()
    }
    // Scan all definitions for InvokeSuperMethod targeting this class
    for ((_, defn) <- root.defs) {
      scanExpr(defn.expr)
    }
    methods.toList
  }

  /** Generates bytecode for a bridge method that calls `INVOKESPECIAL superClass.methodName`. */
  private def superBridgeIns(superClass: JvmName, method: java.lang.reflect.Method)(implicit mv: MethodVisitor): Unit = {
    val paramTypes = method.getParameterTypes.toList.map(javaClassToBackendType)
    val returnTpe = javaClassToBackendType(method.getReturnType)
    val descriptor = MethodDescriptor(paramTypes, if (method.getReturnType == java.lang.Void.TYPE) VoidableType.Void else returnTpe)

    // ALOAD 0 (this)
    thisLoad()
    // Load each parameter (starting at slot 1)
    withNames(1, paramTypes) { case (_, args) =>
      for (arg <- args) arg.load()
    }
    // INVOKESPECIAL superClass.methodName(descriptor)
    INVOKESPECIAL(superClass, method.getName, descriptor)

    // Return
    if (method.getReturnType == java.lang.Void.TYPE) {
      RETURN()
    } else {
      xReturn(returnTpe)
    }
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
