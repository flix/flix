/*
 * Copyright 2021 Jonathan Lindegaard Starup
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.phase.jvm.classes

import ca.uwaterloo.flix.api.{CompilerConstants, Flix}
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol}
import ca.uwaterloo.flix.language.jvm.{ClassDescs, JavaClasses}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.{IsFinal, NotFinal}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethodName, InstanceField, StaticMethod}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{RootPackage, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.MethodTypeDescs.mkVoidDescriptor
import ca.uwaterloo.flix.language.phase.jvm.{ClassMaker, GenFunAndClosureClasses, MethodTypeDescs}
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc
import java.lang.constant.ConstantDescs.CD_Object

/**
  * The `Main` class, whose `main` method is the entry point of a compiled Flix program.
  */
object GenMain {

  /** The JVM class descriptor for the generated `Main` class. */
  val Desc: ClassDesc = mkDesc(RootPackage, CompilerConstants.EntryPointClassName)

  def genByteCode(sym: Symbol.DefnSym)(implicit flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkClass(this.Desc, IsFinal)

    cm.mkStaticMethod(MainMethod, IsPublic, NotFinal, mainIns(sym)(_))

    cm.closeClassMaker()
  }

  def MainMethod: StaticMethod = StaticMethod(this.Desc, "main", mkVoidDescriptor(JavaClasses.String.arrayType()))

  private def mainIns(sym: Symbol.DefnSym)(implicit mv: MethodVisitor): Unit = {
    val defName = GenFunAndClosureClasses.defnDesc(sym)
    withName(0, JavaClasses.String.arrayType())(args => {
      args.load()
      INVOKESTATIC(GenGlobal.SetArgsMethod)
      NEW(defName)
      DUP()
      INVOKESPECIAL(defName, ConstructorMethodName, MethodTypeDescs.NothingToVoid)
      DUP()
      GETSTATIC(GenUnit.SingletonField)
      PUTFIELD(InstanceField(defName, "arg0", CD_Object))
      GenResult.unwindSuspensionFreeThunk(s"in ${ClassDescs.binaryNameOf(Desc)}", SourceLocation.Unknown)
      POP()
      RETURN()
    })
  }

}
