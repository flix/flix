/*
 * Copyright 2021 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.phase.jvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.{IsFinal, NotFinal}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethodName, InstanceField, StaticMethod}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{RootPackage, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.MethodTypeDescs.mkVoidDescriptor
import ca.uwaterloo.flix.language.phase.jvm.{BackendObjType, ClassMaker, GenFunAndClosureClasses, JavaClasses, MethodTypeDescs}
import ca.uwaterloo.flix.util.ClassDescs
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc
import java.lang.constant.ConstantDescs.CD_Object

/**
  * The `Main` class, whose `main` method is the entry point of a compiled Flix program.
  */
object GenMain {

  val desc: ClassDesc = mkDesc(RootPackage, "Main")

  def genByteCode(sym: Symbol.DefnSym)(implicit flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkClass(this.desc, IsFinal)

    cm.mkStaticMethod(MainMethod, IsPublic, NotFinal, mainIns(sym)(_))

    cm.closeClassMaker()
  }

  def MainMethod: StaticMethod = StaticMethod(this.desc, "main", mkVoidDescriptor(JavaClasses.String.arrayType()))

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
      GenResult.unwindSuspensionFreeThunk(s"in ${ClassDescs.binaryNameOf(desc)}", SourceLocation.Unknown)
      POP()
      RETURN()
    })
  }

}
