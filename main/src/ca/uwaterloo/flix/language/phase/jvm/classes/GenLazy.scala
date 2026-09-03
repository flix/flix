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
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.jvm.JavaClasses
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.{IsFinal, NotFinal}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.{IsPrivate, IsPublic}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Volatility.{IsVolatile, NotVolatile}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethod, InstanceField, InstanceMethod}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{RootPackage, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.MethodTypeDescs.mkDescriptor
import ca.uwaterloo.flix.language.phase.jvm.{ClassConstants, ClassMaker, Mangle}
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc
import java.lang.constant.ConstantDescs.CD_Object

/** The class of a Flix lazy value: an unevaluated thunk, the forced value, and a lock. */
object GenLazy {

  def desc(tpe: ClassDesc): ClassDesc =
    mkDesc(RootPackage, Mangle.mkClassName("Lazy", Mangle.erasedName(tpe)))


  def genByteCode(tpe: ClassDesc)(implicit flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkClass(desc(tpe), IsFinal)

    cm.mkConstructor(Constructor(tpe), IsPublic, constructorIns(tpe)(_))
    cm.mkField(ExpField(tpe), IsPublic, NotFinal, IsVolatile)
    cm.mkField(ValueField(tpe), IsPublic, NotFinal, NotVolatile)
    cm.mkField(LockField(tpe), IsPrivate, NotFinal, NotVolatile)
    cm.mkMethod(Nil, ForceMethod(tpe), IsPublic, IsFinal, forceIns(tpe)(_))

    cm.closeClassMaker()
  }

  def ExpField(tpe: ClassDesc): InstanceField = InstanceField(desc(tpe), "expression", CD_Object)

  def ValueField(tpe: ClassDesc): InstanceField = InstanceField(desc(tpe), "value", tpe)

  private def LockField(tpe: ClassDesc): InstanceField = InstanceField(desc(tpe), "lock", JavaClasses.ReentrantLock)

  def Constructor(tpe: ClassDesc): ConstructorMethod = ConstructorMethod(desc(tpe), List(CD_Object))

  /** `[] --> return` */
  private def constructorIns(tpe: ClassDesc)(implicit mv: MethodVisitor): Unit =
    withName(1, CD_Object)(exp => {
      // super()
      thisLoad()
      INVOKESPECIAL(ClassConstants.Object.Constructor)
      // this.exp = exp
      thisLoad()
      exp.load()
      PUTFIELD(ExpField(tpe))
      // this.lock = new ReentrantLock()
      thisLoad()
      NEW(JavaClasses.ReentrantLock)
      DUP()
      INVOKESPECIAL(ClassConstants.ReentrantLock.Constructor)
      PUTFIELD(LockField(tpe))
      // return
      RETURN()
    })

  def ForceMethod(tpe: ClassDesc): InstanceMethod = InstanceMethod(desc(tpe), "force", mkDescriptor()(tpe))

  /** `[] --> return tpe` */
  private def forceIns(tpe: ClassDesc)(implicit mv: MethodVisitor): Unit = {
    def unlockLock(): Unit = {
      thisLoad()
      GETFIELD(LockField(tpe))
      INVOKEVIRTUAL(ClassConstants.ReentrantLock.UnlockMethod)
    }

    thisLoad()
    GETFIELD(LockField(tpe))
    INVOKEVIRTUAL(ClassConstants.ReentrantLock.LockInterruptiblyMethod)
    tryCatch {
      thisLoad()
      GETFIELD(ExpField(tpe))
      // if the expression is not null, compute the value and erase the expression
      ifCondition(Condition.NONNULL) {
        thisLoad()
        // get expression as thunk
        DUP()
        GETFIELD(ExpField(tpe))
        CHECKCAST(GenThunk.Desc)
        // this.value = thunk.unwind()
        GenResult.unwindSuspensionFreeThunkToType(tpe, "during call to Lazy.force", SourceLocation.Unknown)
        PUTFIELD(ValueField(tpe))
        // this.exp = null
        thisLoad()
        pushNull()
        PUTFIELD(ExpField(tpe))
      }
      thisLoad()
      GETFIELD(ValueField(tpe))
    } {
      // catch
      unlockLock()
      ATHROW()
    }
    unlockLock()
    xReturn(tpe)
  }

}
