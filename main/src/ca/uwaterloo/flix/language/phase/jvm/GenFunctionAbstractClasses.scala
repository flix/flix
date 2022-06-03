/*
 * Copyright 2017 Magnus Madsen
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

package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ErasedAst.Root
import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions._
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.NotFinal
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor

/**
  * Generates bytecode for the function abstract classes.
  */
object GenFunctionAbstractClasses {

  /**
    * Returns the set of function abstract classes for the given set of types `ts`.
    */
  def gen(arrows: Iterable[BackendObjType.Arrow])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    arrows.foldLeft(Map.empty[JvmName, JvmClass]) {
      case (macc, arrow) =>
        macc + (arrow.jvmName -> JvmClass(arrow.jvmName, genFunctionalInterface(arrow)))
    }
  }

  /**
    * Returns the function abstract class of the given type `arrow`.
    */
  private def genFunctionalInterface(arrow: BackendObjType.Arrow)(implicit root: Root, flix: Flix): Array[Byte] = {
    // (Int, String) -> Bool example:
    // public abstract class Fn2$Int$Obj$Bool extends Cont$Bool implements java.util.function.Function {
    //   public abstract int arg0;
    //   public abstract Object arg1;
    //   public Fn2$Int$Obj$Bool() { ... }
    // }

    // TODO: this or subclasses do not implement Function::apply?

    val cont = arrow.continuation
    val cm = ClassMaker.mkAbstractClass(arrow.jvmName,
      superClass = cont.jvmName,
      interfaces = List(JvmName.Function))

    cm.mkConstructor(genConstructor(cont), MethodDescriptor.NothingToVoid, IsPublic)

    for (argIndex <- arrow.args.indices)
      arrow.ArgField(argIndex).mkField(cm, IsPublic, NotFinal)

    cm.closeClassMaker()
  }

  private def genConstructor(cont: BackendObjType.Continuation): InstructionSet =
    thisLoad() ~
      invokeConstructor(cont.jvmName, MethodDescriptor.NothingToVoid) ~
      RETURN()

}
