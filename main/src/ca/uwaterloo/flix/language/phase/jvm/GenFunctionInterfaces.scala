/*
 * Copyright 2017 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.ExecutableAst.Root
import ca.uwaterloo.flix.language.ast.Type

/**
  * Generates bytecode for the function interfaces.
  */
object GenFunctionInterfaces {

  /**
    * Returns the set of function interfaces for the given set of types `ts`.
    */
  def gen(ts: Set[Type])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    //
    // Generate a function interface for each type and collect the results in a map.
    //
    ts.foldLeft(Map.empty[JvmName, JvmClass]) {
      case (macc, tpe) => genFunctionalInterface(tpe) match {
        case None => macc
        case Some(clazz) => macc + (clazz.name -> clazz)
      }
    }
  }

  /**
    * Optionally returns the function interface of the given type `tpe`.
    *
    * Returns `[[None]]` if the type is not a function type.
    */
  private def genFunctionalInterface(tpe: Type)(implicit root: Root, flix: Flix): Option[JvmClass] = {
    // Compute the type constructor and type arguments.
    val base = tpe.typeConstructor
    val args = tpe.typeArguments

    // Immediately return None if the type is a non-function type.
    if (!base.isArrow) {
      return None
    }

    // The function arity is simply the number of type arguments.
    val arity = args.length

    // The name of the functional interface is of the form:
    // Fn1$Int$Bool,
    // Fn2$Int$Int$Bool,
    // Fn3$Obj$Obj$Obj$Bool, etc.
    // TODO: Probably use a helper function to compute this?
    val name = "Fn" + arity + "$"

    for (arg <- args) {
      // do something with the arguments ...

    }

    None // TODO
  }


}
