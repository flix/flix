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
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.ExecutableAst.{Def, Root}

/**
  * Generates bytecode for the main class.
  */
object GenMain {

  /**
    * Returns the main class.
    */
  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = getMain(root) match {
    case None => Map.empty
    case Some(defn) =>
      // TODO: Emit a main class with a call to the main method shim.
      Map.empty
  }

  /**
    * Optionally returns the main definition in the given AST `root`.
    */
  private def getMain(root: Root): Option[Def] = {
    // The main function must be called `main` and occur in the root namespace.
    val sym = Symbol.mkDefnSym("main")

    // Check if the main function exists.
    root.defs.get(sym) flatMap {
      case defn =>
        // The main function must take zero arguments.
        if (defn.formals.isEmpty) Some(defn) else None
    }
  }

}
