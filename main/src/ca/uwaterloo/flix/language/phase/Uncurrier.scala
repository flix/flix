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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.SimplifiedAst.{Def, Root}
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.{SpecialOperator, Type}
import ca.uwaterloo.flix.util.Validation

/**
  * Introduces uncurried versions of certain definitions where needed for interop.
  */
object Uncurrier extends Phase[Root, Root] {

  /**
    * Introduces uncurried definitions where needed.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationError] = {
    visitSpecialOps(root.specialOps, root)
    ???
  }

  def visitSpecialOps(specialOps: Map[SpecialOperator, Map[Type, Symbol.DefnSym]], root: Root)(implicit flix: Flix): Map[SpecialOperator, Map[Type, Symbol.DefnSym]] = {


    ???
  }

  def visitSpecialOp(specialOp: SpecialOperator, tpe: Type, sym: Symbol.DefnSym, root: Root)(implicit flix: Flix): Symbol.DefnSym = {
    ???
  }

  /**
    * Introduces an uncurried version if the definition associated with the given symbol `sym`.
    *
    * Returns the same symbol if the definition is not marked for uncurrying.
    */
  def uncurryDef(sym: Symbol.DefnSym, root: Root)(implicit flix: Flix): Symbol.DefnSym = {
    // Lookup the original definition.
    val defn = root.defs(sym)

    // Check if it is marked for uncurrying. If not, return the original symbol.



    val uncurriedSym = Symbol.freshDefnSym(sym)

    val ann = ???
    val mod = ???
    val fparams = ???
    val exp = ???
    val tpe = ???
    val loc = ???

    val uncurriedDef = Def(ann, mod, sym, fparams, exp, tpe, loc)

    ???
  }

}
