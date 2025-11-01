/*
 * Copyright 2021 Magnus Madsen
 *                Casper Dalgaard Nielsen
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

package ca.uwaterloo.flix.language.phase.monomorph

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{LoweredAst, MonoAst, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.phase.monomorph.Specialization.Context
import ca.uwaterloo.flix.language.phase.monomorph.Symbols.Defs

/**
  * This phase translates AST expressions related to the Datalog subset of the
  * language into `Fixpoint.Ast.Datalog` values (which are ordinary Flix values).
  * This allows the Datalog engine to be implemented as an ordinary Flix program.
  *
  * In addition to translating expressions, types must also be translated from
  * Schema types to enum types.
  *
  * Finally, values must be boxed using the Boxable.
  *
  * It also translates channels to the lowered types.
  */
object Lowering {

  /**
    * Lowers the type `t`. Currently implemented as a no-op.
    *
    * When implemented:
    * Replaces schema types with the Datalog enum type and channel-related types with the channel enum type.
    *
    * @param t the type to be lowered.
    * @return
    */
   def lowerType(t: Type): Type = t

  /**
    * Returns the definition associated with the given symbol `sym`.
    *
    * @param tpe must be subst. Can be visited, if the underlying function can handle that
    */
  private def lookup(sym: Symbol.DefnSym, tpe: Type)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): Symbol.DefnSym =
    Specialization.specializeDefnSym(sym, tpe)

  /**
    * Make a new channel tuple (sender, receiver) expression
    *
    * @param tpe The specialized type of the result
    */
  def visitNewChannel(exp: MonoAst.Expr, tpe: Type, eff: Type, loc: SourceLocation)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = {
    val itpe = Type.mkIoArrow(exp.tpe, tpe, loc)
    val defnSym = lookup(Defs.ChannelNewTuple, itpe)
    MonoAst.Expr.ApplyDef(defnSym, exp :: Nil, lowerType(itpe), lowerType(tpe), eff, loc)
  }

}
