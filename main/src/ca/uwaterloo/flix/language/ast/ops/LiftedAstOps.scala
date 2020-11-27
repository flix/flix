/*
 * Copyright 2020 Andreas Heglingegård
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.language.ast.ops

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.LiftedAst._
import ca.uwaterloo.flix.language.ast.Symbol

//import scala.collection.immutable.Map

object LiftedAstOps {

  /**
    * Replaces all parameter variables and in the expression with fresh variables
    */
  def refreshVarNames(defn: Def)(implicit flix: Flix): Def = {

    def visitFormalParams(fParam: FormalParam, varMap: Map[Symbol.VarSym, Symbol.VarSym]): FormalParam = fParam match {
      case FormalParam(sym, mod, tpe, loc) =>
        val s = varMap(sym)
        FormalParam(s, mod, tpe, loc)
    }

    def visit(exp0: Expression, varMap: Map[Symbol.VarSym, Symbol.VarSym]): Expression = exp0 match {
      case Expression.Unit => exp0
      case Expression.Null(_) => exp0
      case Expression.True => exp0
      case Expression.False => exp0
      case Expression.Char(_) => exp0
      case Expression.Float32(_) => exp0
      case Expression.Float64(_) => exp0
      case Expression.Int8(_) => exp0
      case Expression.Int16(_) => exp0
      case Expression.Int32(_) => exp0
      case Expression.Int64(_) => exp0
      case Expression.BigInt(_) => exp0
      case Expression.Str(_) => exp0
      case Expression.Var(sym, tpe, loc) =>
        val s = varMap(sym)
        Expression.Var(s, tpe, loc)

      // Todo: Not sure if the VarSym in freeVars should be refreshed
      case Expression.Closure(sym, freeVars, tpe, loc) => ???
      case Expression.ApplyClo(exp, args, tpe, loc) =>
        val e = visit(exp, varMap)
        val a = args.map(ea => visit(ea, varMap))
        Expression.ApplyClo(e, a, tpe, loc)

      case Expression.ApplyDef(sym, args, tpe, loc) =>
        val a = args.map(ea => visit(ea, varMap))
        Expression.ApplyDef(sym, a, tpe, loc)

      case Expression.ApplyCloTail(exp, args, tpe, loc) =>
        val e = visit(exp, varMap)
        val a = args.map(ea => visit(ea, varMap))
        Expression.ApplyCloTail(e, a, tpe, loc)

      case Expression.ApplyDefTail(sym, args, tpe, loc) =>
        val a = args.map(ea => visit(ea, varMap))
        Expression.ApplyDefTail(sym, a, tpe, loc)

      // Todo: Not sure what is going on here
      case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
        ???

      case Expression.Unary(sop, op, exp, tpe, loc) =>
        val e = visit(exp, varMap)
        Expression.Unary(sop, op, e, tpe, loc)

      case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1, varMap)
        val e2 = visit(exp2, varMap)
        Expression.Binary(sop, op, e1, e2, tpe, loc)

      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        val e1 = visit(exp1, varMap)
        val e2 = visit(exp2, varMap)
        val e3 = visit(exp3, varMap)
        Expression.IfThenElse(e1, e2, e3, tpe, loc)

      case Expression.Branch(exp, branches, tpe, loc) =>
        val e = visit(exp, varMap)
        val b = branches.map {
          case (lSym, expLSym) => lSym -> visit(expLSym, varMap)
        }
        Expression.Branch(e, b, tpe, loc)

      // Todo: We might need to refresh the labelSym too
      case Expression.JumpTo(_, _, _) => exp0

      // Todo: Check that only the second exp should use the new map
      case Expression.Let(sym, exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1, varMap)
        val newVarMap = varMap + (sym -> Symbol.freshVarSym(sym))
        val e2 = visit(exp2, newVarMap)
        Expression.Let(sym, e1, e2, tpe, loc)
    }

    val variablesMap = Map.from(defn.fparams.map {
      case FormalParam(sym, _, _, _) => sym -> Symbol.freshVarSym(sym)
    })

    val newExp = visit(defn.exp, variablesMap)
    val newFParams = defn.fparams.map(fp => visitFormalParams(fp, variablesMap))
    defn.copy(exp = newExp, fparams = newFParams)
  }

}
