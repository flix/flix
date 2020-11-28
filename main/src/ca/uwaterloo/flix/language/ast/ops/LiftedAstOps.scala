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

    /**
      * Replaces the variable in the CatchRule with a fresh one, and propagates that and other VarSym replacements
      */
    def visitCatchRule(cr: CatchRule, varMap: Map[Symbol.VarSym, Symbol.VarSym]): CatchRule = {
      val CatchRule(sym, clazz, exp) = cr
      val newSym = Symbol.freshVarSym(sym)
      val newVarMap = varMap + (sym -> newSym)
      val e = visit(exp, newVarMap)
      CatchRule(newSym, clazz, e)
    }

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

      // Todo: Maybe we should refresh the labels before visiting the exps?
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

      // Todo: Is this a sym I should care about?
      case Expression.Is(sym, tag, exp, loc) =>
        val e = visit(exp, varMap)
        Expression.Is(sym, tag, e, loc)

      case Expression.Tag(sym, tag, exp, tpe, loc) =>
        val e = visit(exp, varMap)
        Expression.Tag(sym, tag, e, tpe, loc)

      case Expression.Untag(sym, tag, exp, tpe, loc) =>
        val e = visit(exp, varMap)
        Expression.Untag(sym, tag, e, tpe, loc)

      case Expression.Index(base, offset, tpe, loc) =>
        val b = visit(base, varMap)
        Expression.Index(b, offset, tpe, loc)

      case Expression.IndexMut(base, offset, toInsert, tpe, loc) =>
        val b = visit(base, varMap)
        val ti = visit(toInsert, varMap)
        Expression.IndexMut(b, offset, ti, tpe, loc)

      case Expression.Tuple(elms, tpe, loc) =>
        val es = elms.map(e => visit(e, varMap))
        Expression.Tuple(es, tpe, loc)

      case Expression.RecordEmpty(_, _) => exp0

      case Expression.RecordSelect(exp, field, tpe, loc) =>
        val e = visit(exp, varMap)
        Expression.RecordSelect(e, field, tpe, loc)

      // Todo: Could value or rest make a new variable affecting the other?
      case Expression.RecordExtend(field, value, rest, tpe, loc) =>
        val v = visit(value, varMap)
        val r = visit(rest, varMap)
        Expression.RecordExtend(field, v, r, tpe, loc)

      case Expression.RecordRestrict(field, rest, tpe, loc) =>
        val r = visit(rest, varMap)
        Expression.RecordRestrict(field, r, tpe, loc)

      case Expression.ArrayLit(elms, tpe, loc) =>
        val es = elms.map(e => visit(e, varMap))
        Expression.ArrayLit(es, tpe, loc)

      case Expression.ArrayNew(elm, len, tpe, loc) =>
        val e = visit(elm, varMap)
        val l = visit(len, varMap)
        Expression.ArrayNew(e, l, tpe, loc)

      case Expression.ArrayLoad(base, index, tpe, loc) =>
        val b = visit(base, varMap)
        val i = visit(index, varMap)
        Expression.ArrayLoad(b, i, tpe, loc)

      case Expression.ArrayStore(base, index, elm, tpe, loc) =>
        val b = visit(base, varMap)
        val i = visit(index, varMap)
        val e = visit(elm, varMap)
        Expression.ArrayStore(b, i, e, tpe, loc)

      case Expression.ArrayLength(base, tpe, loc) =>
        val b = visit(base, varMap)
        Expression.ArrayLength(b, tpe, loc)

      case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
        val b = visit(base, varMap)
        val bi = visit(beginIndex, varMap)
        val ei = visit(endIndex, varMap)
        Expression.ArraySlice(b, bi, ei, tpe, loc)

      case Expression.Ref(exp, tpe, loc) =>
        val e = visit(exp, varMap)
        Expression.Ref(e, tpe, loc)

      case Expression.Deref(exp, tpe, loc) =>
        val e = visit(exp, varMap)
        Expression.Deref(e, tpe, loc)

      // Todo: could a let in exp2 affect exp1?
      case Expression.Assign(exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1, varMap)
        val e2 = visit(exp2, varMap)
        Expression.Assign(e1, e2, tpe, loc)

      // Todo: Should the map be updated first?
      case Expression.Existential(fparam, exp, loc) =>
        val FormalParam(sym, _, _, _) = fparam
        val newVarSym = Symbol.freshVarSym(sym)
        val newVarMap = varMap + (sym -> newVarSym)
        val e = visit(exp, newVarMap)
        val fp = visitFormalParams(fparam, newVarMap)
        Expression.Existential(fp, e, loc)

      case Expression.Universal(fparam, exp, loc) =>
        val FormalParam(sym, _, _, _) = fparam
        val newVarSym = Symbol.freshVarSym(sym)
        val newVarMap = varMap + (sym -> newVarSym)
        val e = visit(exp, newVarMap)
        val fp = visitFormalParams(fparam, newVarMap)
        Expression.Universal(fp, e, loc)

      case Expression.Cast(exp, tpe, loc) =>
        val e = visit(exp, varMap)
        Expression.Cast(e, tpe, loc)

      // Todo: Should the syms from the rules be added to the map
      case Expression.TryCatch(exp, rules, tpe, loc) =>
        val e = visit(exp, varMap)
        val r = rules.map(cr => visitCatchRule(cr, varMap))
        Expression.TryCatch(e, r, tpe, loc)
    }

    val variablesMap = Map.from(defn.fparams.map {
      case FormalParam(sym, _, _, _) => sym -> Symbol.freshVarSym(sym)
    })

    val newExp = visit(defn.exp, variablesMap)
    val newFParams = defn.fparams.map(fp => visitFormalParams(fp, variablesMap))
    defn.copy(exp = newExp, fparams = newFParams)
  }

}
