/*
 * Copyright 2021 Matthew Lutze
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
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.errors.TerminationError
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation.{ToFailure, ToSuccess}

// MATT docs
object Terminator extends Phase[Root, Root] {

  // MATT wrap with timer thingy
  override def run(input: Root)(implicit flix: Flix): Validation[Root, TerminationError] = {
    val defVal = Validation.traverseX(input.defs.values)(checkDef)


    Validation.sequenceX(defVal :: Nil) map {
      _ => input
    }
  }

  def checkDef(defn: Def): Validation[Unit, TerminationError] = {
    if (unconditionallyRecurses(defn.impl.exp, defn.sym, defn.spec.fparams.length)) {
      TerminationError.UnconditionalDefRecursion(defn.sym, defn.spec.loc).toFailure
    } else {
      ().toSuccess
    }
  }

  // MATT also check sigs
  def unconditionallyRecurses(exp0: Expression, sym: Symbol.DefnSym, arity: Int): Boolean = {
    def visit(exp: Expression): Boolean = {
      exp match {
        case Expression.Unit(loc) => false
        case Expression.Null(tpe, loc) => false
        case Expression.True(loc) => false
        case Expression.False(loc) => false
        case Expression.Char(lit, loc) => false
        case Expression.Float32(lit, loc) => false
        case Expression.Float64(lit, loc) => false
        case Expression.Int8(lit, loc) => false
        case Expression.Int16(lit, loc) => false
        case Expression.Int32(lit, loc) => false
        case Expression.Int64(lit, loc) => false
        case Expression.BigInt(lit, loc) => false
        case Expression.Str(lit, loc) => false
        case Expression.Default(tpe, loc) => false
        case Expression.Wild(tpe, loc) => false
        case Expression.Var(sym, tpe, loc) => false
        case Expression.Def(sym, tpe, loc) => false
        case Expression.Sig(sym, tpe, loc) => false
        case Expression.Hole(sym, tpe, eff, loc) => false
        case Expression.Lambda(fparam, exp, tpe, loc) => false
        case Expression.Apply(Expression.Def(sym1, _, _), exps, tpe, eff, loc) if sym1 == sym && exps.lengthIs >= arity => true
        case Expression.Apply(exp, exps, tpe, eff, loc) => visit(exp) || exps.exists(visit)
        case Expression.Unary(sop, exp, tpe, eff, loc) => visit(exp)
        case Expression.Binary(sop, exp1, exp2, tpe, eff, loc) => visit(exp1) || visit(exp2)
        case Expression.Let(sym, exp1, exp2, tpe, eff, loc) => visit(exp1) || visit(exp2)
        case Expression.LetRegion(sym, exp, tpe, eff, loc) => visit(exp)
        case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) => visit(exp1) || (visit(exp2) && visit(exp3))
        case Expression.Stm(exp1, exp2, tpe, eff, loc) => visit(exp1) || visit(exp2)
        case Expression.Match(exp, rules, tpe, eff, loc) => visit(exp) || rules.forall { case MatchRule(_, guard, exp1) => visit(guard) || visit(exp1) } // MATT docs
        case Expression.Choose(exps, rules, tpe, eff, loc) => ??? // MATT
        case Expression.Tag(sym, tag, exp, tpe, eff, loc) => visit(exp)
        case Expression.Tuple(elms, tpe, eff, loc) => elms.exists(visit)
        case Expression.RecordEmpty(tpe, loc) => false
        case Expression.RecordSelect(exp, field, tpe, eff, loc) => visit(exp)
        case Expression.RecordExtend(field, value, rest, tpe, eff, loc) => visit(exp) || visit(rest)
        case Expression.RecordRestrict(field, rest, tpe, eff, loc) => visit(rest)
        case Expression.ArrayLit(elms, tpe, eff, loc) => elms.exists(visit)
        case Expression.ArrayNew(elm, len, tpe, eff, loc) => visit(elm) || visit(len)
        case Expression.ArrayLoad(base, index, tpe, eff, loc) => visit(base) || visit(index)
        case Expression.ArrayLength(base, eff, loc) => visit(base)
        case Expression.ArrayStore(base, index, elm, loc) => visit(base) || visit(index) || visit(elm)
        case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) => visit(base) || visit(beginIndex) || visit(endIndex)
        case Expression.Ref(exp, tpe, eff, loc) => visit(exp)
        case Expression.Deref(exp, tpe, eff, loc) => visit(exp)
        case Expression.Assign(exp1, exp2, tpe, eff, loc) => visit(exp1) || visit(exp2)
        case Expression.Existential(fparam, exp, loc) => ??? // MATT
        case Expression.Universal(fparam, exp, loc) => ??? // MATT
        case Expression.Ascribe(exp, tpe, eff, loc) => visit(exp)
        case Expression.Cast(exp, tpe, eff, loc) => visit(exp)
        case Expression.TryCatch(exp, rules, tpe, eff, loc) => ??? // MATT
        case Expression.InvokeConstructor(constructor, args, tpe, eff, loc) => args.exists(visit)
        case Expression.InvokeMethod(method, exp, args, tpe, eff, loc) => visit(exp) || args.exists(visit)
        case Expression.InvokeStaticMethod(method, args, tpe, eff, loc) => args.exists(visit)
        case Expression.GetField(field, exp, tpe, eff, loc) => visit(exp)
        case Expression.PutField(field, exp1, exp2, tpe, eff, loc) => visit(exp1) || visit(exp2)
        case Expression.GetStaticField(field, tpe, eff, loc) => false
        case Expression.PutStaticField(field, exp, tpe, eff, loc) => visit(exp)
        case Expression.NewChannel(exp, tpe, eff, loc) => visit(exp)
        case Expression.GetChannel(exp, tpe, eff, loc) => visit(exp)
        case Expression.PutChannel(exp1, exp2, tpe, eff, loc) => visit(exp1) || visit(exp2)
        case Expression.SelectChannel(rules, default, tpe, eff, loc) => ??? // MATT
        case Expression.Spawn(exp, tpe, eff, loc) => visit(exp)
        case Expression.Lazy(exp, tpe, loc) => false
        case Expression.Force(exp, tpe, eff, loc) => visit(exp)
        case Expression.FixpointConstraintSet(cs, stf, tpe, loc) => ??? // MATT
        case Expression.FixpointMerge(exp1, exp2, stf, tpe, eff, loc) => visit(exp1) || visit(exp2)
        case Expression.FixpointSolve(exp, stf, tpe, eff, loc) => visit(exp)
        case Expression.FixpointFilter(pred, exp, tpe, eff, loc) => visit(exp)
        case Expression.FixpointProjectIn(exp, pred, tpe, eff, loc) => visit(exp)
        case Expression.FixpointProjectOut(pred, exp, tpe, eff, loc) => visit(exp)
        case Expression.MatchEff(exp1, exp2, exp3, tpe, eff, loc) => ??? // MATT
      }
    }

    visit(exp0)
  }

}
