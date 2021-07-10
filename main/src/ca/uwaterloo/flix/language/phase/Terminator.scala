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
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.errors.TerminationError
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation.{ToFailure, ToSuccess}

/**
  * Performs checks related to termination.
  */
object Terminator extends Phase[Root, Root] {

  override def run(root: Root)(implicit flix: Flix): Validation[Root, TerminationError] = flix.phase("Terminator") {
    val defVal = Validation.traverseX(root.defs.values)(checkDef)

    defVal map {
      _ => root
    }
  }

  /**
    * Returns an error if the `defn` recurses unconditionally.
    */
  def checkDef(defn: Def): Validation[Unit, TerminationError] = {
    if (unconditionallyRecurses(defn.impl.exp, defn)) {
      TerminationError.UnconditionalRecursion(defn.sym).toFailure
    } else {
      ().toSuccess
    }
  }

  /**
    * Returns true if `exp00` unconditionally recurses, according to `recursiveAppCheck`.
    */
  def unconditionallyRecurses(exp00: Expression, defn: Def): Boolean = {
    def visit(exp0: Expression): Boolean = exp0 match {
      case Expression.Unit(_) => false
      case Expression.Null(_, _) => false
      case Expression.True(_) => false
      case Expression.False(_) => false
      case Expression.Char(_, _) => false
      case Expression.Float32(_, _) => false
      case Expression.Float64(_, _) => false
      case Expression.Int8(_, _) => false
      case Expression.Int16(_, _) => false
      case Expression.Int32(_, _) => false
      case Expression.Int64(_, _) => false
      case Expression.BigInt(_, _) => false
      case Expression.Str(_, _) => false
      case Expression.Default(_, _) => false
      case Expression.Wild(_, _) => false
      case Expression.Var(_, _, _) => false
      case Expression.Def(_, _, _) => false
      case Expression.Sig(_, _, _) => false
      case Expression.Hole(_, _, _, _) => false
      case Expression.Lambda(_, _, _, _) => false

      // If the Apply is a recursive call, then return true
      case Expression.Apply(Expression.Def(sym, _, _), exps, _, _, _) if (defn.sym == sym && exps.length >= defn.spec.fparams.length) => true
      // Otherwise continue as normal
      case Expression.Apply(exp, exps, _, _, _) => visit(exp) || exps.exists(visit)

      case Expression.Unary(_, exp, _, _, _) => visit(exp)
      case Expression.Binary(_, exp1, exp2, _, _, _) => visit(exp1) || visit(exp2)
      case Expression.Let(_, exp1, exp2, _, _, _) => visit(exp1) || visit(exp2)
      case Expression.LetRegion(_, exp, _, _, _) => visit(exp)
      case Expression.IfThenElse(exp1, exp2, exp3, _, _, _) => visit(exp1) || (visit(exp2) && visit(exp3))
      case Expression.Stm(exp1, exp2, _, _, _) => visit(exp1) || visit(exp2)
      case Expression.Match(exp, rules, _, _, _) => visit(exp) || rules.forall { match0 => visit(match0.guard) || visit(match0.exp) }
      case Expression.Choose(exps, rules, _, _, _) => exps.exists(visit) || rules.forall { rule => visit(rule.exp) }
      case Expression.Tag(_, _, exp, _, _, _) => visit(exp)
      case Expression.Tuple(elms, _, _, _) => elms.exists(visit)
      case Expression.RecordEmpty(_, _) => false
      case Expression.RecordSelect(exp, _, _, _, _) => visit(exp)
      case Expression.RecordExtend(_, value, rest, _, _, _) => visit(value) || visit(rest)
      case Expression.RecordRestrict(_, rest, _, _, _) => visit(rest)
      case Expression.ArrayLit(elms, _, _, _) => elms.exists(visit)
      case Expression.ArrayNew(elm, len, _, _, _) => visit(elm) || visit(len)
      case Expression.ArrayLoad(base, index, _, _, _) => visit(base) || visit(index)
      case Expression.ArrayLength(base, _, _) => visit(base)
      case Expression.ArrayStore(base, index, elm, _) => visit(base) || visit(index) || visit(elm)
      case Expression.ArraySlice(base, beginIndex, endIndex, _, _) => visit(base) || visit(beginIndex) || visit(endIndex)
      case Expression.Ref(exp, _, _, _) => visit(exp)
      case Expression.Deref(exp, _, _, _) => visit(exp)
      case Expression.Assign(exp1, exp2, _, _, _) => visit(exp1) || visit(exp2)
      case Expression.Existential(_, exp, _) => visit(exp)
      case Expression.Universal(_, exp, _) => visit(exp)
      case Expression.Ascribe(exp, _, _, _) => visit(exp)
      case Expression.Cast(exp, _, _, _) => visit(exp)
      case Expression.TryCatch(exp, rules, _, _, _) => visit(exp) || rules.forall { rule => visit(rule.exp) }
      case Expression.InvokeConstructor(_, args, _, _, _) => args.exists(visit)
      case Expression.InvokeMethod(_, exp, args, _, _, _) => visit(exp) || args.exists(visit)
      case Expression.InvokeStaticMethod(_, args, _, _, _) => args.exists(visit)
      case Expression.GetField(_, exp, _, _, _) => visit(exp)
      case Expression.PutField(_, exp1, exp2, _, _, _) => visit(exp1) || visit(exp2)
      case Expression.GetStaticField(_, _, _, _) => false
      case Expression.PutStaticField(_, exp, _, _, _) => visit(exp)
      case Expression.NewChannel(exp, _, _, _) => visit(exp)
      case Expression.GetChannel(exp, _, _, _) => visit(exp)
      case Expression.PutChannel(exp1, exp2, _, _, _) => visit(exp1) || visit(exp2)
      case Expression.SelectChannel(rules, default, _, _, _) => rules.forall { rule => visit(rule.exp) || visit(rule.chan) } && default.forall(visit)
      case Expression.Spawn(exp, _, _, _) => visit(exp)

      case Expression.Lazy(_, _, _) => false // Lazily evaluated expressions terminate

      case Expression.Force(exp, _, _, _) => visit(exp)
      case Expression.FixpointConstraintSet(_, _, _, _) => false
      case Expression.FixpointMerge(exp1, exp2, _, _, _, _) => visit(exp1) || visit(exp2)
      case Expression.FixpointSolve(exp, _, _, _, _) => visit(exp)
      case Expression.FixpointFilter(_, exp, _, _, _) => visit(exp)
      case Expression.FixpointProjectIn(exp, _, _, _, _) => visit(exp)
      case Expression.FixpointProjectOut(_, exp, _, _, _) => visit(exp)
      case Expression.MatchEff(exp1, exp2, exp3, _, _, _) => visit(exp1) || (visit(exp2) || visit(exp3))
    }

    visit(exp00)
  }

}
