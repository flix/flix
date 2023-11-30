/*
 * Copyright 2023 Jonathan Lindegaard Starup
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
import ca.uwaterloo.flix.language.ast.ReducedAst._
import ca.uwaterloo.flix.util.ParOps


object EffectBinder {

  /**
    * Identity Function.
    */
  def run(root: Root)(implicit flix: Flix): Root = flix.phase("EffectBinder") {
    val newDefs = ParOps.parMapValues(root.defs)(letBindEffectsDef)
    root.copy(defs = newDefs)
  }

  /**
    * Identity Function.
    */
  private def letBindEffectsDef(defn: Def)(implicit flix: Flix): Def = {
    val stmt = letBindEffectsStmt(defn.stmt)
    defn.copy(stmt = stmt)
  }

  /**
    * Identity Function.
    */
  private def letBindEffectsStmt(stmt: Stmt)(implicit flix: Flix): Stmt = stmt match {
    case Stmt.Ret(expr, tpe, loc) => Stmt.Ret(letBindEffectsTopLevel(expr), tpe, loc)
  }

  /**
    * Identity Function.
    */
  private def letBindEffectsTopLevel(exp: Expr)(implicit flix: Flix): Expr = exp match {
    case Expr.Cst(cst, tpe, loc) =>
      letBindEffects(Expr.Cst(cst, tpe, loc))

    case Expr.Var(sym, tpe, loc) =>
      letBindEffects(Expr.Var(sym, tpe, loc))

    case Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
      letBindEffects(Expr.ApplyAtomic(op, exps, tpe, purity, loc))

    case Expr.ApplyClo(exp, exps, ct, tpe, purity, loc) =>
      letBindEffects(Expr.ApplyClo(exp, exps, ct, tpe, purity, loc))

    case Expr.ApplyDef(sym, exps, ct, tpe, purity, loc) =>
      letBindEffects(Expr.ApplyDef(sym, exps, ct, tpe, purity, loc))

    case Expr.ApplySelfTail(sym, formals, actuals, tpe, purity, loc) =>
      letBindEffects(Expr.ApplySelfTail(sym, formals, actuals, tpe, purity, loc))

    case Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      letBindEffects(Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc))

    case Expr.Branch(exp, branches, tpe, purity, loc) =>
      val e = letBindEffectsTopLevel(exp)
      val branches1 = branches.map {
        case (sym, branchExp) => (sym, letBindEffectsTopLevel(branchExp))
      }
      Expr.Branch(e, branches1, tpe, purity, loc)

    case Expr.JumpTo(sym, tpe, purity, loc) =>
      Expr.JumpTo(sym, tpe, purity, loc)

    case Expr.Let(sym, exp1, exp2, tpe, purity, loc) =>
      val e1 = letBindEffects(exp1)
      val e2 = letBindEffectsTopLevel(exp2)
      Expr.Let(sym, e1, e2, tpe, purity, loc)

    case Expr.LetRec(varSym, index, defSym, exp1, exp2, tpe, purity, loc) =>
      val e1 = letBindEffects(exp1)
      val e2 = letBindEffectsTopLevel(exp2)
      Expr.LetRec(varSym, index, defSym, e1, e2, tpe, purity, loc)

    case Expr.Scope(sym, exp, tpe, purity, loc) =>
      val e = letBindEffectsTopLevel(exp)
      Expr.Scope(sym, e, tpe, purity, loc)

    case Expr.TryCatch(exp, rules, tpe, purity, loc) =>
      val e = letBindEffectsTopLevel(exp)
      val rules1 = rules.map {
        case cr => CatchRule(cr.sym, cr.clazz, letBindEffectsTopLevel(cr.exp))
      }
      Expr.TryCatch(e, rules1, tpe, purity, loc)

    case Expr.TryWith(exp, effUse, rules, tpe, purity, loc) =>
      val e = letBindEffectsTopLevel(exp)
      val rules1 = rules.map {
        case hr => hr.copy(exp = letBindEffectsTopLevel(hr.exp))
      }
      Expr.TryWith(e, effUse, rules1, tpe, purity, loc)

    case Expr.Do(op, exps, tpe, purity, loc) =>
      letBindEffects(Expr.Do(op, exps, tpe, purity, loc))

    case Expr.Resume(exp, tpe, loc) =>
      letBindEffects(Expr.Resume(exp, tpe, loc))

    case Expr.NewObject(name, clazz, tpe, purity, methods, exps, loc) =>
      letBindEffects(Expr.NewObject(name, clazz, tpe, purity, methods, exps, loc))
  }

  /**
    * Identity Function.
    */
  private def letBindEffects(exp: Expr)(implicit flix: Flix): Expr = exp match {
    case Expr.Cst(cst, tpe, loc) =>
      Expr.Cst(cst, tpe, loc)

    case Expr.Var(sym, tpe, loc) =>
      Expr.Var(sym, tpe, loc)

    case Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
      Expr.ApplyAtomic(op, exps, tpe, purity, loc)

    case Expr.ApplyClo(exp, exps, ct, tpe, purity, loc) =>
      Expr.ApplyClo(exp, exps, ct, tpe, purity, loc)

    case Expr.ApplyDef(sym, exps, ct, tpe, purity, loc) =>
      Expr.ApplyDef(sym, exps, ct, tpe, purity, loc)

    case Expr.ApplySelfTail(sym, formals, actuals, tpe, purity, loc) =>
      Expr.ApplySelfTail(sym, formals, actuals, tpe, purity, loc)

    case Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc)

    case Expr.Branch(exp, branches, tpe, purity, loc) =>
      Expr.Branch(exp, branches, tpe, purity, loc)

    case Expr.JumpTo(sym, tpe, purity, loc) =>
      Expr.JumpTo(sym, tpe, purity, loc)

    case Expr.Let(sym, exp1, exp2, tpe, purity, loc) =>
      Expr.Let(sym, exp1, exp2, tpe, purity, loc)

    case Expr.LetRec(varSym, index, defSym, exp1, exp2, purity, tpe, loc) =>
      Expr.LetRec(varSym, index, defSym, exp1, exp2, purity, tpe, loc)

    case Expr.Scope(sym, exp, tpe, purity, loc) =>
      Expr.Scope(sym, exp, tpe, purity, loc)

    case Expr.TryCatch(exp, rules, tpe, purity, loc) =>
      Expr.TryCatch(exp, rules, tpe, purity, loc)

    case Expr.TryWith(exp, effUse, rules, tpe, purity, loc) =>
      Expr.TryWith(exp, effUse, rules, tpe, purity, loc)

    case Expr.Do(op, exps, tpe, purity, loc) =>
      Expr.Do(op, exps, tpe, purity, loc)

    case Expr.NewObject(name, clazz, tpe, purity, methods, exps, loc) =>
      Expr.NewObject(name, clazz, tpe, purity, methods, exps, loc)

    case Expr.Resume(exp, tpe, loc) =>
      Expr.Resume(exp, tpe, loc)
  }

}
