/*
 * Copyright 2024 Anna Krogh, Patrick Lundvig, Christian Bonde
 * 2024 Jakob Schneider Villumsen
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
import ca.uwaterloo.flix.language.ast.MonoAst
import ca.uwaterloo.flix.language.ast.MonoAst.{Expr, Pattern}
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugMonoAst
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}
import ca.uwaterloo.flix.language.ast.Symbol

/**
  * Iterative runs of the optimizer pipeline: OccurrenceAnalyzer -> Inliner.
  */
object Optimizer1 {

  /**
    * Returns an optimized version of the given AST `root`.
    */
  def run(root: MonoAst.Root)(implicit flix: Flix): MonoAst.Root = flix.phase("Optimizer1") {
    var result = root
    for (_ <- 1 to 3) {
      val afterOccurrenceAnalyzer = OccurrenceAnalyzer1.run(result)
      val afterInliner = Inliner1.run(afterOccurrenceAnalyzer)
      result = afterInliner.unsafeGet
    }
    applyFreshVariables(result)
  }

  private def applyFreshVariables(root: MonoAst.Root)(implicit flix: Flix): MonoAst.Root = {
    def visitDef(def0: MonoAst.Def): MonoAst.Def = {
      def0.copy(exp = visitExp(def0.exp)(Map.empty))
    }

    def visitFormalParam(fparam0: MonoAst.FormalParam): (MonoAst.FormalParam, Map[Symbol.VarSym, Symbol.VarSym]) = fparam0 match {
      case MonoAst.FormalParam(sym, mod, tpe, src, loc) =>
        val freshVarSym = Symbol.freshVarSym(sym)
        (MonoAst.FormalParam(freshVarSym, mod, tpe, src, loc), Map(sym -> freshVarSym))
    }

    def visitFormalParams(fparams0: List[MonoAst.FormalParam]): (List[MonoAst.FormalParam], Map[Symbol.VarSym, Symbol.VarSym]) = {
      val (fps, substs) = fparams0.map(visitFormalParam).unzip
      val subst = substs.reduceLeft(_ ++ _)
      (fps, subst)
    }

    def visitExp(expr0: MonoAst.Expr)(implicit subst0: Map[Symbol.VarSym, Symbol.VarSym]): MonoAst.Expr = expr0 match {
      case Expr.Cst(cst, tpe, loc) =>
        Expr.Cst(cst, tpe, loc)

      case Expr.Var(sym, tpe, loc) =>
        val freshVarSym = subst0.getOrElse(sym, sym)
        Expr.Var(freshVarSym, tpe, loc)

      case Expr.Lambda(fparam, exp, tpe, loc) =>
        val (fp, subst1) = visitFormalParam(fparam)
        val e = visitExp(exp)(subst0 ++ subst1)
        Expr.Lambda(fp, e, tpe, loc)

      case Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        Expr.ApplyAtomic(op, es, tpe, eff, loc)

      case Expr.ApplyClo(exp, exps, tpe, eff, loc) =>
        val e = visitExp(exp)
        val es = exps.map(visitExp)
        Expr.ApplyClo(e, es, tpe, eff, loc)

      case Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        Expr.ApplyDef(sym, es, itpe, tpe, eff, loc)

      case Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
        val freshVarSym = subst0.getOrElse(sym, throw InternalCompilerException(s"unexpected stale local def sym $sym", loc))
        val es = exps.map(visitExp)
        Expr.ApplyLocalDef(freshVarSym, es, tpe, eff, loc)

      case Expr.Let(sym, exp1, exp2, tpe, eff, loc) =>
        val freshVarSym = Symbol.freshVarSym(sym)
        val subst1 = subst0 + (sym -> freshVarSym)
        val e1 = visitExp(exp1)(subst1)
        val e2 = visitExp(exp2)(subst1)
        Expr.Let(freshVarSym, e1, e2, tpe, eff, loc)

      case Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, loc) =>
        val freshVarSym = Symbol.freshVarSym(sym)
        val subst1 = subst0 + (sym -> freshVarSym)
        val e2 = visitExp(exp2)(subst1)
        val (fps, subst2) = visitFormalParams(fparams)
        val subst3 = subst1 ++ subst2
        val e1 = visitExp(exp1)(subst3)
        Expr.LocalDef(freshVarSym, fps, e1, e2, tpe, eff, loc)

      case Expr.Scope(sym, regionVar, exp, tpe, eff, loc) =>
        val freshVarSym = Symbol.freshVarSym(sym)
        val subst1 = subst0 + (sym -> freshVarSym)
        val e = visitExp(exp)(subst1)
        Expr.Scope(freshVarSym, regionVar, e, tpe, eff, loc)

      case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        val e3 = visitExp(exp3)
        Expr.IfThenElse(e1, e2, e3, tpe, eff, loc)

      case Expr.Stm(exp1, exp2, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        Expr.Stm(e1, e2, tpe, eff, loc)

      case Expr.Discard(exp, eff, loc) =>
        val e = visitExp(exp)
        Expr.Discard(e, eff, loc)

      case Expr.Match(exp, rules, tpe, eff, loc) =>
        val e = visitExp(exp)
        val rs = rules.map {
          case MonoAst.MatchRule(pat, guard, exp) =>
            val (p, subst1) = visitPattern(pat)
            val subst2 = subst0 ++ subst1
            val g = guard.map(visitExp(_)(subst2))
            val e = visitExp(exp)(subst2)
            MonoAst.MatchRule(p, g, e)
        }
        Expr.Match(e, rs, tpe, eff, loc)

      case Expr.VectorLit(exps, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        Expr.VectorLit(es, tpe, eff, loc)

      case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        Expr.VectorLoad(e1, e2, tpe, eff, loc)

      case Expr.VectorLength(exp, loc) =>
        val e = visitExp(exp)
        Expr.VectorLength(e, loc)

      case Expr.Ascribe(exp, tpe, eff, loc) =>
        val e = visitExp(exp)
        Expr.Ascribe(e, tpe, eff, loc)

      case Expr.Cast(exp, declaredType, declaredEff, tpe, eff, loc) =>
        val e = visitExp(exp)
        Expr.Cast(e, declaredType, declaredEff, tpe, eff, loc)

      case Expr.TryCatch(exp, rules, tpe, eff, loc) =>
        val e = visitExp(exp)
        val rs = rules.map {
          case MonoAst.CatchRule(sym, clazz, exp1) =>
            val freshVarSym = Symbol.freshVarSym(sym)
            val subst1 = subst0 + (sym -> freshVarSym)
            val e1 = visitExp(exp1)(subst1)
            MonoAst.CatchRule(freshVarSym, clazz, e1)
        }
        Expr.TryCatch(e, rs, tpe, eff, loc)

      case Expr.TryWith(exp, effUse, rules, tpe, eff, loc) =>
        val e = visitExp(exp)
        val rs = rules.map {
          case MonoAst.HandlerRule(op, fparams, exp1) =>
            val (fps, subst1) = visitFormalParams(fparams)
            val subst2 = subst0 ++ subst1
            val e1 = visitExp(exp1)(subst2)
            MonoAst.HandlerRule(op, fps, e1)
        }
        Expr.TryWith(e, effUse, rs, tpe, eff, loc)

      case Expr.Do(op, exps, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        Expr.Do(op, es, tpe, eff, loc)

      case Expr.NewObject(name, clazz, tpe, eff, methods, loc) =>
        val ms = methods.map {
          case MonoAst.JvmMethod(ident, fparams, exp, retTpe, eff, loc) =>
            val (fps, subst1) = visitFormalParams(fparams)
            val subst2 = subst0 ++ subst1
            val e = visitExp(exp)(subst2)
            MonoAst.JvmMethod(ident, fps, e, retTpe, eff, loc)
        }
        Expr.NewObject(name, clazz, tpe, eff, ms, loc)
    }

    def visitPattern(pattern0: MonoAst.Pattern): (MonoAst.Pattern, Map[Symbol.VarSym, Symbol.VarSym]) = pattern0 match {
      case Pattern.Wild(tpe, loc) =>
        (Pattern.Wild(tpe, loc), Map.empty)

      case Pattern.Var(sym, tpe, loc) =>
        val freshVarSym = Symbol.freshVarSym(sym)
        (Pattern.Var(freshVarSym, tpe, loc), Map(sym -> freshVarSym))

      case Pattern.Cst(cst, tpe, loc) =>
        (Pattern.Cst(cst, tpe, loc), Map.empty)

      case Pattern.Tag(sym, pat, tpe, loc) =>
        val (p, subst) = visitPattern(pat)
        (Pattern.Tag(sym, p, tpe, loc), subst)

      case Pattern.Tuple(pats, tpe, loc) =>
        val (ps, substs) = pats.map(visitPattern).unzip
        val subst = substs.reduceLeft(_ ++ _)
        (Pattern.Tuple(ps, tpe, loc), subst)

      case Pattern.Record(pats, pat, tpe, loc) =>
        val (ps, substs) = pats.map(visitRecordLabelPattern).unzip
        val (p, subst) = visitPattern(pat)
        val subst1 = substs.foldLeft(subst)(_ ++ _)
        (Pattern.Record(ps, p, tpe, loc), subst1)

      case Pattern.RecordEmpty(tpe, loc) =>
        (Pattern.RecordEmpty(tpe, loc), Map.empty)
    }

    def visitRecordLabelPattern(pattern0: MonoAst.Pattern.Record.RecordLabelPattern): (MonoAst.Pattern.Record.RecordLabelPattern, Map[Symbol.VarSym, Symbol.VarSym]) = pattern0 match {
      case MonoAst.Pattern.Record.RecordLabelPattern(label, pat, tpe, loc) =>
        val (p, subst) = visitPattern(pat)
        (MonoAst.Pattern.Record.RecordLabelPattern(label, p, tpe, loc), subst)
    }

    val defs = ParOps.parMapValues(root.defs)(visitDef)
    root.copy(defs = defs)
  }
}
