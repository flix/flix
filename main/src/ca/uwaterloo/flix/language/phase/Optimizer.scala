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
import ca.uwaterloo.flix.language.ast.OccurrenceAst1.Linearity
import ca.uwaterloo.flix.language.ast.{OccurrenceAst1, Symbol}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.util.ParOps
import ca.uwaterloo.flix.util.collection.ListMap

/**
  * Iterative runs of the optimizer pipeline: OccurrenceAnalyzer -> Inliner.
  */
object Optimizer {

  /**
    * Returns an optimized version of the given AST `root`.
    */
  def run(root: MonoAst.Root)(implicit flix: Flix): MonoAst.Root = flix.phase("Optimizer1") {
    if (flix.options.xnooptimizer) {
      root
    } else {
      var result = ToOccurrenceAst1.run(root)
      var stats: Stats = null
      for (_ <- 1 to flix.options.inlinerRounds) {
        val afterOccurrenceAnalyzer = OccurrenceAnalyzer1.run(result)
        val (afterInliner, stats1) = Inliner.run(afterOccurrenceAnalyzer)
        stats = if (stats == null) stats1 else stats ++ stats1
        result = afterInliner
      }
      // println(stats)
      ToMonoAst.run(result)
    }
  }

  private object ToOccurrenceAst1 {
    def run(root: MonoAst.Root)(implicit flix: Flix): OccurrenceAst1.Root = {
      val defs = ParOps.parMapValues(root.defs)(visitDef)
      OccurrenceAst1.Root(defs, root.enums, root.structs, root.effects, root.mainEntryPoint, root.entryPoints, root.sources)
    }

    private def visitDef(def0: MonoAst.Def): OccurrenceAst1.Def = def0 match {
      case MonoAst.Def(sym, spec, exp, loc) =>
        val e = visitExp(exp)
        val fps = spec.fparams.map(fp => (fp, OccurrenceAst1.Occur.Dead))
        val ctx = OccurrenceAst1.DefContext(isDirectCall = false, OccurrenceAst1.Occur.Dead, 0, 0, isSelfRecursive = false)
        OccurrenceAst1.Def(sym, fps, spec, e, ctx, loc)
    }

    private def visitExp(expr0: MonoAst.Expr): OccurrenceAst1.Expr = expr0 match {
      case MonoAst.Expr.Cst(cst, tpe, loc) =>
        OccurrenceAst1.Expr.Cst(cst, tpe, loc)

      case MonoAst.Expr.Var(sym, tpe, loc) =>
        OccurrenceAst1.Expr.Var(sym, tpe, loc)

      case MonoAst.Expr.Lambda(fparam, exp, tpe, loc) =>
        val fp = (fparam, OccurrenceAst1.Occur.Dead)
        val e = visitExp(exp)
        OccurrenceAst1.Expr.Lambda(fp, e, tpe, loc)

      case MonoAst.Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        OccurrenceAst1.Expr.ApplyAtomic(op, es, tpe, eff, loc)

      case MonoAst.Expr.ApplyClo(exp1, exp2, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        OccurrenceAst1.Expr.ApplyClo(e1, e2, tpe, eff, loc)

      case MonoAst.Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        OccurrenceAst1.Expr.ApplyDef(sym, es, itpe, tpe, eff, loc)

      case MonoAst.Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        OccurrenceAst1.Expr.ApplyLocalDef(sym, es, tpe, eff, loc)

      case MonoAst.Expr.Let(sym, exp1, exp2, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        val occur = OccurrenceAst1.Occur.Dead
        OccurrenceAst1.Expr.Let(sym, e1, e2, tpe, eff, occur, loc)

      case MonoAst.Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, loc) =>
        val fps = fparams.map(fp => (fp, OccurrenceAst1.Occur.Dead))
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        val occur = OccurrenceAst1.Occur.Dead
        OccurrenceAst1.Expr.LocalDef(sym, fps, e1, e2, tpe, eff, occur, loc)

      case MonoAst.Expr.Scope(sym, regSym, exp, tpe, eff, loc) =>
        val e = visitExp(exp)
        OccurrenceAst1.Expr.Scope(sym, regSym, e, tpe, eff, loc)

      case MonoAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        val e3 = visitExp(exp3)
        OccurrenceAst1.Expr.IfThenElse(e1, e2, e3, tpe, eff, loc)

      case MonoAst.Expr.Stm(exp1, exp2, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        OccurrenceAst1.Expr.Stm(e1, e2, tpe, eff, loc)

      case MonoAst.Expr.Discard(exp, eff, loc) =>
        val e = visitExp(exp)
        OccurrenceAst1.Expr.Discard(e, eff, loc)

      case MonoAst.Expr.Match(exp, rules, tpe, eff, loc) =>
        val e = visitExp(exp)
        val rs = rules.map {
          case MonoAst.MatchRule(pat, guard, exp1) =>
            val p = visitPattern(pat)
            val g = guard.map(visitExp)
            val e1 = visitExp(exp1)
            OccurrenceAst1.MatchRule(p, g, e1)
        }
        OccurrenceAst1.Expr.Match(e, rs, tpe, eff, loc)

      case MonoAst.Expr.VectorLit(exps, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        OccurrenceAst1.Expr.VectorLit(es, tpe, eff, loc)

      case MonoAst.Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        OccurrenceAst1.Expr.VectorLoad(e1, e2, tpe, eff, loc)

      case MonoAst.Expr.VectorLength(exp, loc) =>
        val e = visitExp(exp)
        OccurrenceAst1.Expr.VectorLength(e, loc)

      case MonoAst.Expr.Ascribe(exp, tpe, eff, loc) =>
        val e = visitExp(exp)
        OccurrenceAst1.Expr.Ascribe(e, tpe, eff, loc)

      case MonoAst.Expr.Cast(exp, declaredType, declaredEff, tpe, eff, loc) =>
        val e = visitExp(exp)
        OccurrenceAst1.Expr.Cast(e, declaredType, declaredEff, tpe, eff, loc)

      case MonoAst.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
        val e = visitExp(exp)
        val rs = rules.map {
          case MonoAst.CatchRule(sym, clazz, exp1) =>
            val e1 = visitExp(exp1)
            OccurrenceAst1.CatchRule(sym, clazz, e1)
        }
        OccurrenceAst1.Expr.TryCatch(e, rs, tpe, eff, loc)

      case MonoAst.Expr.RunWith(exp, effUse, rules, tpe, eff, loc) =>
        val e = visitExp(exp)
        val rs = rules.map {
          case MonoAst.HandlerRule(op, fparams, exp1) =>
            val e1 = visitExp(exp1)
            OccurrenceAst1.HandlerRule(op, fparams, e1, Linearity.Many)
        }
        OccurrenceAst1.Expr.RunWith(e, effUse, rs, tpe, eff, loc)

      case MonoAst.Expr.Do(op, exps, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        OccurrenceAst1.Expr.Do(op, es, tpe, eff, loc)

      case MonoAst.Expr.NewObject(name, clazz, tpe, eff, methods, loc) =>
        val ms = methods.map {
          case MonoAst.JvmMethod(ident, fparams, exp1, retTpe, eff1, loc1) =>
            val e1 = visitExp(exp1)
            OccurrenceAst1.JvmMethod(ident, fparams, e1, retTpe, eff1, loc1)
        }
        OccurrenceAst1.Expr.NewObject(name, clazz, tpe, eff, ms, loc)
    }

    private def visitPattern(pat0: MonoAst.Pattern): OccurrenceAst1.Pattern = pat0 match {
      case MonoAst.Pattern.Wild(tpe, loc) =>
        OccurrenceAst1.Pattern.Wild(tpe, loc)

      case MonoAst.Pattern.Var(sym, tpe, loc) =>
        OccurrenceAst1.Pattern.Var(sym, tpe, OccurrenceAst1.Occur.Dead, loc)

      case MonoAst.Pattern.Cst(cst, tpe, loc) =>
        OccurrenceAst1.Pattern.Cst(cst, tpe, loc)

      case MonoAst.Pattern.Tag(sym, pats, tpe, loc) =>
        val ps = pats.map(visitPattern)
        OccurrenceAst1.Pattern.Tag(sym, ps, tpe, loc)

      case MonoAst.Pattern.Tuple(pats, tpe, loc) =>
        val ps = pats.map(visitPattern)
        OccurrenceAst1.Pattern.Tuple(ps, tpe, loc)

      case MonoAst.Pattern.Record(pats, pat, tpe, loc) =>
        val ps = pats.map {
          case MonoAst.Pattern.Record.RecordLabelPattern(label, pat1, tpe1, loc1) =>
            val p1 = visitPattern(pat1)
            OccurrenceAst1.Pattern.Record.RecordLabelPattern(label, p1, tpe1, loc1)
        }
        val p = visitPattern(pat)
        OccurrenceAst1.Pattern.Record(ps, p, tpe, loc)
    }
  }

  private object ToMonoAst {
    def run(root: OccurrenceAst1.Root)(implicit flix: Flix): MonoAst.Root = {
      val defs = ParOps.parMapValues(root.defs)(visitDef)
      MonoAst.Root(defs, root.enums, root.structs, root.effects, root.mainEntryPoint, root.entryPoints, root.sources)
    }

    private def visitDef(def0: OccurrenceAst1.Def): MonoAst.Def = def0 match {
      case OccurrenceAst1.Def(sym, _, spec, exp, _, loc) =>
        val e = visitExp(exp)
        MonoAst.Def(sym, spec, e, loc)
    }

    private def visitExp(expr0: OccurrenceAst1.Expr): MonoAst.Expr = expr0 match {
      case OccurrenceAst1.Expr.Cst(cst, tpe, loc) =>
        MonoAst.Expr.Cst(cst, tpe, loc)

      case OccurrenceAst1.Expr.Var(sym, tpe, loc) =>
        MonoAst.Expr.Var(sym, tpe, loc)

      case OccurrenceAst1.Expr.Lambda((fparam, _), exp, tpe, loc) =>
        val e = visitExp(exp)
        MonoAst.Expr.Lambda(fparam, e, tpe, loc)

      case OccurrenceAst1.Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        MonoAst.Expr.ApplyAtomic(op, es, tpe, eff, loc)

      case OccurrenceAst1.Expr.ApplyClo(exp1, exp2, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        MonoAst.Expr.ApplyClo(e1, e2, tpe, eff, loc)

      case OccurrenceAst1.Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        MonoAst.Expr.ApplyDef(sym, es, itpe, tpe, eff, loc)

      case OccurrenceAst1.Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        MonoAst.Expr.ApplyLocalDef(sym, es, tpe, eff, loc)

      case OccurrenceAst1.Expr.Let(sym, exp1, exp2, tpe, eff, _, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        MonoAst.Expr.Let(sym, e1, e2, tpe, eff, loc)

      case OccurrenceAst1.Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, _, loc) =>
        val fps = fparams.map { case (fp, _) => fp }
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        MonoAst.Expr.LocalDef(sym, fps, e1, e2, tpe, eff, loc)

      case OccurrenceAst1.Expr.Scope(sym, regSym, exp, tpe, eff, loc) =>
        val e = visitExp(exp)
        MonoAst.Expr.Scope(sym, regSym, e, tpe, eff, loc)

      case OccurrenceAst1.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        val e3 = visitExp(exp3)
        MonoAst.Expr.IfThenElse(e1, e2, e3, tpe, eff, loc)

      case OccurrenceAst1.Expr.Stm(exp1, exp2, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        MonoAst.Expr.Stm(e1, e2, tpe, eff, loc)

      case OccurrenceAst1.Expr.Discard(exp, eff, loc) =>
        val e = visitExp(exp)
        MonoAst.Expr.Discard(e, eff, loc)

      case OccurrenceAst1.Expr.Match(exp, rules, tpe, eff, loc) =>
        val e = visitExp(exp)
        val rs = rules.map {
          case OccurrenceAst1.MatchRule(pat, guard, exp1) =>
            val p = visitPattern(pat)
            val g = guard.map(visitExp)
            val e1 = visitExp(exp1)
            MonoAst.MatchRule(p, g, e1)
        }
        MonoAst.Expr.Match(e, rs, tpe, eff, loc)

      case OccurrenceAst1.Expr.VectorLit(exps, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        MonoAst.Expr.VectorLit(es, tpe, eff, loc)

      case OccurrenceAst1.Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        MonoAst.Expr.VectorLoad(e1, e2, tpe, eff, loc)

      case OccurrenceAst1.Expr.VectorLength(exp, loc) =>
        val e = visitExp(exp)
        MonoAst.Expr.VectorLength(e, loc)

      case OccurrenceAst1.Expr.Ascribe(exp, tpe, eff, loc) =>
        val e = visitExp(exp)
        MonoAst.Expr.Ascribe(e, tpe, eff, loc)

      case OccurrenceAst1.Expr.Cast(exp, declaredType, declaredEff, tpe, eff, loc) =>
        val e = visitExp(exp)
        MonoAst.Expr.Cast(e, declaredType, declaredEff, tpe, eff, loc)

      case OccurrenceAst1.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
        val e = visitExp(exp)
        val rs = rules.map {
          case OccurrenceAst1.CatchRule(sym, clazz, exp1) =>
            val e1 = visitExp(exp1)
            MonoAst.CatchRule(sym, clazz, e1)
        }
        MonoAst.Expr.TryCatch(e, rs, tpe, eff, loc)

      case OccurrenceAst1.Expr.RunWith(exp, effUse, rules, tpe, eff, loc) =>
        val e = visitExp(exp)
        val rs = rules.map {
          case OccurrenceAst1.HandlerRule(op, fparams, exp1, _) =>
            val e1 = visitExp(exp1)
            MonoAst.HandlerRule(op, fparams, e1)
        }
        MonoAst.Expr.RunWith(e, effUse, rs, tpe, eff, loc)

      case OccurrenceAst1.Expr.Do(op, exps, tpe, eff, loc) =>
        val es = exps.map(visitExp)
        MonoAst.Expr.Do(op, es, tpe, eff, loc)

      case OccurrenceAst1.Expr.NewObject(name, clazz, tpe, eff, methods, loc) =>
        val ms = methods.map {
          case OccurrenceAst1.JvmMethod(ident, fparams, exp1, retTpe, eff1, loc1) =>
            val e1 = visitExp(exp1)
            MonoAst.JvmMethod(ident, fparams, e1, retTpe, eff1, loc1)
        }
        MonoAst.Expr.NewObject(name, clazz, tpe, eff, ms, loc)
    }

    private def visitPattern(pat0: OccurrenceAst1.Pattern): MonoAst.Pattern = pat0 match {
      case OccurrenceAst1.Pattern.Wild(tpe, loc) =>
        MonoAst.Pattern.Wild(tpe, loc)

      case OccurrenceAst1.Pattern.Var(sym, tpe, _, loc) =>
        MonoAst.Pattern.Var(sym, tpe, loc)

      case OccurrenceAst1.Pattern.Cst(cst, tpe, loc) =>
        MonoAst.Pattern.Cst(cst, tpe, loc)

      case OccurrenceAst1.Pattern.Tag(sym, pats, tpe, loc) =>
        val ps = pats.map(visitPattern)
        MonoAst.Pattern.Tag(sym, ps, tpe, loc)

      case OccurrenceAst1.Pattern.Tuple(pats, tpe, loc) =>
        val ps = pats.map(visitPattern)
        MonoAst.Pattern.Tuple(ps, tpe, loc)

      case OccurrenceAst1.Pattern.Record(pats, pat, tpe, loc) =>
        val ps = pats.map {
          case OccurrenceAst1.Pattern.Record.RecordLabelPattern(label, pat1, tpe1, loc1) =>
            val p1 = visitPattern(pat1)
            MonoAst.Pattern.Record.RecordLabelPattern(label, p1, tpe1, loc1)
        }
        val p = visitPattern(pat)
        MonoAst.Pattern.Record(ps, p, tpe, loc)
    }
  }

  case class Stats(inlinedDefs: ListMap[Symbol.DefnSym, Symbol.DefnSym],
                   inlinedVars: ListMap[Symbol.DefnSym, Symbol.VarSym],
                   betaReductions: Map[Symbol.DefnSym, Int],
                   eliminatedVars: ListMap[Symbol.DefnSym, Symbol.VarSym],
                   simplifiedIfThenElse: Map[Symbol.DefnSym, Int],
                   eliminatedStms: Map[Symbol.DefnSym, Int]) {
    def ++(that: Stats): Stats = {
      val inlinedDefs1 = inlinedDefs ++ that.inlinedDefs
      val inlinedVars1 = inlinedVars ++ that.inlinedVars
      val betaReductions1 = Stats.merge(betaReductions, that.betaReductions)(_ + _)
      val eliminatedVars1 = eliminatedVars ++ that.eliminatedVars
      val simplifiedIfThenElse1 = Stats.merge(simplifiedIfThenElse, that.simplifiedIfThenElse)(_ + _)
      val eliminatedStms1 = Stats.merge(eliminatedStms, that.eliminatedStms)(_ + _)
      Stats(inlinedDefs1, inlinedVars1, betaReductions1, eliminatedVars1, simplifiedIfThenElse1, eliminatedStms1)
    }

    override def toString: String = {
      s"""====== STATISTICS ======
         |
         |Inlined Defs: $inlinedDefs
         |
         |Inlined Vars: $inlinedVars
         |
         |Eliminated Vars: $eliminatedVars
         |
         |Beta Reductions: $betaReductions
         |
         |Eliminated Stms: $eliminatedStms
         |
         |Eliminated Ifs: $simplifiedIfThenElse
         |""".stripMargin
    }
  }

  object Stats {
    def merge[A, B](m1: Map[A, B], m2: Map[A, B])(combine: (B, B) => B): Map[A, B] = {
      val smallest = if (m1.size < m2.size) m1 else m2
      val biggest = if (m1.size >= m2.size) m1 else m2
      smallest.foldLeft(biggest) {
        case (acc, (k, v1)) => acc.get(k) match {
          case Some(v2) => acc + (k -> combine(v1, v2))
          case None => acc + (k -> v1)
        }
      }
    }

    def toCount[A](iterable: Iterable[(A, Int)]): Map[A, Int] = {
      iterable.foldLeft(Map.empty[A, Int]) {
        case (m, (k, i)) => m.get(k) match {
          case Some(j) => m + (k -> (i + j))
          case None => m + (k -> i)
        }
      }
    }
  }
}
