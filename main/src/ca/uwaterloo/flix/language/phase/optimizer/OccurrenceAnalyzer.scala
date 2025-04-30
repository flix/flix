/*
 * Copyright 2022 Anna Krogh, Patrick Lundvig, Christian Bonde
 * Copyright 2025 Jakob Schneider Villumsen
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

package ca.uwaterloo.flix.language.phase.optimizer

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.OccurrenceAst.{DefContext, Occur}
import ca.uwaterloo.flix.language.ast.Symbol.VarSym
import ca.uwaterloo.flix.language.ast.{OccurrenceAst, Symbol}
import ca.uwaterloo.flix.util.ParOps

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.CollectionHasAsScala

/**
  * The occurrence analyzer collects occurrence information on binders according to the definition of [[Occur]].
  * Additionally, it also counts the number of subexpressions in a function to compute its size.
  */
object OccurrenceAnalyzer {

  /**
    * Performs occurrence analysis on the given AST `root`.
    */
  def run(root: OccurrenceAst.Root, delta: Set[Symbol.DefnSym])(implicit flix: Flix): (OccurrenceAst.Root, Set[Symbol.DefnSym]) = {
    implicit val sctx: SharedContext = SharedContext.mk()
    val changed = root.defs.filter(kv => delta.contains(kv._1))
    val visitedDefs = ParOps.parMapValues(changed)(visitDef)
    val defs = root.defs ++ visitedDefs
    val newDelta = sctx.changed.asScala.toSet
    (root.copy(defs = defs), newDelta)
  }

  /**
    * Performs occurrence analysis on `defn`.
    */
  private def visitDef(defn: OccurrenceAst.Def)(implicit sctx: SharedContext): OccurrenceAst.Def = {
    val lctx: LocalContext = LocalContext.mk()
    val (exp, ctx) = visitExp(defn.exp)(defn.sym, lctx, sctx)
    if (!(exp eq defn.exp)) {
      sctx.changed.add(defn.sym)
    }
    val defContext = DefContext(lctx.localDefs.get(), isDirectCall(exp), isSelfRecursive(ctx.selfOccur))
    val fparams = defn.fparams.map(visitFormalParam(_, ctx))
    OccurrenceAst.Def(defn.sym, fparams, defn.spec, exp, defContext, defn.loc)
  }

  /**
    * Performs occurrence analysis on `exp0`
    */
  private def visitExp(exp0: OccurrenceAst.Expr)(implicit sym0: Symbol.DefnSym, lctx: LocalContext, sctx: SharedContext): (OccurrenceAst.Expr, ExprContext) = {
    exp0 match {
      case OccurrenceAst.Expr.Cst(_, _, _) =>
        (exp0, ExprContext.Empty)

      case OccurrenceAst.Expr.Var(sym, _, _) =>
        (exp0, ExprContext.Empty.addVar(sym, Occur.Once))

      case OccurrenceAst.Expr.Lambda(fparam, exp, tpe, loc) =>
        val (e, ctx1) = visitExp(exp)
        val ctx2 = ctx1.map {
          case Occur.Once => Occur.OnceInLambda
          case o => o
        }
        val fp = visitFormalParam(fparam, ctx2)
        val ctx3 = ctx2.removeVar(fp.sym)
        if ((e eq exp) && (fp eq fparam)) {
          (exp0, ctx3) // Reuse exp0.
        } else {
          (OccurrenceAst.Expr.Lambda(fp, e, tpe, loc), ctx3)
        }

      case OccurrenceAst.Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
        val (es, ctxs) = exps.map(visitExp).unzip
        val ctx = ctxs.foldLeft(ExprContext.Empty)(combineSeq)
        if (exps.zip(es).forall { case (e1, e2) => e1 eq e2 }) {
          (exp0, ctx) // Reuse exp0.
        } else {
          (OccurrenceAst.Expr.ApplyAtomic(op, es, tpe, eff, loc), ctx)
        }

      case OccurrenceAst.Expr.ApplyClo(exp1, exp2, tpe, eff, loc) =>
        val (e1, ctx1) = visitExp(exp1)
        val (e2, ctx2) = visitExp(exp2)
        val ctx3 = combineSeq(ctx1, ctx2)
        if ((e1 eq exp1) && (e2 eq exp2)) {
          (exp0, ctx3) // Reuse exp0.
        } else {
          (OccurrenceAst.Expr.ApplyClo(e1, e2, tpe, eff, loc), ctx3)
        }

      case OccurrenceAst.Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
        val (es, ctxs) = exps.map(visitExp).unzip
        val ctx1 = if (sym == sym0) ExprContext.RecursiveOnce else ExprContext.Empty
        val ctx2 = ctxs.foldLeft(ctx1)(combineSeq)
        if (exps.zip(es).forall { case (e1, e2) => e1 eq e2 }) {
          (exp0, ctx2) // Reuse exp0.
        } else {
          (OccurrenceAst.Expr.ApplyDef(sym, es, itpe, tpe, eff, loc), ctx2)
        }

      case OccurrenceAst.Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
        val (es, ctxs) = exps.map(visitExp).unzip
        val ctx = ctxs.foldLeft(ExprContext.Empty)(combineSeq).addVar(sym, Occur.Once)
        if (exps.zip(es).forall { case (e1, e2) => e1 eq e2 }) {
          (exp0, ctx) // Reuse exp0.
        } else {
          (OccurrenceAst.Expr.ApplyLocalDef(sym, es, tpe, eff, loc), ctx)
        }

      case OccurrenceAst.Expr.Let(sym, exp1, exp2, tpe, eff, occur0, loc) =>
        val (e1, ctx1) = visitExp(exp1)
        val (e2, ctx2) = visitExp(exp2)
        val ctx3 = combineSeq(ctx1, ctx2)
        val occur = ctx3.get(sym)
        val ctx4 = ctx3.removeVar(sym)
        if ((e1 eq exp1) && (e2 eq exp2) && (occur eq occur0)) {
          (exp0, ctx4) // Reuse exp0.
        } else {
          (OccurrenceAst.Expr.Let(sym, e1, e2, tpe, eff, occur, loc), ctx4)
        }

      case OccurrenceAst.Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, occur0, loc) =>
        val (e1, ctx1) = visitExp(exp1)
        val ctx2 = ctx1.map {
          case Occur.Once => Occur.OnceInLocalDef
          case o => o
        }
        val fps = fparams.map(visitFormalParam(_, ctx2))
        val ctx3 = ctx2.removeVars(fps.map(_.sym))
        val (e2, ctx4) = visitExp(exp2)
        val ctx5 = combineSeq(ctx3, ctx4)
        val occur = ctx5.get(sym)
        val ctx6 = ctx5.removeVar(sym)
        lctx.localDefs.incrementAndGet()
        if ((e1 eq exp1) && (e2 eq exp2) && fparams.zip(fps).forall { case (fp1, fp2) => fp1 eq fp2 } && (occur eq occur0)) {
          (exp0, ctx6) // Reuse exp0.
        } else {
          (OccurrenceAst.Expr.LocalDef(sym, fps, e1, e2, tpe, eff, occur, loc), ctx6)
        }

      case OccurrenceAst.Expr.Scope(sym, rsym, exp, tpe, eff, loc) =>
        val (e, ctx) = visitExp(exp)
        if (e eq exp) {
          (exp0, ctx) // Reuse exp0.
        } else {
          (OccurrenceAst.Expr.Scope(sym, rsym, e, tpe, eff, loc), ctx)
        }

      case OccurrenceAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
        val (e1, ctx1) = visitExp(exp1)
        val (e2, ctx2) = visitExp(exp2)
        val (e3, ctx3) = visitExp(exp3)
        val ctx4 = combineSeq(ctx1, combineBranch(ctx2, ctx3))
        if ((e1 eq exp1) && (e2 eq exp2) && (e3 eq exp3)) {
          (exp0, ctx4) // Reuse exp0.
        } else {
          (OccurrenceAst.Expr.IfThenElse(e1, e2, e3, tpe, eff, loc), ctx4)
        }

      case OccurrenceAst.Expr.Stm(exp1, exp2, tpe, eff, loc) =>
        val (e1, ctx1) = visitExp(exp1)
        val (e2, ctx2) = visitExp(exp2)
        val ctx3 = combineSeq(ctx1, ctx2)
        if ((e1 eq exp1) && (e2 eq exp2)) {
          (exp0, ctx3) // Reuse exp0.
        } else {
          (OccurrenceAst.Expr.Stm(e1, e2, tpe, eff, loc), ctx3)
        }

      case OccurrenceAst.Expr.Discard(exp, eff, loc) =>
        val (e, ctx) = visitExp(exp)
        if (e eq exp) {
          (exp0, ctx) // Reuse exp0.
        } else {
          (OccurrenceAst.Expr.Discard(e, eff, loc), ctx)
        }

      case OccurrenceAst.Expr.Match(exp, rules, tpe, eff, loc) =>
        val (e, ctx1) = visitExp(exp)
        val (rs, ctxs) = rules.map(visitMatchRule).unzip
        val ctx2 = ctxs.foldLeft(ExprContext.Empty)(combineBranch)
        val ctx3 = combineSeq(ctx1, ctx2)
        if ((e eq exp) && rules.zip(rs).forall { case (r1, r2) => r1 eq r2 }) {
          (exp0, ctx3) // Reuse exp0.
        } else {
          (OccurrenceAst.Expr.Match(e, rs, tpe, eff, loc), ctx3)
        }

      case OccurrenceAst.Expr.VectorLit(exps, tpe, eff, loc) =>
        val (es, ctxs) = exps.map(visitExp).unzip
        val ctx = ctxs.foldLeft(ExprContext.Empty)(combineSeq)
        if (exps.zip(es).forall { case (e1, e2) => e1 eq e2 }) {
          (exp0, ctx) // Reuse exp0.
        } else {
          (OccurrenceAst.Expr.VectorLit(es, tpe, eff, loc), ctx)
        }

      case OccurrenceAst.Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
        val (e1, ctx1) = visitExp(exp1)
        val (e2, ctx2) = visitExp(exp2)
        val ctx3 = combineSeq(ctx1, ctx2)
        if ((e1 eq exp1) && (e2 eq exp2)) {
          (exp0, ctx3) // Reuse exp0.
        } else {
          (OccurrenceAst.Expr.VectorLoad(e1, e2, tpe, eff, loc), ctx3)
        }

      case OccurrenceAst.Expr.VectorLength(exp, loc) =>
        val (e, ctx) = visitExp(exp)
        if (e eq exp) {
          (exp0, ctx) // Reuse exp0.
        } else {
          (OccurrenceAst.Expr.VectorLength(e, loc), ctx)
        }

      case OccurrenceAst.Expr.Ascribe(exp, tpe, eff, loc) =>
        val (e, ctx) = visitExp(exp)
        if (e eq exp) {
          (exp0, ctx) // Reuse exp0.
        } else {
          (OccurrenceAst.Expr.Ascribe(e, tpe, eff, loc), ctx)
        }

      case OccurrenceAst.Expr.Cast(exp, declaredType, declaredEff, tpe, eff, loc) =>
        val (e, ctx) = visitExp(exp)
        if (e eq exp) {
          (exp0, ctx) // Reuse exp0.
        } else {
          (OccurrenceAst.Expr.Cast(e, declaredType, declaredEff, tpe, eff, loc), ctx)
        }

      case OccurrenceAst.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
        val (e, ctx1) = visitExp(exp)
        val (rs, ctxs) = rules.map(visitCatchRule).unzip
        val ctx2 = ctxs.foldLeft(ExprContext.Empty)(combineBranch)
        val ctx3 = combineSeq(ctx1, ctx2)
        if ((e eq exp) && rules.zip(rs).forall { case (r1, r2) => r1 eq r2 }) {
          (exp0, ctx3) // Reuse exp0.
        } else {
          (OccurrenceAst.Expr.TryCatch(e, rs, tpe, eff, loc), ctx3)
        }

      case OccurrenceAst.Expr.RunWith(exp, effUse, rules, tpe, eff, loc) =>
        val (e, ctx1) = visitExp(exp)
        val (rs, ctxs) = rules.map(visitHandlerRule).unzip
        val ctx2 = ctxs.foldLeft(ExprContext.Empty)(combineBranch)
        val ctx3 = combineSeq(ctx1, ctx2)
        if ((e eq exp) && rules.zip(rs).forall { case (r1, r2) => r1 eq r2 }) {
          (exp0, ctx3) // Reuse exp0.
        } else {
          (OccurrenceAst.Expr.RunWith(e, effUse, rs, tpe, eff, loc), ctx3)
        }

      case OccurrenceAst.Expr.Do(op, exps, tpe, eff, loc) =>
        val (es, ctxs) = exps.map(visitExp).unzip
        val ctx = ctxs.foldLeft(ExprContext.Empty)(combineSeq)
        if (exps.zip(es).forall { case (e1, e2) => e1 eq e2 }) {
          (exp0, ctx) // Reuse exp0.
        } else {
          (OccurrenceAst.Expr.Do(op, es, tpe, eff, loc), ctx)
        }

      case OccurrenceAst.Expr.NewObject(name, clazz, tpe, eff, methods, loc) =>
        val (ms, ctxs) = methods.map(visitJvmMethod).unzip
        val ctx = ctxs.foldLeft(ExprContext.Empty)(combineBranch)
        if (methods.zip(ms).forall { case (m1, m2) => m1 eq m2 }) {
          (exp0, ctx) // Reuse exp0.
        } else {
          (OccurrenceAst.Expr.NewObject(name, clazz, tpe, eff, ms, loc), ctx)
        }
    }
  }

  private def visitMatchRule(rule: OccurrenceAst.MatchRule)(implicit sym0: Symbol.DefnSym, lctx: LocalContext, sctx: SharedContext): (OccurrenceAst.MatchRule, ExprContext) = rule match {
    case OccurrenceAst.MatchRule(pat, guard, exp) =>
      val (g, ctx1) = guard.map(visitExp).unzip
      val (e, ctx2) = visitExp(exp)
      val ctx3 = combineSeqOpt(ctx1, ctx2)
      val (p, syms) = visitPattern(pat, ctx3)
      val ctx4 = ctx3.removeVars(syms)
      if ((p eq pat) && (g eq guard) && (e eq exp)) {
        (rule, ctx4) // Reuse rule.
      } else {
        (OccurrenceAst.MatchRule(p, g, e), ctx4)
      }
  }

  private def visitCatchRule(rule: OccurrenceAst.CatchRule)(implicit sym0: Symbol.DefnSym, lctx: LocalContext, sctx: SharedContext): (OccurrenceAst.CatchRule, ExprContext) = rule match {
    case OccurrenceAst.CatchRule(sym, clazz, exp) =>
      val (e, ctx1) = visitExp(exp)
      val ctx2 = ctx1.removeVar(sym)
      if (e eq exp) {
        (rule, ctx2) // Reuse rule.
      } else {
        (OccurrenceAst.CatchRule(sym, clazz, e), ctx2)
      }
  }

  private def visitHandlerRule(rule: OccurrenceAst.HandlerRule)(implicit sym0: Symbol.DefnSym, lctx: LocalContext, sctx: SharedContext): (OccurrenceAst.HandlerRule, ExprContext) = rule match {
    case OccurrenceAst.HandlerRule(op, fparams, exp) =>
      val (e, ctx1) = visitExp(exp)
      val fps = fparams.map(visitFormalParam(_, ctx1))
      val ctx2 = ctx1.removeVars(fps.map(_.sym))
      if ((e eq exp) && fparams.zip(fps).forall { case (fp1, fp2) => fp1 eq fp2 }) {
        (rule, ctx2) // Reuse rule.
      } else {
        (OccurrenceAst.HandlerRule(op, fps, e), ctx2)
      }
  }

  private def visitJvmMethod(method: OccurrenceAst.JvmMethod)(implicit sym0: Symbol.DefnSym, lctx: LocalContext, sctx: SharedContext): (OccurrenceAst.JvmMethod, ExprContext) = method match {
    case OccurrenceAst.JvmMethod(ident, fparams, exp, retTpe, eff, loc) =>
      val (e, ctx1) = visitExp(exp)
      val fps = fparams.map(visitFormalParam(_, ctx1))
      val ctx2 = ctx1.removeVars(fps.map(_.sym))
      if ((e eq exp) && fparams.zip(fps).forall { case (fp1, fp2) => fp1 eq fp2 }) {
        (method, ctx2) // Reuse method.
      } else {
        (OccurrenceAst.JvmMethod(ident, fparams, e, retTpe, eff, loc), ctx2)
      }
  }

  private def visitPattern(pat0: OccurrenceAst.Pattern, ctx: ExprContext): (OccurrenceAst.Pattern, Set[VarSym]) = pat0 match {
    case OccurrenceAst.Pattern.Wild(_, _) =>
      (pat0, Set.empty)

    case OccurrenceAst.Pattern.Var(sym, tpe, _, loc) =>
      val occur = ctx.get(sym)
      (OccurrenceAst.Pattern.Var(sym, tpe, occur, loc), Set(sym))

    case OccurrenceAst.Pattern.Cst(_, _, _) =>
      (pat0, Set.empty)

    case OccurrenceAst.Pattern.Tag(sym, pats, tpe, loc) =>
      val (ps, listOfSyms) = pats.map(visitPattern(_, ctx)).unzip
      val syms = listOfSyms.flatten.toSet
      (OccurrenceAst.Pattern.Tag(sym, ps, tpe, loc), syms)

    case OccurrenceAst.Pattern.Tuple(pats, tpe, loc) =>
      val (ps, listOfSyms) = pats.map(visitPattern(_, ctx)).unzip
      val syms = listOfSyms.flatten.toSet
      (OccurrenceAst.Pattern.Tuple(ps, tpe, loc), syms)

    case OccurrenceAst.Pattern.Record(pats, pat, tpe, loc) =>
      val (ps, listOfSyms) = pats.map(visitRecordLabelPattern(_, ctx)).unzip
      val (p, syms0) = visitPattern(pat, ctx)
      val syms = listOfSyms.flatten.toSet ++ syms0
      (OccurrenceAst.Pattern.Record(ps, p, tpe, loc), syms)

  }

  private def visitRecordLabelPattern(pat0: OccurrenceAst.Pattern.Record.RecordLabelPattern, ctx: ExprContext): (OccurrenceAst.Pattern.Record.RecordLabelPattern, Set[VarSym]) = pat0 match {
    case OccurrenceAst.Pattern.Record.RecordLabelPattern(label, pat, tpe, loc) =>
      val (p, syms) = visitPattern(pat, ctx)
      (OccurrenceAst.Pattern.Record.RecordLabelPattern(label, p, tpe, loc), syms)
  }

  private def visitFormalParam(fparam0: OccurrenceAst.FormalParam, ctx: ExprContext): OccurrenceAst.FormalParam = {
    val occur = ctx.get(fparam0.sym)
    if (occur eq fparam0.occur) {
      fparam0
    } else {
      fparam0.copy(occur = occur)
    }
  }

  /**
    * Combines `ctx1` and `ctx2` into a single [[ExprContext]] using [[combineSeq]] to merge [[Occur]].
    */
  private def combineSeq(ctx1: ExprContext, ctx2: ExprContext): ExprContext = {
    combine(ctx1, ctx2, combineSeq)
  }

  /**
    * Combines `ctx1` and `ctx2` into a single [[ExprContext]] using [[combineSeq]] to merge [[Occur]].
    *
    * If `ctx1` is [[None]] then `ctx2` is returned.
    */
  private def combineSeqOpt(ctx1: Option[ExprContext], ctx2: ExprContext): ExprContext = {
    ctx1.map(combineSeq(_, ctx2)).getOrElse(ctx2)
  }

  /**
    * Combines `ctx1` and `ctx2` into a single [[ExprContext]] using [[combineBranch]] to merge [[Occur]].
    */
  private def combineBranch(ctx1: ExprContext, ctx2: ExprContext): ExprContext = {
    combine(ctx1, ctx2, combineBranch)
  }

  /**
    * Combines `ctx1` and `ctx2` into a single [[ExprContext]] using `combine` to merge [[Occur]].
    */
  private def combine(ctx1: ExprContext, ctx2: ExprContext, combine: (Occur, Occur) => Occur): ExprContext = {
    if (ctx1 eq ExprContext.Empty) {
      return ctx2
    }
    if (ctx2 eq ExprContext.Empty) {
      return ctx1
    }

    val selfOccur = combine(ctx1.selfOccur, ctx2.selfOccur)
    val varMap = combineMaps(ctx1.vars, ctx2.vars, combine)
    ExprContext(selfOccur, varMap)
  }

  /**
    * Combines maps `m1` and `m2` into a single map using `combine` to merge [[Occur]].
    */
  private def combineMaps[A](m1: Map[A, Occur], m2: Map[A, Occur], combine: (Occur, Occur) => Occur): Map[A, Occur] = {
    if (m1.isEmpty) {
      return m2
    }
    if (m2.isEmpty) {
      return m1
    }

    val (smallest, largest) = if (m1.size < m2.size) (m1, m2) else (m2, m1)
    smallest.foldLeft[Map[A, Occur]](largest) {
      case (acc, (k, v)) =>
        val occur = combine(v, acc.getOrElse(k, Occur.Dead))
        acc + (k -> occur)
    }
  }

  /**
    * Combines two occurrences `o1` and `o2` from the same branch into a single occurrence.
    *
    * If none of the occurrences are [[Occur.Dead]] then they are merged as [[Occur.Many]].
    */
  private def combineSeq(o1: Occur, o2: Occur): Occur = (o1, o2) match {
    case (Occur.Dead, _) => o2
    case (_, Occur.Dead) => o1
    case _ => Occur.Many
  }

  /**
    * Combines two occurrences `o1` and `o2` from distinct branches into a single occurrence.
    *
    * If none of the occurrences are [[Occur.Dead]] then they are merged as [[Occur.Many]],
    * except if both occurrences are [[Occur.Once]] then they are merged as [[Occur.ManyBranch]].
    */
  private def combineBranch(o1: Occur, o2: Occur): Occur = (o1, o2) match {
    case (Occur.Dead, _) => o2
    case (_, Occur.Dead) => o1
    case (Occur.Once, Occur.Once) => Occur.ManyBranch
    case _ => Occur.Many
  }

  private object ExprContext {

    /** Context for an empty sequence of expressions. */
    val Empty: ExprContext = ExprContext(Occur.Dead, Map.empty)

    /** Context for a self-recursive call. */
    val RecursiveOnce: ExprContext = ExprContext(Occur.Once, Map.empty)

  }

  /**
    * Stores various pieces of information extracted from an expression.
    *
    * @param selfOccur Occurrence information on how the function occurs in its own definition.
    * @param vars      A map from variable symbols to occurrence information (this also includes uses of [[OccurrenceAst.Expr.LocalDef]]).
    *                  If the map does not contain a certain symbol, then the symbol is [[Occur.Dead]].
    */
  case class ExprContext(selfOccur: Occur, vars: Map[VarSym, Occur]) {

    /**
      * Returns the occurrence information collected on `sym`.
      * If [[vars]] does not contain `sym`, then it is [[Occur.Dead]].
      */
    def get(sym: VarSym): Occur = {
      this.vars.getOrElse(sym, Occur.Dead)
    }

    /** Returns a new [[ExprContext]] with the mapping `sym -> occur` added to [[vars]]. */
    def addVar(sym: VarSym, occur: Occur): ExprContext = {
      this.copy(vars = this.vars + (sym -> occur))
    }

    /** Returns a new [[ExprContext]] with `sym` and the corresponding value removed from [[vars]]. */
    def removeVar(sym: VarSym): ExprContext = {
      this.copy(vars = this.vars - sym)
    }

    /** Returns a new [[ExprContext]] with `syms` and the corresponding values removed from [[vars]]. */
    def removeVars(syms: Iterable[VarSym]): ExprContext = {
      this.copy(vars = this.vars -- syms)
    }

    /** Applies `f` to each value in `vars`, i.e., maps `(k, v) > (k, f(v))`. */
    def map(f: Occur => Occur): ExprContext = {
      val newVars = vars.map {
        case (k, v) => (k, f(v))
      }
      this.copy(vars = newVars)
    }
  }

  /**
    * Returns true if `expr0` is a function call.
    */
  private def isDirectCall(expr0: OccurrenceAst.Expr): Boolean = expr0 match {
    case OccurrenceAst.Expr.ApplyDef(_, _, _, _, _, _) => true
    case OccurrenceAst.Expr.ApplyClo(_, _, _, _, _) => true
    case _ => false
  }

  /**
    * Returns true if `defn` occurs in `ctx`.
    */
  private def isSelfRecursive(occur: Occur): Boolean = occur match {
    case Occur.Dead => false
    case Occur.Once => true
    case Occur.OnceInLambda => true
    case Occur.OnceInLocalDef => true
    case Occur.Many => true
    case Occur.ManyBranch => true
  }

  /**
    * Companion object for [[LocalContext]].
    */
  private object LocalContext {

    /**
      * Returns a fresh [[LocalContext]].
      */
    def mk(): LocalContext = new LocalContext(new AtomicInteger(0))

  }

  /**
    * A local context, scoped for each function definition.
    * No requirements on thread-safety since it is scoped.
    *
    * @param localDefs The number of declared [[OccurrenceAst.Expr.LocalDef]]s in the expression.
    *                  Must be mutable.
    */
  private case class LocalContext(localDefs: AtomicInteger)

  private object SharedContext {

    def mk(): SharedContext = new SharedContext(new ConcurrentLinkedQueue())

  }

  private case class SharedContext(changed: ConcurrentLinkedQueue[Symbol.DefnSym])

}
