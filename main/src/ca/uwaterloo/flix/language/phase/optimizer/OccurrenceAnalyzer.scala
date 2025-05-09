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
import ca.uwaterloo.flix.language.ast.MonoAst.{DefContext, Expr, Occur}
import ca.uwaterloo.flix.language.ast.Symbol.VarSym
import ca.uwaterloo.flix.language.ast.{MonoAst, SourceLocation, Symbol}
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

import java.util.concurrent.atomic.AtomicInteger

/**
  * The occurrence analyzer collects occurrence information on binders according to the definition of [[Occur]].
  * Additionally, it also counts the number of subexpressions in a function to compute its size.
  */
object OccurrenceAnalyzer {

  /**
    * Performs occurrence analysis on the given AST `root`.
    */
  def run(root: MonoAst.Root, delta: Set[Symbol.DefnSym])(implicit flix: Flix): MonoAst.Root = {
    val changedDefs = root.defs.filter(kv => delta.contains(kv._1))
    val visitedDefs = ParOps.parMapValues(changedDefs)(visitDef(_)(root))
    root.copy(defs = root.defs ++ visitedDefs)
  }

  /**
    * Performs occurrence analysis on `defn`.
    */
  private def visitDef(defn: MonoAst.Def)(implicit root: MonoAst.Root): MonoAst.Def = {
    val lctx: LocalContext = LocalContext.mk()
    val (exp, ctx) = visitExp(defn.exp)(defn.sym, lctx, root)
    val defContext = DefContext(lctx.localDefs.get(), isSelfRef(ctx.selfOccur), defn.spec.defContext.refs)
    val fparams = defn.spec.fparams.map(visitFormalParam(_, ctx))
    val spec = defn.spec.copy(fparams = fparams, defContext = defContext)
    MonoAst.Def(defn.sym, spec, exp, defn.loc)
  }

  /**
    * Performs occurrence analysis on `exp0`
    */
  private def visitExp(exp0: Expr)(implicit sym0: Symbol.DefnSym, lctx: LocalContext, root: MonoAst.Root): (Expr, ExprContext) = {
    exp0 match {
      case Expr.Cst(_, _, _) =>
        (exp0, ExprContext.Empty)

      case Expr.Var(sym, _, _) =>
        (exp0, ExprContext.Empty.addVar(sym, Occur.Once))

      case Expr.Lambda(fparam, exp, tpe, loc) =>
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
          (Expr.Lambda(fp, e, tpe, loc), ctx3)
        }

      case Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
        val (es, ctxs) = exps.map(visitExp).unzip
        val ctx = ctxs.foldLeft(ExprContext.Empty)(combineSeq)
        if (exps.zip(es).forall { case (e1, e2) => e1 eq e2 }) {
          (exp0, ctx) // Reuse exp0.
        } else {
          (Expr.ApplyAtomic(op, es, tpe, eff, loc), ctx)
        }

      case Expr.ApplyClo(exp1, exp2, tpe, eff, loc) =>
        val (e1, ctx1) = visitExp(exp1)
        val (e2, ctx2) = visitExp(exp2)
        val ctx3 = combineSeq(ctx1, ctx2)
        if ((e1 eq exp1) && (e2 eq exp2)) {
          (exp0, ctx3) // Reuse exp0.
        } else {
          (Expr.ApplyClo(e1, e2, tpe, eff, loc), ctx3)
        }

      case Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
        val (es, ctxs) = exps.map(visitExp).unzip
        val ctx1 = if (sym == sym0) {
          ExprContext.RecursiveOnce
        } else {
          root.defs(sym).spec.defContext.refs.incrementAndGet()
          ExprContext.Empty
        }
        val ctx2 = ctxs.foldLeft(ctx1)(combineSeq)
        if (exps.zip(es).forall { case (e1, e2) => e1 eq e2 }) {
          (exp0, ctx2) // Reuse exp0.
        } else {
          (Expr.ApplyDef(sym, es, itpe, tpe, eff, loc), ctx2)
        }

      case Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
        val (es, ctxs) = exps.map(visitExp).unzip
        val ctx = ctxs.foldLeft(ExprContext.Empty)(combineSeq).addVar(sym, Occur.Once)
        if (exps.zip(es).forall { case (e1, e2) => e1 eq e2 }) {
          (exp0, ctx) // Reuse exp0.
        } else {
          (Expr.ApplyLocalDef(sym, es, tpe, eff, loc), ctx)
        }

      case Expr.Let(sym, exp1, exp2, tpe, eff, occur0, loc) =>
        val (e1, ctx1) = visitExp(exp1)
        val (e2, ctx2) = visitExp(exp2)
        val ctx3 = combineSeq(ctx1, ctx2)
        val occur = ctx3.get(sym)
        val ctx4 = ctx3.removeVar(sym)
        if ((e1 eq exp1) && (e2 eq exp2) && (occur eq occur0)) {
          (exp0, ctx4) // Reuse exp0.
        } else {
          (Expr.Let(sym, e1, e2, tpe, eff, occur, loc), ctx4)
        }

      case Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, occur0, loc) =>
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
          (Expr.LocalDef(sym, fps, e1, e2, tpe, eff, occur, loc), ctx6)
        }

      case Expr.Scope(sym, rsym, exp, tpe, eff, loc) =>
        val (e, ctx) = visitExp(exp)
        if (e eq exp) {
          (exp0, ctx) // Reuse exp0.
        } else {
          (Expr.Scope(sym, rsym, e, tpe, eff, loc), ctx)
        }

      case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
        val (e1, ctx1) = visitExp(exp1)
        val (e2, ctx2) = visitExp(exp2)
        val (e3, ctx3) = visitExp(exp3)
        val ctx4 = combineSeq(ctx1, combineBranch(ctx2, ctx3))
        if ((e1 eq exp1) && (e2 eq exp2) && (e3 eq exp3)) {
          (exp0, ctx4) // Reuse exp0.
        } else {
          (Expr.IfThenElse(e1, e2, e3, tpe, eff, loc), ctx4)
        }

      case Expr.Stm(exp1, exp2, tpe, eff, loc) =>
        val (e1, ctx1) = visitExp(exp1)
        val (e2, ctx2) = visitExp(exp2)
        val ctx3 = combineSeq(ctx1, ctx2)
        if ((e1 eq exp1) && (e2 eq exp2)) {
          (exp0, ctx3) // Reuse exp0.
        } else {
          (Expr.Stm(e1, e2, tpe, eff, loc), ctx3)
        }

      case Expr.Discard(exp, eff, loc) =>
        val (e, ctx) = visitExp(exp)
        if (e eq exp) {
          (exp0, ctx) // Reuse exp0.
        } else {
          (Expr.Discard(e, eff, loc), ctx)
        }

      case Expr.Match(exp, rules, tpe, eff, loc) =>
        val (e, ctx1) = visitExp(exp)
        val (rs, ctxs) = rules.map(visitMatchRule).unzip
        val ctx2 = ctxs.foldLeft(ExprContext.Empty)(combineBranch)
        val ctx3 = combineSeq(ctx1, ctx2)
        if ((e eq exp) && rules.zip(rs).forall { case (r1, r2) => r1 eq r2 }) {
          (exp0, ctx3) // Reuse exp0.
        } else {
          (Expr.Match(e, rs, tpe, eff, loc), ctx3)
        }

      case Expr.VectorLit(exps, tpe, eff, loc) =>
        val (es, ctxs) = exps.map(visitExp).unzip
        val ctx = ctxs.foldLeft(ExprContext.Empty)(combineSeq)
        if (exps.zip(es).forall { case (e1, e2) => e1 eq e2 }) {
          (exp0, ctx) // Reuse exp0.
        } else {
          (Expr.VectorLit(es, tpe, eff, loc), ctx)
        }

      case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
        val (e1, ctx1) = visitExp(exp1)
        val (e2, ctx2) = visitExp(exp2)
        val ctx3 = combineSeq(ctx1, ctx2)
        if ((e1 eq exp1) && (e2 eq exp2)) {
          (exp0, ctx3) // Reuse exp0.
        } else {
          (Expr.VectorLoad(e1, e2, tpe, eff, loc), ctx3)
        }

      case Expr.VectorLength(exp, loc) =>
        val (e, ctx) = visitExp(exp)
        if (e eq exp) {
          (exp0, ctx) // Reuse exp0.
        } else {
          (Expr.VectorLength(e, loc), ctx)
        }

      case Expr.Cast(exp, tpe, eff, loc) =>
        val (e, ctx) = visitExp(exp)
        if (e eq exp) {
          (exp0, ctx) // Reuse exp0.
        } else {
          (Expr.Cast(e, tpe, eff, loc), ctx)
        }

      case Expr.TryCatch(exp, rules, tpe, eff, loc) =>
        val (e, ctx1) = visitExp(exp)
        val (rs, ctxs) = rules.map(visitCatchRule).unzip
        val ctx2 = ctxs.foldLeft(ExprContext.Empty)(combineBranch)
        val ctx3 = combineSeq(ctx1, ctx2)
        if ((e eq exp) && rules.zip(rs).forall { case (r1, r2) => r1 eq r2 }) {
          (exp0, ctx3) // Reuse exp0.
        } else {
          (Expr.TryCatch(e, rs, tpe, eff, loc), ctx3)
        }

      case Expr.RunWith(exp, effUse, rules, tpe, eff, loc) =>
        val (e, ctx1) = visitExp(exp)
        val (rs, ctxs) = rules.map(visitHandlerRule).unzip
        val ctx2 = ctxs.foldLeft(ExprContext.Empty)(combineBranch)
        val ctx3 = combineSeq(ctx1, ctx2)
        if ((e eq exp) && rules.zip(rs).forall { case (r1, r2) => r1 eq r2 }) {
          (exp0, ctx3) // Reuse exp0.
        } else {
          (Expr.RunWith(e, effUse, rs, tpe, eff, loc), ctx3)
        }

      case Expr.Do(op, exps, tpe, eff, loc) =>
        val (es, ctxs) = exps.map(visitExp).unzip
        val ctx = ctxs.foldLeft(ExprContext.Empty)(combineSeq)
        if (exps.zip(es).forall { case (e1, e2) => e1 eq e2 }) {
          (exp0, ctx) // Reuse exp0.
        } else {
          (Expr.Do(op, es, tpe, eff, loc), ctx)
        }

      case Expr.NewObject(name, clazz, tpe, eff, methods, loc) =>
        val (ms, ctxs) = methods.map(visitJvmMethod).unzip
        val ctx = ctxs.foldLeft(ExprContext.Empty)(combineBranch)
        if (methods.zip(ms).forall { case (m1, m2) => m1 eq m2 }) {
          (exp0, ctx) // Reuse exp0.
        } else {
          (Expr.NewObject(name, clazz, tpe, eff, ms, loc), ctx)
        }
    }
  }

  private def visitMatchRule(rule: MonoAst.MatchRule)(implicit sym0: Symbol.DefnSym, lctx: LocalContext): (MonoAst.MatchRule, ExprContext) = rule match {
    case MonoAst.MatchRule(pat, guard, exp) =>
      val (g, ctx1) = guard.map(visitExp).unzip
      val (e, ctx2) = visitExp(exp)
      val ctx3 = combineSeqOpt(ctx1, ctx2)
      val (p, syms) = visitPattern(pat, ctx3)
      val ctx4 = ctx3.removeVars(syms)
      if ((p eq pat) && (g eq guard) && (e eq exp)) {
        (rule, ctx4) // Reuse rule.
      } else {
        (MonoAst.MatchRule(p, g, e), ctx4)
      }
  }

  private def visitCatchRule(rule: MonoAst.CatchRule)(implicit sym0: Symbol.DefnSym, lctx: LocalContext): (MonoAst.CatchRule, ExprContext) = rule match {
    case MonoAst.CatchRule(sym, clazz, exp) =>
      val (e, ctx1) = visitExp(exp)
      val ctx2 = ctx1.removeVar(sym)
      if (e eq exp) {
        (rule, ctx2) // Reuse rule.
      } else {
        (MonoAst.CatchRule(sym, clazz, e), ctx2)
      }
  }

  private def visitHandlerRule(rule: MonoAst.HandlerRule)(implicit sym0: Symbol.DefnSym, lctx: LocalContext): (MonoAst.HandlerRule, ExprContext) = rule match {
    case MonoAst.HandlerRule(op, fparams, exp) =>
      val (e, ctx1) = visitExp(exp)
      val fps = fparams.map(visitFormalParam(_, ctx1))
      val ctx2 = ctx1.removeVars(fps.map(_.sym))
      if ((e eq exp) && fparams.zip(fps).forall { case (fp1, fp2) => fp1 eq fp2 }) {
        (rule, ctx2) // Reuse rule.
      } else {
        (MonoAst.HandlerRule(op, fps, e), ctx2)
      }
  }

  private def visitJvmMethod(method: MonoAst.JvmMethod)(implicit sym0: Symbol.DefnSym, lctx: LocalContext): (MonoAst.JvmMethod, ExprContext) = method match {
    case MonoAst.JvmMethod(ident, fparams, exp, retTpe, eff, loc) =>
      val (e, ctx1) = visitExp(exp)
      val fps = fparams.map(visitFormalParam(_, ctx1))
      val ctx2 = ctx1.removeVars(fps.map(_.sym))
      if ((e eq exp) && fparams.zip(fps).forall { case (fp1, fp2) => fp1 eq fp2 }) {
        (method, ctx2) // Reuse method.
      } else {
        (MonoAst.JvmMethod(ident, fparams, e, retTpe, eff, loc), ctx2)
      }
  }

  private def visitPattern(pat0: MonoAst.Pattern, ctx: ExprContext): (MonoAst.Pattern, Set[VarSym]) = pat0 match {
    case MonoAst.Pattern.Wild(_, _) =>
      (pat0, Set.empty) // Always reuse pat0.

    case MonoAst.Pattern.Var(sym, tpe, occur0, loc) =>
      val occur = ctx.get(sym)
      if (occur eq occur0) {
        (pat0, Set(sym)) // Reuse pat0.
      } else {
        (MonoAst.Pattern.Var(sym, tpe, occur, loc), Set(sym))
      }

    case MonoAst.Pattern.Cst(_, _, _) =>
      (pat0, Set.empty) // Always reuse pat0.

    case MonoAst.Pattern.Tag(sym, pats, tpe, loc) =>
      val (ps, listOfSyms) = pats.map(visitPattern(_, ctx)).unzip
      val syms = listOfSyms.flatten.toSet
      if (pats.zip(ps).forall { case (p1, p2) => p1 eq p2 }) {
        (pat0, syms) // Reuse pat0.
      } else {
        (MonoAst.Pattern.Tag(sym, ps, tpe, loc), syms)
      }

    case MonoAst.Pattern.Tuple(pats, tpe, loc) =>
      val (ps, listOfSyms) = pats.map(visitPattern(_, ctx)).unzip
      val syms = listOfSyms.flatten.toSet
      if (pats.zip(ps).forall { case (p1, p2) => p1 eq p2 }) {
        (pat0, syms) // Reuse pat0.
      } else {
        (MonoAst.Pattern.Tuple(ps, tpe, loc), syms)
      }

    case MonoAst.Pattern.Record(pats, pat, tpe, loc) =>
      val (ps, listOfSyms) = pats.map(visitRecordLabelPattern(_, ctx)).unzip
      val (p, syms0) = visitPattern(pat, ctx)
      val syms = listOfSyms.flatten.toSet ++ syms0
      if ((p eq pat) && pats.zip(ps).forall { case (p1, p2) => p1 eq p2 }) {
        (pat0, syms) // Reuse pat0.
      } else {
        (MonoAst.Pattern.Record(ps, p, tpe, loc), syms)
      }
  }

  private def visitRecordLabelPattern(pat0: MonoAst.Pattern.Record.RecordLabelPattern, ctx: ExprContext): (MonoAst.Pattern.Record.RecordLabelPattern, Set[VarSym]) = pat0 match {
    case MonoAst.Pattern.Record.RecordLabelPattern(label, pat, tpe, loc) =>
      val (p, syms) = visitPattern(pat, ctx)
      if (p eq pat) {
        (pat0, syms) // Reuse pat0.
      } else {
        (MonoAst.Pattern.Record.RecordLabelPattern(label, p, tpe, loc), syms)
      }
  }

  private def visitFormalParam(fparam0: MonoAst.FormalParam, ctx: ExprContext): MonoAst.FormalParam = {
    val occur = ctx.get(fparam0.sym)
    if (occur eq fparam0.occur) {
      fparam0 // Reuse fparam0.
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
    * @param vars      A map from variable symbols to occurrence information (this also includes uses of [[Expr.LocalDef]]).
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
    * Returns true if `defn` occurs in `ctx`.
    */
  private def isSelfRef(occur: Occur): Boolean = occur match {
    case Occur.Unknown => throw InternalCompilerException("unexpected unknown occurrence information", SourceLocation.Unknown)
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
    * @param localDefs The number of declared [[Expr.LocalDef]]s in the expression.
    *                  Must be mutable.
    */
  private case class LocalContext(localDefs: AtomicInteger)

}
