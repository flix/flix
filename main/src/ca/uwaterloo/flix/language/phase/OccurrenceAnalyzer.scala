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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.OccurrenceAst.Occur.*
import ca.uwaterloo.flix.language.ast.OccurrenceAst.{DefContext, Occur}
import ca.uwaterloo.flix.language.ast.Symbol.{DefnSym, VarSym}
import ca.uwaterloo.flix.language.ast.{OccurrenceAst, Symbol}
import ca.uwaterloo.flix.util.ParOps

import java.util.concurrent.atomic.AtomicInteger

/**
  * The occurrence analyzer collects occurrence information on binders according to the definition of [[Occur]].
  * Additionally, it also counts the number of subexpressions in a function to compute its size.
  */
object OccurrenceAnalyzer {

  /**
    * Performs occurrence analysis on the given AST `root`.
    */
  def run(root: OccurrenceAst.Root)(implicit flix: Flix): OccurrenceAst.Root = {
    val defs = ParOps.parMapValues(root.defs)(visitDef)
    OccurrenceAst.Root(defs, root.enums, root.structs, root.effects, root.mainEntryPoint, root.entryPoints, root.sources)
  }

  /**
    * Performs occurrence analysis on `defn`.
    */
  private def visitDef(defn: OccurrenceAst.Def): OccurrenceAst.Def = {
    implicit val lctx: LocalContext = LocalContext.mk()
    val (exp, ctx) = visitExp(defn.exp)(defn.sym, lctx)
    val defContext = DefContext(lctx.size.get(), lctx.localDefs.get(), isDirectCall(exp), isSelfRecursive(ctx.selfOccur))
    val fparams = defn.fparams.map(fp => fp.copy(occur = ctx.get(fp.sym)))
    OccurrenceAst.Def(defn.sym, fparams, defn.spec, exp, defContext, defn.loc)
  }

  /**
    * Performs occurrence analysis on `exp0`
    */
  private def visitExp(exp0: OccurrenceAst.Expr)(implicit sym0: Symbol.DefnSym, lctx: LocalContext): (OccurrenceAst.Expr, ExprContext) = {
    lctx.size.incrementAndGet()
    exp0 match {
      case OccurrenceAst.Expr.Cst(_, _, _) =>
        (exp0, ExprContext.empty)

      case OccurrenceAst.Expr.Var(sym, _, _) =>
        (exp0, ExprContext.empty.addVar(sym, Once))

      case OccurrenceAst.Expr.Lambda(fparam, exp, tpe, loc) =>
        val (e, ctx1) = visitExp(exp)
        val ctx2 = ctx1.map {
          case Once => OnceInLambda
        }
        val occur = ctx2.get(fparam.sym)
        val fp = fparam.copy(occur = occur)
        val ctx3 = ctx2.removeVar(fparam.sym)
        (OccurrenceAst.Expr.Lambda(fp, e, tpe, loc), ctx3)

      case OccurrenceAst.Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
        val (es, ctxs) = exps.map(visitExp).unzip
        val ctx = ctxs.foldLeft(ExprContext.empty)(combineSeq)
        (OccurrenceAst.Expr.ApplyAtomic(op, es, tpe, eff, loc), ctx)

      case OccurrenceAst.Expr.ApplyClo(exp1, exp2, tpe, eff, loc) =>
        val (e1, ctx1) = visitExp(exp1)
        val (e2, ctx2) = visitExp(exp2)
        val ctx3 = combineSeq(ctx1, ctx2)
        if (e1.eq(exp1) && e2.eq(exp2)) { // Reuse memory if there is no change
          (exp0, ctx3)
        } else {
          (OccurrenceAst.Expr.ApplyClo(e1, e2, tpe, eff, loc), ctx3)
        }

      case OccurrenceAst.Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
        val (es, ctxs) = exps.map(visitExp).unzip
        val ctx1 = if (sym == sym0) ExprContext.recursiveOnce else ExprContext.empty
        val ctx2 = ctxs.foldLeft(ctx1)(combineSeq)
        (OccurrenceAst.Expr.ApplyDef(sym, es, itpe, tpe, eff, loc), ctx2)

      case OccurrenceAst.Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
        val (es, ctxs) = exps.map(visitExp).unzip
        val ctx = ctxs.foldLeft(ExprContext.empty)(combineSeq)
        (OccurrenceAst.Expr.ApplyLocalDef(sym, es, tpe, eff, loc), ctx)

      case OccurrenceAst.Expr.Let(sym, exp1, exp2, tpe, eff, _, loc) =>
        val (e1, ctx1) = visitExp(exp1)
        val (e2, ctx2) = visitExp(exp2)
        val ctx3 = combineSeq(ctx1, ctx2)
        val occur = ctx3.get(sym)
        val ctx4 = ctx3.removeVar(sym)
        (OccurrenceAst.Expr.Let(sym, e1, e2, tpe, eff, occur, loc), ctx4)

      case OccurrenceAst.Expr.LocalDef(sym, formalParams, exp1, exp2, tpe, eff, _, loc) =>
        val (e1, ctx1) = visitExp(exp1)
        val ctx2 = ctx1.map {
          case Once => OnceInLocalDef
        }
        val fps = formalParams.map(fp => fp.copy(occur = ctx2.get(fp.sym)))
        val ctx3 = ctx2.removeVars(fps.map(_.sym))
        val (e2, ctx4) = visitExp(exp2)
        val ctx5 = combineSeq(ctx3, ctx4)
        val occur = ctx5.get(sym)
        val ctx6 = ctx5.removeVar(sym)
        lctx.localDefs.incrementAndGet()
        (OccurrenceAst.Expr.LocalDef(sym, fps, e1, e2, tpe, eff, occur, loc), ctx6)

      case OccurrenceAst.Expr.Scope(sym, rsym, exp, tpe, eff, loc) =>
        val (e, ctx) = visitExp(exp)
        (OccurrenceAst.Expr.Scope(sym, rsym, e, tpe, eff, loc), ctx)

      case OccurrenceAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
        val (e1, ctx1) = visitExp(exp1)
        val (e2, ctx2) = visitExp(exp2)
        val (e3, ctx3) = visitExp(exp3)
        val ctx4 = combineSeq(ctx1, combineBranch(ctx2, ctx3))
        (OccurrenceAst.Expr.IfThenElse(e1, e2, e3, tpe, eff, loc), ctx4)

      case OccurrenceAst.Expr.Stm(exp1, exp2, tpe, eff, loc) =>
        val (e1, ctx1) = visitExp(exp1)
        val (e2, ctx2) = visitExp(exp2)
        val ctx3 = combineSeq(ctx1, ctx2)
        (OccurrenceAst.Expr.Stm(e1, e2, tpe, eff, loc), ctx3)

      case OccurrenceAst.Expr.Discard(exp, eff, loc) =>
        val (e, ctx) = visitExp(exp)
        (OccurrenceAst.Expr.Discard(e, eff, loc), ctx)

      case OccurrenceAst.Expr.Match(exp, rules, tpe, eff, loc) =>
        val (e, ctx1) = visitExp(exp)
        val (rs, ctxs) = rules.map(visitMatchRule).unzip
        val ctx2 = ctxs.foldLeft(ExprContext.empty)(combineBranch)
        val ctx3 = combineSeq(ctx1, ctx2)
        (OccurrenceAst.Expr.Match(e, rs, tpe, eff, loc), ctx3)

      case OccurrenceAst.Expr.VectorLit(exps, tpe, eff, loc) =>
        val (es, ctxs) = exps.map(visitExp).unzip
        val ctx = ctxs.foldLeft(ExprContext.empty)(combineSeq)
        (OccurrenceAst.Expr.VectorLit(es, tpe, eff, loc), ctx)

      case OccurrenceAst.Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
        val (e1, ctx1) = visitExp(exp1)
        val (e2, ctx2) = visitExp(exp2)
        val ctx3 = combineSeq(ctx1, ctx2)
        (OccurrenceAst.Expr.VectorLoad(e1, e2, tpe, eff, loc), ctx3)

      case OccurrenceAst.Expr.VectorLength(exp, loc) =>
        val (e, ctx) = visitExp(exp)
        (OccurrenceAst.Expr.VectorLength(e, loc), ctx)

      case OccurrenceAst.Expr.Ascribe(exp, tpe, eff, loc) =>
        val (e, ctx) = visitExp(exp)
        (OccurrenceAst.Expr.Ascribe(e, tpe, eff, loc), ctx)

      case OccurrenceAst.Expr.Cast(exp, declaredType, declaredEff, tpe, eff, loc) =>
        val (e, ctx) = visitExp(exp)
        (OccurrenceAst.Expr.Cast(e, declaredType, declaredEff, tpe, eff, loc), ctx)

      case OccurrenceAst.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
        val (e, ctx1) = visitExp(exp)
        val (rs, ctxs) = rules.map(visitCatchRule).unzip
        val ctx2 = ctxs.foldLeft(ExprContext.empty)(combineBranch)
        val ctx3 = combineSeq(ctx1, ctx2)
        (OccurrenceAst.Expr.TryCatch(e, rs, tpe, eff, loc), ctx3)

      case OccurrenceAst.Expr.RunWith(exp, effUse, rules, tpe, eff, loc) =>
        val (e, ctx1) = visitExp(exp)
        val (rs, ctxs) = rules.map(visitHandlerRule).unzip
        val ctx2 = ctxs.foldLeft(ExprContext.empty)(combineBranch)
        val ctx3 = combineSeq(ctx1, ctx2)
        (OccurrenceAst.Expr.RunWith(e, effUse, rs, tpe, eff, loc), ctx3)

      case OccurrenceAst.Expr.Do(op, exps, tpe, eff, loc) =>
        val (es, ctxs) = exps.map(visitExp).unzip
        val ctx = ctxs.foldLeft(ExprContext.empty)(combineSeq)
        (OccurrenceAst.Expr.Do(op, es, tpe, eff, loc), ctx)

      case OccurrenceAst.Expr.NewObject(name, clazz, tpe, eff, methods, loc) =>
        val (ms, ctxs) = methods.map(visitJvmMethod).unzip
        val ctx = ctxs.foldLeft(ExprContext.empty)(combineBranch)
        (OccurrenceAst.Expr.NewObject(name, clazz, tpe, eff, ms, loc), ctx)
    }
  }

  private def visitMatchRule(rule: OccurrenceAst.MatchRule)(implicit sym0: Symbol.DefnSym, lctx: LocalContext): (OccurrenceAst.MatchRule, ExprContext) = rule match {
    case OccurrenceAst.MatchRule(pat, guard, exp) =>
      val (g, ctx1) = guard.map(visitExp).unzip
      val (e, ctx2) = visitExp(exp)
      val ctx3 = combineSeqOpt(ctx1, ctx2)
      val (p, syms) = visitPattern(pat)(ctx3)
      val ctx4 = ctx3.removeVars(syms)
      (OccurrenceAst.MatchRule(p, g, e), ctx4)
  }

  private def visitCatchRule(rule: OccurrenceAst.CatchRule)(implicit sym0: Symbol.DefnSym, lctx: LocalContext): (OccurrenceAst.CatchRule, ExprContext) = rule match {
    case OccurrenceAst.CatchRule(sym, clazz, exp) =>
      val (e, ctx1) = visitExp(exp)
      val ctx2 = ctx1.removeVar(sym)
      (OccurrenceAst.CatchRule(sym, clazz, e), ctx2)
  }

  private def visitHandlerRule(rule: OccurrenceAst.HandlerRule)(implicit sym0: Symbol.DefnSym, lctx: LocalContext): (OccurrenceAst.HandlerRule, ExprContext) = rule match {
    case OccurrenceAst.HandlerRule(op, fparams, exp) =>
      val (e, ctx1) = visitExp(exp)
      val fps = fparams.map(fp => fp.copy(occur = ctx1.get(fp.sym)))
      val ctx2 = ctx1.removeVars(fps.map(_.sym))
      (OccurrenceAst.HandlerRule(op, fps, e), ctx2)
  }

  private def visitJvmMethod(method: OccurrenceAst.JvmMethod)(implicit sym0: Symbol.DefnSym, lctx: LocalContext): (OccurrenceAst.JvmMethod, ExprContext) = method match {
    case OccurrenceAst.JvmMethod(ident, fparams, exp, retTpe, eff, loc) =>
      val (c, ctx) = visitExp(exp)
      (OccurrenceAst.JvmMethod(ident, fparams, c, retTpe, eff, loc), ctx)
  }

  private def visitPattern(pattern0: OccurrenceAst.Pattern)(implicit ctx: ExprContext): (OccurrenceAst.Pattern, Set[VarSym]) = pattern0 match {
    case OccurrenceAst.Pattern.Wild(_, _) =>
      (pattern0, Set.empty)

    case OccurrenceAst.Pattern.Var(sym, tpe, _, loc) =>
      (OccurrenceAst.Pattern.Var(sym, tpe, ctx.get(sym), loc), Set(sym))

    case OccurrenceAst.Pattern.Cst(_, _, _) =>
      (pattern0, Set.empty)

    case OccurrenceAst.Pattern.Tag(sym, pats, tpe, loc) =>
      val (ps, listOfSyms) = pats.map(visitPattern).unzip
      val syms = listOfSyms.flatten.toSet
      (OccurrenceAst.Pattern.Tag(sym, ps, tpe, loc), syms)

    case OccurrenceAst.Pattern.Tuple(pats, tpe, loc) =>
      val (ps, listOfSyms) = pats.map(visitPattern).unzip
      val syms = listOfSyms.flatten.toSet
      (OccurrenceAst.Pattern.Tuple(ps, tpe, loc), syms)

    case OccurrenceAst.Pattern.Record(pats, pat, tpe, loc) =>
      val (ps, listOfSyms) = pats.map(visitRecordLabelPattern).unzip
      val (p, syms0) = visitPattern(pat)
      val syms = listOfSyms.flatten.toSet ++ syms0
      (OccurrenceAst.Pattern.Record(ps, p, tpe, loc), syms)

  }

  private def visitRecordLabelPattern(pattern0: OccurrenceAst.Pattern.Record.RecordLabelPattern)(implicit ctx: ExprContext): (OccurrenceAst.Pattern.Record.RecordLabelPattern, Set[VarSym]) = pattern0 match {
    case OccurrenceAst.Pattern.Record.RecordLabelPattern(label, pat, tpe, loc) =>
      val (p, syms) = visitPattern(pat)
      (OccurrenceAst.Pattern.Record.RecordLabelPattern(label, p, tpe, loc), syms)
  }

  /**
    * Combines `ctx1` and `ctx2` into a single [[ExprContext]].
    */
  private def combineBranch(ctx1: ExprContext, ctx2: ExprContext): ExprContext = {
    combine(ctx1, ctx2, combineBranch)
  }

  /**
    * Combines `ctx1` and `ctx2` into a single [[ExprContext]].
    */
  private def combineSeq(ctx1: ExprContext, ctx2: ExprContext): ExprContext = {
    combine(ctx1, ctx2, combineSeq)
  }

  /**
    * Combines `ctx1` and `ctx2` into a single [[ExprContext]].
    */
  private def combineSeqOpt(ctx1: Option[ExprContext], ctx2: ExprContext): ExprContext = {
    ctx1.map(combineSeq(_, ctx2)).getOrElse(ctx2)
  }

  /**
    * Combines `ctx1` and `ctx2` into a single [[ExprContext]].
    */
  private def combine(ctx1: ExprContext, ctx2: ExprContext, combine: (Occur, Occur) => Occur): ExprContext = {
    val selfOccur = combine(ctx1.selfOccur, ctx2.selfOccur)
    val varMap = combineMaps(ctx1.vars, ctx2.vars, combine)
    ExprContext(selfOccur, varMap)
  }

  /**
    * Combines maps `m1` and `m2` into a single map.
    */
  private def combineMaps[A](m1: Map[A, Occur], m2: Map[A, Occur], combine: (Occur, Occur) => Occur): Map[A, Occur] = {
    val (smallest, largest) = if (m1.size < m2.size) (m1, m2) else (m2, m1)
    smallest.foldLeft[Map[A, Occur]](largest) {
      case (acc, (k, v)) =>
        val occur = combine(v, acc.getOrElse(k, Dead))
        acc + (k -> occur)
    }
  }

  /**
    * Maps each [[DefnSym]] to its corresponding [[Occur]] combining with [[combineSeq]].
    */
  private def combineSeq(kvs: Iterable[(DefnSym, Occur)]): Map[DefnSym, Occur] = {
    kvs.foldLeft(Map.empty[DefnSym, Occur]) {
      case (acc, (sym, occur1)) => acc.get(sym) match {
        case Some(occur2) => acc + (sym -> combineSeq(occur1, occur2))
        case None => acc + (sym -> occur1)
      }
    }
  }

  /**
    * Combines two occurrences `o1` and `o2` from the same branch into a single occurrence.
    */
  private def combineSeq(o1: Occur, o2: Occur): Occur = (o1, o2) match {
    case (Dead, _) => o2
    case (_, Dead) => o1
    case _ => Many
  }

  /**
    * Combines two occurrences `o1` and `o2` from distinct branches into a single occurrence.
    */
  private def combineBranch(o1: Occur, o2: Occur): Occur = (o1, o2) match {
    case (Dead, _) => o2
    case (_, Dead) => o1
    case (Once, Once) => ManyBranch
    case _ => Many
  }

  private object ExprContext {

    /** Context for an empty sequence of expressions. */
    def empty: ExprContext = ExprContext(Dead, Map.empty)

    /** Context for a self-recursive call. */
    def recursiveOnce: ExprContext = ExprContext(Once, Map.empty)

  }

  /**
    * Stores various pieces of information extracted from an expression.
    *
    * @param selfOccur Occurrence information on how the function occurs in its own definition.
    * @param vars      A map from variable symbols to occurrence information (this also includes uses of [[OccurrenceAst.Expr.LocalDef]]).
    *                  If the map does not contain a certain symbol, then the symbol is [[Dead]].
    */
  case class ExprContext(selfOccur: Occur, vars: Map[VarSym, Occur]) {

    /**
      * Returns the occurrence information collected on `sym`.
      * If [[vars]] does not contain `sym`, then it is [[Dead]].
      */
    def get(sym: VarSym): Occur = {
      this.vars.getOrElse(sym, Dead)
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

    /**
      * Applies `f` to each value in `vars` where `f` is defined.
      * Returns a new [[ExprContext]] with the updated map.
      */
    def map(f: PartialFunction[Occur, Occur]): ExprContext = {
      val newVars = vars.map {
        case (k, v) =>
          if (f.isDefinedAt(v))
            (k, f(v))
          else
            (k, v)
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
    def mk(): LocalContext = new LocalContext(new AtomicInteger(0), new AtomicInteger(0))

  }

  /**
    * A local context, scoped for each function definition.
    * No requirements on thread-safety since it is scoped.
    *
    * @param localDefs The number of declared [[OccurrenceAst.Expr.LocalDef]]s in the expression.
    *                  Must be mutable.
    * @param size      The total number of subexpressions (including the expression itself).
    *                  Must be mutable.
    */
  private case class LocalContext(localDefs: AtomicInteger, size: AtomicInteger)

}
