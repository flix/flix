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
  private def visitExp(exp0: OccurrenceAst.Expr)(implicit sym0: Symbol.DefnSym, lctx: LocalContext): (OccurrenceAst.Expr, ExpContext) = exp0 match {
    case OccurrenceAst.Expr.Cst(cst, tpe, loc) =>
      lctx.size.incrementAndGet()
      (OccurrenceAst.Expr.Cst(cst, tpe, loc), ExpContext.empty)

    case OccurrenceAst.Expr.Var(sym, tpe, loc) =>
      (OccurrenceAst.Expr.Var(sym, tpe, loc), ExpContext.empty.addVar(sym, Once))

    case OccurrenceAst.Expr.Lambda(fp, exp, tpe, loc) =>
      val (e, ctx1) = visitExp(exp)
      val ctx2 = captureVarsInLambda(ctx1)
      val occur = ctx2.get(fp.sym)
      val ctx3 = ctx2.removeVar(fp.sym)
      lctx.size.incrementAndGet()
      (OccurrenceAst.Expr.Lambda(fp.copy(occur = occur), e, tpe, loc), ctx3)

    case OccurrenceAst.Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
      val (es, ctxs) = exps.map(visitExp).unzip
      val ctx = ctxs.foldLeft(ExpContext.empty)(combineSeq)
      lctx.size.incrementAndGet()
      (OccurrenceAst.Expr.ApplyAtomic(op, es, tpe, eff, loc), ctx)

    case OccurrenceAst.Expr.ApplyClo(exp1, exp2, tpe, eff, loc) =>
      val (e1, ctx1) = visitExp(exp1)
      val (e2, ctx2) = visitExp(exp2)
      val ctx3 = combineSeq(ctx1, ctx2)
      lctx.size.incrementAndGet()
      (OccurrenceAst.Expr.ApplyClo(e1, e2, tpe, eff, loc), ctx3)

    case OccurrenceAst.Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
      val (es, ctxs) = exps.map(visitExp).unzip
      val ctx = ctxs.foldLeft(ExpContext.empty)(combineSeq)
      lctx.size.incrementAndGet()
      (OccurrenceAst.Expr.ApplyDef(sym, es, itpe, tpe, eff, loc), ctx)

    case OccurrenceAst.Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
      val (es, ctxs) = exps.map(visitExp).unzip
      val ctx = ctxs.foldLeft(ExpContext.empty)(combineSeq)
      lctx.size.incrementAndGet()
      (OccurrenceAst.Expr.ApplyLocalDef(sym, es, tpe, eff, loc), ctx)

    case OccurrenceAst.Expr.Let(sym, exp1, exp2, tpe, eff, _, loc) =>
      val (e1, ctx1) = visitExp(exp1)
      val (e2, ctx2) = visitExp(exp2)
      val ctx3 = combineSeq(ctx1, ctx2)
      val occur = ctx3.get(sym)
      val ctx4 = ctx3.removeVar(sym)
      lctx.size.incrementAndGet()
      (OccurrenceAst.Expr.Let(sym, e1, e2, tpe, eff, occur, loc), ctx4)

    case OccurrenceAst.Expr.LocalDef(sym, formalParams, exp1, exp2, tpe, eff, _, loc) =>
      val (e1, ctx1) = visitExp(exp1)
      val ctx2 = captureVarsInLocalDef(ctx1)
      val fps = formalParams.map(fp => fp.copy(occur = ctx2.get(fp.sym)))
      val ctx3 = ctx2.removeVars(fps.map(_.sym))
      val (e2, ctx4) = visitExp(exp2)
      val ctx5 = combineSeq(ctx3, ctx4)
      val occur = ctx5.get(sym)
      val ctx6 = ctx5.removeVar(sym)
      lctx.localDefs.incrementAndGet()
      lctx.size.incrementAndGet()
      (OccurrenceAst.Expr.LocalDef(sym, fps, e1, e2, tpe, eff, occur, loc), ctx6)

    case OccurrenceAst.Expr.Scope(sym, rsym, exp, tpe, eff, loc) =>
      val (e, ctx) = visitExp(exp)
      lctx.size.incrementAndGet()
      (OccurrenceAst.Expr.Scope(sym, rsym, e, tpe, eff, loc), ctx)

    case OccurrenceAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val (e1, ctx1) = visitExp(exp1)
      val (e2, ctx2) = visitExp(exp2)
      val (e3, ctx3) = visitExp(exp3)
      val ctx4 = combineSeq(ctx1, combineBranch(ctx2, ctx3))
      lctx.size.incrementAndGet()
      (OccurrenceAst.Expr.IfThenElse(e1, e2, e3, tpe, eff, loc), ctx4)

    case OccurrenceAst.Expr.Stm(exp1, exp2, tpe, eff, loc) =>
      val (e1, ctx1) = visitExp(exp1)
      val (e2, ctx2) = visitExp(exp2)
      val ctx3 = combineSeq(ctx1, ctx2)
      lctx.size.incrementAndGet()
      (OccurrenceAst.Expr.Stm(e1, e2, tpe, eff, loc), ctx3)

    case OccurrenceAst.Expr.Discard(exp, eff, loc) =>
      val (e, ctx) = visitExp(exp)
      lctx.size.incrementAndGet()
      (OccurrenceAst.Expr.Discard(e, eff, loc), ctx)

    case OccurrenceAst.Expr.Match(exp, rules, tpe, eff, loc) =>
      val (e, ctx1) = visitExp(exp)
      val (rs, ctx2) = visitMatchRules(rules)
      val ctx3 = combineSeq(ctx1, ctx2)
      lctx.size.incrementAndGet()
      (OccurrenceAst.Expr.Match(e, rs, tpe, eff, loc), ctx3)

    case OccurrenceAst.Expr.VectorLit(exps, tpe, eff, loc) =>
      val (es, ctxs) = exps.map(visitExp).unzip
      val ctx = ctxs.foldLeft(ExpContext.empty)(combineSeq)
      lctx.size.incrementAndGet()
      (OccurrenceAst.Expr.VectorLit(es, tpe, eff, loc), ctx)

    case OccurrenceAst.Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      val (e1, ctx1) = visitExp(exp1)
      val (e2, ctx2) = visitExp(exp2)
      val ctx3 = combineSeq(ctx1, ctx2)
      lctx.size.incrementAndGet()
      (OccurrenceAst.Expr.VectorLoad(e1, e2, tpe, eff, loc), ctx3)

    case OccurrenceAst.Expr.VectorLength(exp, loc) =>
      val (e, ctx) = visitExp(exp)
      lctx.size.incrementAndGet()
      (OccurrenceAst.Expr.VectorLength(e, loc), ctx)

    case OccurrenceAst.Expr.Ascribe(exp, tpe, eff, loc) =>
      val (e, ctx) = visitExp(exp)
      lctx.size.incrementAndGet()
      (OccurrenceAst.Expr.Ascribe(e, tpe, eff, loc), ctx)

    case OccurrenceAst.Expr.Cast(exp, declaredType, declaredEff, tpe, eff, loc) =>
      val (e, ctx) = visitExp(exp)
      lctx.size.incrementAndGet()
      (OccurrenceAst.Expr.Cast(e, declaredType, declaredEff, tpe, eff, loc), ctx)

    case OccurrenceAst.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val (e, ctx1) = visitExp(exp)
      val (rs, ctx2) = visitTryCatchRules(rules)
      val ctx3 = combineSeq(ctx1, ctx2)
      lctx.size.incrementAndGet()
      (OccurrenceAst.Expr.TryCatch(e, rs, tpe, eff, loc), ctx3)

    case OccurrenceAst.Expr.RunWith(exp, effUse, rules, tpe, eff, loc) =>
      val (e, ctx1) = visitExp(exp)
      val (rs, ctx2) = visitTryWithRules(rules)
      val ctx3 = combineSeq(ctx1, ctx2)
      lctx.size.incrementAndGet()
      (OccurrenceAst.Expr.RunWith(e, effUse, rs, tpe, eff, loc), ctx3)

    case OccurrenceAst.Expr.Do(op, exps, tpe, eff, loc) =>
      val (es, ctxs) = exps.map(visitExp).unzip
      val ctx = ctxs.foldLeft(ExpContext.empty)(combineSeq)
      lctx.size.incrementAndGet()
      (OccurrenceAst.Expr.Do(op, es, tpe, eff, loc), ctx)

    case OccurrenceAst.Expr.NewObject(name, clazz, tpe, eff, methods, loc) =>
      val (ms, ctx) = visitJvmMethods(methods)
      lctx.size.incrementAndGet()
      (OccurrenceAst.Expr.NewObject(name, clazz, tpe, eff, ms, loc), ctx)
  }

  private def visitMatchRules(rules0: List[OccurrenceAst.MatchRule])(implicit sym0: Symbol.DefnSym, lctx: LocalContext): (List[OccurrenceAst.MatchRule], ExpContext) = {
    val (rs, ctx1) = rules0.map {
      case OccurrenceAst.MatchRule(pat, guard, exp) =>
        val (g, ctx1) = guard.map(visitExp).unzip
        val (e, ctx2) = visitExp(exp)
        val ctx3 = combineSeqOpt(ctx1, ctx2)
        val (p, syms) = visitPattern(pat)(ctx3)
        val ctx4 = ctx3.removeVars(syms)
        (OccurrenceAst.MatchRule(p, g, e), ctx4)
    }.unzip
    val ctx2 = ctx1.foldLeft(ExpContext.empty)(combineBranch)
    (rs, ctx2)
  }

  private def visitTryCatchRules(rules0: List[OccurrenceAst.CatchRule])(implicit sym0: Symbol.DefnSym, lctx: LocalContext): (List[OccurrenceAst.CatchRule], ExpContext) = {
    val (rs, ctx1) = rules0.map {
      case OccurrenceAst.CatchRule(sym, clazz, exp) =>
        val (e, ctx1) = visitExp(exp)
        val ctx2 = ctx1.removeVar(sym)
        (OccurrenceAst.CatchRule(sym, clazz, e), ctx2)
    }.unzip
    val ctx2 = ctx1.foldLeft(ExpContext.empty)(combineBranch)
    (rs, ctx2)
  }

  private def visitTryWithRules(rules0: List[OccurrenceAst.HandlerRule])(implicit sym0: Symbol.DefnSym, lctx: LocalContext): (List[OccurrenceAst.HandlerRule], ExpContext) = {
    val (rs, ctx1) = rules0.map {
      case OccurrenceAst.HandlerRule(op, fparams, exp) =>
        val (e, ctx1) = visitExp(exp)
        val fps = fparams.map(fp => fp.copy(occur = ctx1.get(fp.sym)))
        val ctx2 = ctx1.removeVars(fps.map(_.sym))
        (OccurrenceAst.HandlerRule(op, fps, e), ctx2)
    }.unzip
    val ctx2 = ctx1.foldLeft(ExpContext.empty)(combineBranch)
    (rs, ctx2)
  }

  private def visitJvmMethods(methods0: List[OccurrenceAst.JvmMethod])(implicit sym0: Symbol.DefnSym, lctx: LocalContext): (List[OccurrenceAst.JvmMethod], ExpContext) = {
    val (ms, ctx1) = methods0.map {
      case OccurrenceAst.JvmMethod(ident, fparams, exp, retTpe, eff, loc) =>
        val (c, ctx) = visitExp(exp)
        (OccurrenceAst.JvmMethod(ident, fparams, c, retTpe, eff, loc), ctx)
    }.unzip
    val ctx2 = ctx1.foldLeft(ExpContext.empty)(combineBranch)
    (ms, ctx2)
  }

  private def visitPattern(pattern0: OccurrenceAst.Pattern)(implicit ctx: ExpContext): (OccurrenceAst.Pattern, Set[VarSym]) = pattern0 match {
    case OccurrenceAst.Pattern.Wild(tpe, loc) =>
      (OccurrenceAst.Pattern.Wild(tpe, loc), Set.empty)

    case OccurrenceAst.Pattern.Var(sym, tpe, _, loc) =>
      (OccurrenceAst.Pattern.Var(sym, tpe, ctx.get(sym), loc), Set(sym))

    case OccurrenceAst.Pattern.Cst(cst, tpe, loc) =>
      (OccurrenceAst.Pattern.Cst(cst, tpe, loc), Set.empty)

    case OccurrenceAst.Pattern.Tag(sym, pats, tpe, loc) =>
      val (ps, listOfSyms) = pats.map(visitPattern).unzip
      val syms = Set.from(listOfSyms.flatten)
      (OccurrenceAst.Pattern.Tag(sym, ps, tpe, loc), syms)

    case OccurrenceAst.Pattern.Tuple(pats, tpe, loc) =>
      val (ps, listOfSyms) = pats.map(visitPattern).unzip
      val syms = Set.from(listOfSyms.flatten)
      (OccurrenceAst.Pattern.Tuple(ps, tpe, loc), syms)

    case OccurrenceAst.Pattern.Record(pats, pat, tpe, loc) =>
      val (ps, listOfSyms) = pats.map(visitRecordLabelPattern).unzip
      val (p, syms0) = visitPattern(pat)
      val syms = Set.from(listOfSyms.flatten) ++ syms0
      (OccurrenceAst.Pattern.Record(ps, p, tpe, loc), syms)

  }

  private def visitRecordLabelPattern(pattern0: OccurrenceAst.Pattern.Record.RecordLabelPattern)(implicit ctx: ExpContext): (OccurrenceAst.Pattern.Record.RecordLabelPattern, Set[VarSym]) = pattern0 match {
    case OccurrenceAst.Pattern.Record.RecordLabelPattern(label, pat, tpe, loc) =>
      val (p, syms) = visitPattern(pat)
      (OccurrenceAst.Pattern.Record.RecordLabelPattern(label, p, tpe, loc), syms)
  }

  /**
    * Combines `ctx1` and `ctx2` into a single [[ExpContext]].
    */
  private def combineBranch(ctx1: ExpContext, ctx2: ExpContext): ExpContext = {
    combine(ctx1, ctx2, combineBranch)
  }

  /**
    * Combines `ctx1` and `ctx2` into a single [[ExpContext]].
    */
  private def combineSeq(ctx1: ExpContext, ctx2: ExpContext): ExpContext = {
    combine(ctx1, ctx2, combineSeq)
  }

  /**
    * Combines `ctx1` and `ctx2` into a single [[ExpContext]].
    */
  private def combineSeqOpt(ctx1: Option[ExpContext], ctx2: ExpContext): ExpContext = {
    ctx1.map(combineSeq(_, ctx2)).getOrElse(ctx2)
  }

  /**
    * Combines `ctx1` and `ctx2` into a single [[ExpContext]].
    */
  private def combine(ctx1: ExpContext, ctx2: ExpContext, combine: (Occur, Occur) => Occur): ExpContext = {
    val selfOccur = combine(ctx1.selfOccur, ctx2.selfOccur)
    val varMap = combineMaps(ctx1.vars, ctx2.vars, combine)
    ExpContext(selfOccur, varMap)
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

  private object ExpContext {

    /** Context for an empty sequence of expressions. */
    def empty: ExpContext = ExpContext(Dead, Map.empty)

    /** Context for a self-recursive call. */
    def recursiveOnce: ExpContext = ExpContext(Once, Map.empty)

  }

  // TODO: Maybe refactor to member function?
  private def captureVarsInLambda(ctx: ExpContext): ExpContext = {
    update(ctx) {
      case Once => OnceInLambda
    }
  }

  // TODO: Maybe refactor to member function?
  private def captureVarsInLocalDef(expContext: ExpContext): ExpContext = {
    update(expContext) {
      case Once => OnceInLocalDef
    }
  }

  // TODO: Maybe refactor to member function?
  private def update(ctx: ExpContext)(f: PartialFunction[Occur, Occur]): ExpContext = {
    val vars = ctx.vars.map {
      case (k, v) =>
        if (f.isDefinedAt(v))
          (k, f(v))
        else
          (k, v)
    }
    ctx.copy(vars = vars)
  }

  /**
    * Stores various pieces of information extracted from an expression.
    *
    * @param selfOccur Occurrence information on how the function occurs in its own definition.
    * @param vars      A map from variable symbols to occurrence information (this also includes uses of [[OccurrenceAst.Expr.LocalDef]]).
    *                  If the map does not contain a certain symbol, then the symbol is [[Dead]].
    */
  case class ExpContext(selfOccur: Occur, vars: Map[VarSym, Occur]) {

    /**
      * Returns the occurrence information collected on `sym`.
      * If [[vars]] does not contain `sym`, then it is [[Dead]].
      */
    def get(sym: VarSym): Occur = {
      this.vars.getOrElse(sym, Dead)
    }

    /** Returns a new [[ExpContext]] with the mapping `sym -> occur` added to [[vars]]. */
    def addVar(sym: VarSym, occur: Occur): ExpContext = {
      this.copy(vars = this.vars + (sym -> occur))
    }

    /** Returns a new [[ExpContext]] with `sym` and the corresponding value removed from [[vars]]. */
    def removeVar(sym: VarSym): ExpContext = {
      this.copy(vars = this.vars - sym)
    }

    /** Returns a new [[ExpContext]] with `syms` and the corresponding values removed from [[vars]]. */
    def removeVars(syms: Iterable[VarSym]): ExpContext = {
      this.copy(vars = this.vars -- syms)
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
