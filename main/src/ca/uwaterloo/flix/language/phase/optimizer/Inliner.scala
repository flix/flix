/*
 * Copyright 2018 Magnus Madsen, Anna Krogh, Patrick Lundvig, Christian Bonde
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
import ca.uwaterloo.flix.language.ast.OccurrenceAst.{Expr, Occur, Pattern}
import ca.uwaterloo.flix.language.ast.shared.Constant
import ca.uwaterloo.flix.language.ast.{AtomicOp, OccurrenceAst, Symbol, Type}
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

/**
  * Rewrites the body of each def using, using the following transformations:
  *   - Copy Propagation:
  * {{{
  *     let x = 1;
  *     f(x)
  * }}}
  *     becomes
  * {{{
  *     let x = 1;
  *     f(1)
  * }}}
  *   - Dead Code Elimination
  * {{{
  *     let x = 1;
  *     f(1)
  * }}}
  *     becomes
  * {{{
  *     f(1)
  * }}}
  *   - Inline Expansion
  * {{{
  *     f(1)
  * }}}
  *     becomes (where the definition of `f` is `x + 2`)
  * {{{
  *     (x -> x + 2)(1)
  * }}}
  *   - Beta Reduction
  * {{{
  *     (x -> x + 2)(1)
  * }}}
  *     becomes
  * {{{
  *     let x = 1;
  *     x + 2
  * }}}
  */
object Inliner {

  /** Performs inlining on the given AST `root`. */
  def run(root: OccurrenceAst.Root)(implicit flix: Flix): OccurrenceAst.Root = {
    val defs = ParOps.parMapValues(root.defs)(visitDef(_)(root, flix))
    root.copy(defs = defs)
  }

  /** Performs inlining on the body of `def0`. */
  private def visitDef(def0: OccurrenceAst.Def)(implicit root: OccurrenceAst.Root, flix: Flix): OccurrenceAst.Def = def0 match {
    case OccurrenceAst.Def(sym, fparams, spec, exp, ctx, loc) =>
      val e = visitExp(exp, LocalContext.Empty)(sym, root, flix)
      OccurrenceAst.Def(sym, fparams, spec, e, ctx, loc)
  }

  /**
    * Performs inlining on the expression `exp0`.
    *
    * To avoid duplicating variable names, `visitExp` unconditionally
    * assigns new names to all variables.
    * When a binder is visited, it replaces it with a fresh variable and adds
    * it to the variable substitution `varSubst` in th LocalContext `ctx0`.
    * When a variable is visited, it replaces the old variable with the fresh one.
    * Top-level function parameters are not substituted unless inlined, in which case
    * the parameters are let-bound and added to the variable substitution.
    */
  private def visitExp(exp0: Expr, ctx0: LocalContext)(implicit sym0: Symbol.DefnSym, root: OccurrenceAst.Root, flix: Flix): Expr = exp0 match {
    case Expr.Cst(cst, tpe, loc) =>
      Expr.Cst(cst, tpe, loc)

    case Expr.Var(sym, tpe, loc) =>
      // Check for renamed local binder
      ctx0.varSubst.get(sym) match {
        case None => // Function parameter occurrence
          Expr.Var(sym, tpe, loc)

        case Some(freshVarSym) =>
          ctx0.subst.get(freshVarSym) match {
            // Case 1:
            // The variable `sym` is in the substitution map. Replace `sym` with `e1`.
            case Some(e1) =>
              e1 match {
                case SubstRange.DoneExpr(e) => // Reduced expression
                  e

                case SubstRange.SuspendedExpr(exp) => // Reduce suspended expr
                  visitExp(exp, ctx0)
              }

            // Case 2:
            // The variable `sym` is not in the substitution map, but is considered for inlining.
            case None =>
              ctx0.inScopeVars.get(freshVarSym) match {
                case Some(_) => Expr.Var(freshVarSym, tpe, loc)
                case None => Expr.Var(freshVarSym, tpe, loc)
              }
          }
      }

    case Expr.Lambda(fparam, exp, tpe, loc) =>
      val (fp, varSubst1) = freshFormalParam(fparam)
      val varSubst2 = ctx0.varSubst ++ varSubst1
      val inScopeVars1 = ctx0.inScopeVars + (fp.sym -> BoundKind.ParameterOrPattern)
      val ctx = ctx0.copy(varSubst = varSubst2, inScopeVars = inScopeVars1)
      val e = visitExp(exp, ctx)
      Expr.Lambda(fp, e, tpe, loc)

    case Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp(_, ctx0))
      Expr.ApplyAtomic(op, es, tpe, eff, loc)

    case Expr.ApplyClo(exp1, exp2, tpe, eff, loc) =>
      val exprCtx = ExprContext.AppCtx(exp2, ctx0.subst, ctx0.exprCtx)
      val ctx = ctx0.copy(exprCtx = exprCtx)
      val e1 = visitExp(exp1, ctx)
      val e2 = visitExp(exp2, ctx0)
      Expr.ApplyClo(e1, e2, tpe, eff, loc)

    case Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
      val es = exps.map(visitExp(_, ctx0))
      Expr.ApplyDef(sym, es, itpe, tpe, eff, loc)

    case Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
      ctx0.varSubst.get(sym) match {
        case Some(freshVarSym) =>
          val es = exps.map(visitExp(_, ctx0))
          Expr.ApplyLocalDef(freshVarSym, es, tpe, eff, loc)

        case None =>
          throw InternalCompilerException("unexpected stale local def symbol", loc)
      }

    case Expr.Let(sym, exp1, exp2, tpe, eff, occur, loc) =>
      if (isDead(occur)) {
        if (isPure(exp1.eff)) {
          // Case 1:
          // If `sym` is never used (it is `Dead`)  and `exp1` is pure, so it has no side effects, then it is safe to remove `sym`
          // Both code size and runtime are reduced
          visitExp(exp2, ctx0)
        } else {
          // Case 2:
          // If `sym` is never used (it is `Dead`) so it is safe to make a Stm.
          val e1 = visitExp(exp1, ctx0)
          val e2 = visitExp(exp2, ctx0)
          Expr.Stm(e1, e2, tpe, eff, loc)
        }
      } else {
        val freshVarSym = Symbol.freshVarSym(sym)
        val varSubst1 = ctx0.varSubst + (sym -> freshVarSym)

        // Case 3:
        // If `exp1` occurs once, and it is pure, then it is safe to inline.
        // There is a small decrease in code size and runtime.
        val wantToPreInline = isOnce(occur) && isPure(exp1.eff)
        if (wantToPreInline) {
          val subst1 = ctx0.subst + (freshVarSym -> SubstRange.SuspendedExpr(exp1))
          val ctx = ctx0.copy(varSubst = varSubst1, subst = subst1)
          visitExp(exp2, ctx)
        } else {
          val ctx1 = ctx0.copy(exprCtx = ExprContext.Empty)
          val e1 = visitExp(exp1, ctx1)
          // Case 4:
          // If `e1` is trivial and pure, then it is safe to inline.
          // Code size and runtime are not impacted, because only trivial expressions are inlined
          val wantToPostInline = isTrivialExp(e1) && isPure(e1.eff)
          if (wantToPostInline) {
            // If `e1` is to be inlined:
            // Add map `sym` to `e1` and return `e2` without constructing the let expression.
            val subst1 = ctx0.subst + (freshVarSym -> SubstRange.DoneExpr(e1))
            val ctx2 = ctx0.copy(varSubst = varSubst1, subst = subst1)
            visitExp(exp2, ctx2)
          } else {
            // Case 5:
            // If none of the previous cases pass, `sym` is not inlined. Return a let expression with the visited expressions
            // Code size and runtime are not impacted
            val inScopeSet1 = ctx0.inScopeVars + (freshVarSym -> BoundKind.LetBound(e1, occur))
            val ctx2 = ctx0.copy(varSubst = varSubst1, inScopeVars = inScopeSet1)
            val e2 = visitExp(exp2, ctx2)
            Expr.Let(freshVarSym, e1, e2, tpe, eff, occur, loc)
          }
        }
      }

    case Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, occur, loc) =>
      if (isDead(occur)) {
        visitExp(exp2, ctx0)
      } else {
        val freshVarSym = Symbol.freshVarSym(sym)
        val varSubst1 = ctx0.varSubst + (sym -> freshVarSym)
        val ctx1 = ctx0.copy(varSubst = varSubst1)
        val e2 = visitExp(exp2, ctx1)
        val (fps, varSubsts) = fparams.map(freshFormalParam).unzip
        val varSubst2 = varSubsts.foldLeft(varSubst1)(_ ++ _)
        val inScopeVars = ctx0.inScopeVars ++ fps.map(fp => fp.sym -> BoundKind.ParameterOrPattern)
        val ctx2 = ctx0.copy(varSubst = varSubst2, inScopeVars = inScopeVars)
        val e1 = visitExp(exp1, ctx2)
        Expr.LocalDef(freshVarSym, fps, e1, e2, tpe, eff, occur, loc)
      }

    case Expr.Scope(sym, rvar, exp, tpe, eff, loc) =>
      val freshVarSym = Symbol.freshVarSym(sym)
      val varSubst1 = ctx0.varSubst + (sym -> freshVarSym)
      val inScopeVars1 = ctx0.inScopeVars + (freshVarSym -> BoundKind.ParameterOrPattern)
      val ctx = ctx0.copy(varSubst = varSubst1, inScopeVars = inScopeVars1)
      val e = visitExp(exp, ctx)
      Expr.Scope(freshVarSym, rvar, e, tpe, eff, loc)

    case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = visitExp(exp1, ctx0)
      e1 match {
        case Expr.Cst(Constant.Bool(true), _, _) =>
          visitExp(exp2, ctx0)
        case Expr.Cst(Constant.Bool(false), _, _) =>
          visitExp(exp3, ctx0)
        case _ =>
          val e2 = visitExp(exp2, ctx0)
          val e3 = visitExp(exp3, ctx0)
          Expr.IfThenElse(e1, e2, e3, tpe, eff, loc)
      }

    case Expr.Stm(exp1, exp2, tpe, eff, loc) =>
      // Case 1:
      // If `exp1` is pure, so it has no side effects, then it is safe to remove
      // Both code size and runtime are reduced
      if (isPure(exp1.eff)) {
        visitExp(exp2, ctx0)
      } else {
        val e1 = visitExp(exp1, ctx0)
        val e2 = visitExp(exp2, ctx0)
        Expr.Stm(e1, e2, tpe, eff, loc)
      }

    case Expr.Discard(exp, eff, loc) =>
      val e = visitExp(exp, ctx0)
      Expr.Discard(e, eff, loc)

    case Expr.Match(exp, rules, tpe, eff, loc) =>
      val exprCtx = ExprContext.MatchCtx(rules, ctx0.subst, ctx0.exprCtx)
      val ctx = ctx0.copy(exprCtx = exprCtx)
      val e = visitExp(exp, ctx)
      val rs = rules.map {
        case OccurrenceAst.MatchRule(pat, guard, exp1) =>
          val (p, varSubst1) = visitPattern(pat)
          val varSubst2 = ctx0.varSubst ++ varSubst1
          val inScopeVars1 = ctx0.inScopeVars ++ varSubst1.values.map(sym => sym -> BoundKind.ParameterOrPattern)
          val ctx = ctx0.copy(varSubst = varSubst2, inScopeVars = inScopeVars1)
          val g = guard.map(visitExp(_, ctx))
          val e1 = visitExp(exp1, ctx)
          OccurrenceAst.MatchRule(p, g, e1)
      }
      Expr.Match(e, rs, tpe, eff, loc)

    case Expr.VectorLit(exps, tpe, eff, loc) =>
      val es = exps.map(visitExp(_, ctx0))
      Expr.VectorLit(es, tpe, eff, loc)

    case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1, ctx0)
      val e2 = visitExp(exp2, ctx0)
      Expr.VectorLoad(e1, e2, tpe, eff, loc)

    case Expr.VectorLength(exp, loc) =>
      val e = visitExp(exp, ctx0)
      Expr.VectorLength(e, loc)

    case Expr.Ascribe(exp, tpe, eff, loc) =>
      val e = visitExp(exp, ctx0)
      Expr.Ascribe(e, tpe, eff, loc)

    case Expr.Cast(exp, declaredType, declaredEff, tpe, eff, loc) =>
      val e = visitExp(exp, ctx0)
      Expr.Cast(e, declaredType, declaredEff, tpe, eff, loc)

    case Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp, ctx0)
      val rs = rules.map {
        case OccurrenceAst.CatchRule(sym, clazz, exp1) =>
          val freshVarSym = Symbol.freshVarSym(sym)
          val varSubst1 = ctx0.varSubst + (sym -> freshVarSym)
          val inScopeVars1 = ctx0.inScopeVars + (freshVarSym -> BoundKind.ParameterOrPattern)
          val ctx = ctx0.copy(varSubst = varSubst1, inScopeVars = inScopeVars1)
          val e1 = visitExp(exp1, ctx)
          OccurrenceAst.CatchRule(freshVarSym, clazz, e1)
      }
      Expr.TryCatch(e, rs, tpe, eff, loc)

    case Expr.RunWith(exp, effUse, rules, tpe, eff, loc) =>
      val rs = rules.map {
        case OccurrenceAst.HandlerRule(op, fparams, exp1) =>
          val (fps, varSubsts) = fparams.map(freshFormalParam).unzip
          val varSubst1 = varSubsts.fold(ctx0.varSubst)(_ ++ _)
          val inScopeVars1 = ctx0.inScopeVars ++ fps.map(fp => fp.sym -> BoundKind.ParameterOrPattern)
          val ctx = ctx0.copy(varSubst = varSubst1, inScopeVars = inScopeVars1)
          val e1 = visitExp(exp1, ctx)
          OccurrenceAst.HandlerRule(op, fps, e1)
      }
      val e = visitExp(exp, ctx0)
      Expr.RunWith(e, effUse, rs, tpe, eff, loc)

    case Expr.Do(op, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp(_, ctx0))
      Expr.Do(op, es, tpe, eff, loc)

    case Expr.NewObject(name, clazz, tpe, eff, methods0, loc) =>
      val methods = methods0.map {
        case OccurrenceAst.JvmMethod(ident, fparams, exp, retTpe, eff1, loc1) =>
          val (fps, varSubsts) = fparams.map(freshFormalParam).unzip
          val varSubst1 = varSubsts.fold(ctx0.varSubst)(_ ++ _)
          val inScopeVars1 = ctx0.inScopeVars ++ fps.map(fp => fp.sym -> BoundKind.ParameterOrPattern)
          val ctx = ctx0.copy(varSubst = varSubst1, inScopeVars = inScopeVars1)
          val e = visitExp(exp, ctx)
          OccurrenceAst.JvmMethod(ident, fps, e, retTpe, eff1, loc1)
      }
      Expr.NewObject(name, clazz, tpe, eff, methods, loc)
  }

  /** Returns a formal param with a fresh symbol and a substitution for the old variable. */
  private def freshFormalParam(fp0: OccurrenceAst.FormalParam)(implicit flix: Flix): (OccurrenceAst.FormalParam, Map[Symbol.VarSym, Symbol.VarSym]) = fp0 match {
    case OccurrenceAst.FormalParam(sym, mod, tpe, src, occur, loc) =>
      val freshVarSym = Symbol.freshVarSym(sym)
      val subst = Map(sym -> freshVarSym)
      (OccurrenceAst.FormalParam(freshVarSym, mod, tpe, src, occur, loc), subst)
  }

  /**
    * Returns a pattern with fresh variables and a substitution for the old variables.
    *
    * If a variable is unused it is rewritten to a wild pattern.
    */
  private def visitPattern(pat0: Pattern)(implicit flix: Flix): (Pattern, Map[Symbol.VarSym, Symbol.VarSym]) = pat0 match {
    case Pattern.Wild(tpe, loc) =>
      (Pattern.Wild(tpe, loc), Map.empty)

    case Pattern.Var(sym, tpe, occur, loc) =>
      if (isDead(occur)) {
        (Pattern.Wild(tpe, loc), Map.empty)
      } else {
        val freshVarSym = Symbol.freshVarSym(sym)
        (Pattern.Var(freshVarSym, tpe, occur, loc), Map(sym -> freshVarSym))
      }

    case Pattern.Cst(cst, tpe, loc) =>
      (Pattern.Cst(cst, tpe, loc), Map.empty)

    case Pattern.Tag(sym, pats, tpe, loc) =>
      val (ps, varSubsts) = pats.map(visitPattern).unzip
      val varSubst = varSubsts.foldLeft(Map.empty[Symbol.VarSym, Symbol.VarSym])(_ ++ _)
      (Pattern.Tag(sym, ps, tpe, loc), varSubst)

    case Pattern.Tuple(pats, tpe, loc) =>
      val (ps, varSubsts) = pats.map(visitPattern).unzip
      val varSubst = varSubsts.foldLeft(Map.empty[Symbol.VarSym, Symbol.VarSym])(_ ++ _)
      (Pattern.Tuple(ps, tpe, loc), varSubst)

    case Pattern.Record(pats, pat, tpe, loc) =>
      val (ps, varSubsts) = pats.map(visitRecordLabelPattern).unzip
      val (p, varSubst1) = visitPattern(pat)
      val varSubst2 = varSubsts.foldLeft(varSubst1)(_ ++ _)
      (Pattern.Record(ps, p, tpe, loc), varSubst2)
  }

  private def visitRecordLabelPattern(pat0: Pattern.Record.RecordLabelPattern)(implicit flix: Flix): (Pattern.Record.RecordLabelPattern, Map[Symbol.VarSym, Symbol.VarSym]) = pat0 match {
    case Pattern.Record.RecordLabelPattern(label, pat, tpe, loc) =>
      val (p, subst) = visitPattern(pat)
      (Pattern.Record.RecordLabelPattern(label, p, tpe, loc), subst)
  }

  /** Returns `true` if `eff0` is pure. */
  private def isPure(eff0: Type): Boolean = {
    eff0 == Type.Pure
  }

  /** Checks if `occur` is [[Occur.Dead]]. */
  private def isDead(occur: OccurrenceAst.Occur): Boolean = occur match {
    case Occur.Dead => true
    case Occur.Once => false
    case Occur.OnceInLambda => false
    case Occur.OnceInLocalDef => false
    case Occur.ManyBranch => false
    case Occur.Many => false
  }

  /** Checks if `occur` is [[Occur.Once]]. */
  private def isOnce(occur: OccurrenceAst.Occur): Boolean = occur match {
    case Occur.Dead => false
    case Occur.Once => true
    case Occur.OnceInLambda => false
    case Occur.OnceInLocalDef => false
    case Occur.ManyBranch => false
    case Occur.Many => false
  }

  /**
    * Returns `true` if `exp0` is considered a trivial expression.
    *
    * An expression is trivial if it is a:
    *   - primitive literal (float, string, int, bool, unit)
    *   - variable
    *   - unary expression with a trivial operand
    *   - binary expression with trivial operands
    *   - tag with trivial arguments
    *   - tuple with trivial arguments
    *
    * A pure and trivial expression can always be inlined even without duplicating work.
    */
  private def isTrivialExp(exp0: Expr): Boolean = exp0 match {
    case Expr.Cst(_, _, _) => true
    case Expr.Var(_, _, _) => true
    case Expr.ApplyAtomic(AtomicOp.Unary(_), exps, _, _, _) => exps.forall(isTrivialExp)
    case Expr.ApplyAtomic(AtomicOp.Binary(_), exps, _, _, _) => exps.forall(isTrivialExp)
    case Expr.ApplyAtomic(AtomicOp.Tag(_), exps, _, _, _) => exps.forall(isTrivialExp)
    case Expr.ApplyAtomic(AtomicOp.Tuple, exps, _, _, _) => exps.forall(isTrivialExp)
    case _ => false
  }

  /** Represents the range of a substitution from variables to expressions. */
  sealed private trait SubstRange

  private object SubstRange {

    /** An expression that will be inlined but is not yet visited. */
    case class SuspendedExpr(exp: OccurrenceAst.Expr) extends SubstRange

    /** An expression that will be inlined but has already been visited. */
    case class DoneExpr(exp: OccurrenceAst.Expr) extends SubstRange

  }

  /** Contains information on a variable's definition. */
  private sealed trait BoundKind

  private object BoundKind {

    /** Variable is bound by either a parameter or a pattern. Its value is unknown. */
    object ParameterOrPattern extends BoundKind

    /** The right-hand side of a let-bound variable along with its occurrence information. */
    case class LetBound(expr: OccurrenceAst.Expr, occur: Occur) extends BoundKind

  }

  /** Represents the compile-time evaluation state and is used like a stack. */
  private sealed trait ExprContext

  private object ExprContext {

    /** The empty evaluation context. */
    case object Empty extends ExprContext

    /** Function application context. */
    case class AppCtx(expr: Expr, subst: Map[Symbol.VarSym, SubstRange], ctx: ExprContext) extends ExprContext

    /** Match-case expression context. */
    case class MatchCtx(rules: List[OccurrenceAst.MatchRule], subst: Map[Symbol.VarSym, SubstRange], ctx: ExprContext) extends ExprContext

  }

  /** Denotes the level at which the binder is declared. */
  sealed trait Level

  private object Level {

    /** A [[OccurrenceAst.Def]] declaration. The def can be in modules or top-level. */
    case object Def extends Level

    /** Nested inside a [[OccurrenceAst.Def]]. This can be a lambda or local def. */
    case object Nested extends Level

  }

  /**
    * A wrapper class for all the different inlining environments.
    *
    * @param varSubst          a substitution on variables to variables.
    * @param subst             a substitution on variables to expressions.
    * @param inScopeVars       a set of variables considered to be in scope.
    * @param exprCtx           a compile-time evaluation context.
    * @param currentlyInlining a flag denoting whether the current traversal is part of an inline-expansion process.
    */
  private case class LocalContext(varSubst: Map[Symbol.VarSym, Symbol.VarSym], subst: Map[Symbol.VarSym, SubstRange], inScopeVars: Map[Symbol.VarSym, BoundKind], exprCtx: ExprContext, currentlyInlining: Boolean)

  private object LocalContext {

    /** Returns the empty context with `currentlyInlining` set to `false`. */
    val Empty: LocalContext = LocalContext(Map.empty, Map.empty, Map.empty, ExprContext.Empty, currentlyInlining = false)

  }
}
