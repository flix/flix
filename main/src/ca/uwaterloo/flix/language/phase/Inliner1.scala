/*
 * Copyright 2018 Magnus Madsen, Anna Krogh, Patrick Lundvig, Christian Bonde
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
import ca.uwaterloo.flix.language.ast.OccurrenceAst1.Occur.*
import ca.uwaterloo.flix.language.ast.OccurrenceAst1.{DefContext, Expr, Occur, Pattern}
import ca.uwaterloo.flix.language.ast.shared.Constant
import ca.uwaterloo.flix.language.ast.{AtomicOp, MonoAst, OccurrenceAst1, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.collection.{CofiniteSet, ListMap}
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.CollectionHasAsScala

/**
  * The inliner optionally performs beta-reduction at call-sites.
  */
object Inliner1 {

  /**
    * Performs inlining on the given AST `root`.
    */
  def run(root: OccurrenceAst1.Root)(implicit flix: Flix): (OccurrenceAst1.Root, Optimizer1.Stats) = {
    implicit val sctx: SharedContext = SharedContext.mk()
    val defs = ParOps.parMapValues(root.defs)(visitDef(_)(root, sctx, flix))
    val newRoot = OccurrenceAst1.Root(defs, root.enums, root.structs, root.effects, root.mainEntryPoint, root.entryPoints, root.sources)
    val stats = SharedContext.toStats(sctx)
    (newRoot, stats)
  }

  private type InVar = Symbol.VarSym

  private type OutVar = Symbol.VarSym

  private type InExpr = OccurrenceAst1.Expr

  private type OutExpr = OccurrenceAst1.Expr

  // private type InCase = MonoAst.Case

  //  private type OutCase = MonoAst.Case

  sealed private trait SubstRange

  private object SubstRange {

    case class SuspendedExp(exp: InExpr) extends SubstRange

    case class DoneExp(exp: OutExpr) extends SubstRange

  }

  //  private sealed trait Const

  //   private object Const {

  //     case class Lit(lit: Constant) extends Const

  //     case class Tag(tag: Symbol.CaseSym) extends Const

  // }

  private sealed trait Definition

  private object Definition {

    //     case object CaseOrLambda extends Definition

    case class LetBound(expr: OutExpr, occur: Occur) extends Definition

  }

  private type VarSubst = Map[InVar, OutVar]

  private type Subst = Map[InVar, SubstRange]

  private type InScopeVars = Map[OutVar, Definition]

  case class Handler(rule: OccurrenceAst1.HandlerRule)

  private type OutEff = MonoAst.Op.type

  private type InScopeEffs = Map[OutEff, Handler]

  private sealed trait InliningContext

  private object InliningContext {

    case object Start extends InliningContext

    case object Stop extends InliningContext

    // case class Application(inExpr: InExpr, subst: Subst, context: Context) extends Context

    // case class Argument(continuation: OutExpr => OutExpr) extends Context

    // case class Match(sym: InVar, cases: List[InCase], subst: Subst, context: Context) extends Context

  }

  private case class Context(varSubst: VarSubst, subst: Subst, inScopeVars: InScopeVars, inliningContext: InliningContext)

  private object Context {
    def emptyStart: Context = Context(Map.empty, Map.empty, Map.empty, InliningContext.Start)

    def emptyStop: Context = Context(Map.empty, Map.empty, Map.empty, InliningContext.Stop)
  }

  /**
    * Performs expression inlining on the given definition `def0`.
    * Converts definition from [[OccurrenceAst1]] to [[OccurrenceAst1]].
    */
  private def visitDef(def0: OccurrenceAst1.Def)(implicit root: OccurrenceAst1.Root, sctx: SharedContext, flix: Flix): OccurrenceAst1.Def = def0 match {
    case OccurrenceAst1.Def(sym, fparams, spec, exp, ctx, loc) =>
      if (ctx.occur != Dangerous) {
        val e = visitExp(exp, Context.emptyStart)(sym, root, sctx, flix)
        OccurrenceAst1.Def(sym, fparams, spec, e, ctx, loc)
      } else {
        OccurrenceAst1.Def(sym, fparams, spec, exp, ctx, loc)
      }
  }

  /**
    * Performs inlining operations on the expression `exp0` from [[Expr]].
    * Returns a [[Expr]]
    */
  private def visitExp(exp0: Expr, ctx0: Context)(implicit sym0: Symbol.DefnSym, root: OccurrenceAst1.Root, sctx: SharedContext, flix: Flix): Expr = exp0 match {
    case Expr.Cst(cst, tpe, loc) =>
      Expr.Cst(cst, tpe, loc)

    case Expr.Var(sym, tpe, loc) =>
      // Check for renamed local binder
      ctx0.varSubst.get(sym) match {
        case Some(freshVarSym) => ctx0.subst.get(freshVarSym) match {
          // Case 1:
          // The variable `sym` is not in the substitution map and will not be inlined.
          case None => Expr.Var(freshVarSym, tpe, loc)
          // Case 2:
          // The variable `sym` is in the substitution map. Replace `sym` with `e1`.
          case Some(e1) =>
            sctx.inlinedVars.add((sym0, sym))
            e1 match {
              // If `e1` is a `LiftedExp` then `e1` has already been visited
              case SubstRange.DoneExp(e) =>
                //assert(e.tpe == tpe, s"expected '$tpe', got '${e.tpe}' at $loc, inlining '$sym' into '$sym0'")
                e
              // If `e1` is a `OccurrenceExp` then `e1` has not been visited. Visit `e1`
              case SubstRange.SuspendedExp(exp) =>
                val e = visitExp(exp, ctx0)
                //assert(e.tpe == tpe, s"expected '$tpe', got '${e.tpe}' at $loc, inlining '$sym' into '$sym0'")
                e
            }
        }
        case None => // Function parameter occurrence
          Expr.Var(sym, tpe, loc)
      }

    case Expr.Lambda((fparam, occur), exp, tpe, loc) => // TODO: Make parameter wild if dead
      val (fps, varSubst1) = freshFormalParam(fparam)
      val varSubst2 = ctx0.varSubst ++ varSubst1
      val ctx = ctx0.copy(varSubst = varSubst2)
      val e = visitExp(exp, ctx)
      Expr.Lambda((fps, occur), e, tpe, loc)

    case Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp(_, ctx0))
      Expr.ApplyAtomic(op, es, tpe, eff, loc)

    case Expr.ApplyClo(exp1, exp2, tpe, eff, loc) =>
      // TODO: Refactor this to use Context, so inlining and beta reduction cases are moved to Var and Lambda exprs respectively.
      val e2 = visitExp(exp2, ctx0)

      def maybeInline(sym1: OutVar): Expr.ApplyClo = {
        ctx0.inScopeVars.get(sym1) match {
          case Some(Definition.LetBound(lambda, Occur.OnceInLocalDef)) =>
            sctx.inlinedVars.add((sym0, sym1))
            val e1 = refreshBinders(lambda)(Map.empty, flix)
            Expr.ApplyClo(e1, e2, tpe, eff, loc)

          case Some(Definition.LetBound(lambda, Occur.Once)) =>
            sctx.inlinedVars.add((sym0, sym1))
            val e1 = refreshBinders(lambda)(Map.empty, flix)
            Expr.ApplyClo(e1, e2, tpe, eff, loc)

          case _ =>
            val e1 = visitExp(exp1, ctx0)
            Expr.ApplyClo(e1, e2, tpe, eff, loc)
        }
      }

      exp1 match {
        case Expr.Var(sym, _, _) =>
          ctx0.varSubst.get(sym) match {
            case Some(freshVarSym) => maybeInline(freshVarSym)

            case None =>
              // If it is the inScopeSet then we have added it via a let-binding so the varSubst should contain sym.
              // Thus, this is only possible if `sym` is a parameter of the top-level def.
              val e1 = visitExp(exp1, ctx0)
              Expr.ApplyClo(e1, e2, tpe, eff, loc)
          }

        case Expr.Lambda(fparam, body, _, _) =>
          // Direct application, e.g., (x -> x)(1)
          sctx.betaReductions.add((sym0, 1))
          inlineLocalAbstraction(body, List(fparam), List(e2), ctx0)

        case _ =>
          val e1 = visitExp(exp1, ctx0)
          Expr.ApplyClo(e1, e2, tpe, eff, loc)
      }

    case Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
      val es = exps.map(visitExp(_, ctx0))
      val def1 = root.defs.apply(sym)
      // If `def1` is a single non-self call or is trivial
      // then inline the body of `def1`
      if (canInlineDef(def1.context, ctx0.inliningContext)) {
        sctx.inlinedDefs.add((sym0, sym))
        inlineDef(def1.exp, def1.fparams, es)
      } else {
        Expr.ApplyDef(sym, es, itpe, tpe, eff, loc)
      }

    case Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
      ctx0.varSubst.get(sym) match {
        case Some(freshVarSym) =>
          val es = exps.map(visitExp(_, ctx0))
          Expr.ApplyLocalDef(freshVarSym, es, tpe, eff, loc)

        case None => throw InternalCompilerException("unexpected stale local def symbol", loc)
      }

    case Expr.Let(sym, exp1, exp2, tpe, eff, occur, loc) =>
      if (isDead(occur)) {
        if (isPure(exp1.eff)) {
          // Case 1:
          // If `sym` is never used (it is `Dead`)  and `exp1` is pure, so it has no side effects, then it is safe to remove `sym`
          // Both code size and runtime are reduced
          sctx.eliminatedVars.add((sym0, sym))
          visitExp(exp2, ctx0)
        } else {
          // Case 2:
          // If `sym` is never used (it is `Dead`) so it is safe to make a Stm.
          sctx.eliminatedVars.add((sym0, sym))
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
        val wantToPreInline = isUsedOnceAndPure(occur, exp1.eff)
        if (wantToPreInline) {
          sctx.eliminatedVars.add((sym0, sym))
          val subst1 = ctx0.subst + (freshVarSym -> SubstRange.SuspendedExp(exp1))
          val ctx = ctx0.copy(varSubst = varSubst1, subst = subst1)
          visitExp(exp2, ctx)
        } else {
          val e1 = visitExp(exp1, ctx0)
          // Case 4:
          // If `e1` is trivial and pure, then it is safe to inline.
          // Code size and runtime are not impacted, because only trivial expressions are inlined
          val wantToPostInline = isTrivialAndPure(e1, exp1.eff) && occur != DontInline && occur != Dangerous
          if (wantToPostInline) {
            // If `e1` is to be inlined:
            // Add map `sym` to `e1` and return `e2` without constructing the let expression.
            sctx.eliminatedVars.add((sym0, sym))
            val subst1 = ctx0.subst + (freshVarSym -> SubstRange.DoneExp(e1))
            val ctx = ctx0.copy(varSubst = varSubst1, subst = subst1)
            visitExp(exp2, ctx)
          } else {
            // Case 5:
            // If none of the previous cases pass, `sym` is not inlined. Return a let expression with the visited expressions
            // Code size and runtime are not impacted
            val inScopeSet1 = ctx0.inScopeVars + (freshVarSym -> Definition.LetBound(e1, occur))
            val ctx = ctx0.copy(varSubst = varSubst1, inScopeVars = inScopeSet1)
            val e2 = visitExp(exp2, ctx)
            Expr.Let(freshVarSym, e1, e2, tpe, eff, occur, loc)
          }
        }
      }

    case Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, occur, loc) =>
      // TODO: Treat like let-binding above, except it is never trivial so some checks can be omitted
      if (isDead(occur)) { // Probably never happens
        sctx.eliminatedVars.add((sym0, sym))
        visitExp(exp2, ctx0)
      } else {
        val freshVarSym = Symbol.freshVarSym(sym)
        val varSubst1 = ctx0.varSubst + (sym -> freshVarSym)
        val ctx1 = ctx0.copy(varSubst = varSubst1)
        val e2 = visitExp(exp2, ctx1)
        val (fps, varSubsts) = fparams.map {
          case (fp, fpOccur) =>
            val (freshFp, varSubstTmp) = freshFormalParam(fp)
            ((freshFp, fpOccur), varSubstTmp)
        }.unzip
        val varSubst2 = varSubsts.foldLeft(varSubst1)(_ ++ _)
        val ctx2 = ctx0.copy(varSubst = varSubst2)
        val e1 = visitExp(exp1, ctx2)
        Expr.LocalDef(freshVarSym, fps, e1, e2, tpe, eff, occur, loc)
      }

    case Expr.Scope(sym, rvar, exp, tpe, eff, loc) =>
      val freshVarSym = Symbol.freshVarSym(sym)
      val varSubst1 = ctx0.varSubst + (sym -> freshVarSym)
      val ctx = ctx0.copy(varSubst = varSubst1)
      val e = visitExp(exp, ctx)
      Expr.Scope(freshVarSym, rvar, e, tpe, eff, loc)

    case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = visitExp(exp1, ctx0)
      e1 match {
        case Expr.Cst(Constant.Bool(true), _, _) =>
          sctx.simplifiedIfThenElse.add((sym0, 1))
          visitExp(exp2, ctx0)
        case Expr.Cst(Constant.Bool(false), _, _) =>
          sctx.simplifiedIfThenElse.add((sym0, 1))
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
        sctx.eliminatedStms.add((sym0, 1))
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
      val e = visitExp(exp, ctx0)
      val rs = rules.map {
        case OccurrenceAst1.MatchRule(pat, guard, exp1) =>
          val (p, varSubst1) = visitPattern(pat)
          val varSubst2 = ctx0.varSubst ++ varSubst1
          val ctx = ctx0.copy(varSubst = varSubst2)
          val g = guard.map(visitExp(_, ctx))
          val e1 = visitExp(exp1, ctx)
          OccurrenceAst1.MatchRule(p, g, e1)
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
        case OccurrenceAst1.CatchRule(sym, clazz, exp1) =>
          val freshVarSym = Symbol.freshVarSym(sym)
          val varSubst1 = ctx0.varSubst + (sym -> freshVarSym)
          val ctx = ctx0.copy(varSubst = varSubst1)
          val e1 = visitExp(exp1, ctx)
          OccurrenceAst1.CatchRule(freshVarSym, clazz, e1)
      }
      Expr.TryCatch(e, rs, tpe, eff, loc)

    case Expr.RunWith(exp, effUse, rules, tpe, eff, loc) =>
      val e = visitExp(exp, ctx0)
      val rs = rules.map {
        case OccurrenceAst1.HandlerRule(op, fparams, exp1, linearity) =>
          val (fps, varSubsts) = fparams.map(freshFormalParam).unzip
          val varSubst1 = varSubsts.fold(ctx0.varSubst)(_ ++ _)
          val ctx = ctx0.copy(varSubst = varSubst1)
          val e1 = visitExp(exp1, ctx)
          OccurrenceAst1.HandlerRule(op, fps, e1, linearity)
      }
      Expr.RunWith(e, effUse, rs, tpe, eff, loc)

    case Expr.Do(op, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp(_, ctx0))
      Expr.Do(op, es, tpe, eff, loc)

    case Expr.NewObject(name, clazz, tpe, eff, methods0, loc) =>
      val methods = methods0.map {
        case OccurrenceAst1.JvmMethod(ident, fparams, exp, retTpe, eff1, loc1) =>
          val (fps, varSubsts) = fparams.map(freshFormalParam).unzip
          val varSubst1 = varSubsts.fold(ctx0.varSubst)(_ ++ _)
          val ctx = ctx0.copy(varSubst = varSubst1)
          val e = visitExp(exp, ctx)
          OccurrenceAst1.JvmMethod(ident, fps, e, retTpe, eff1, loc1)
      }
      Expr.NewObject(name, clazz, tpe, eff, methods, loc)
  }

  /**
    * Recursively bind each argument in `args` to a let-expression with a fresh symbol
    * Add corresponding symbol from `symbols` to substitution map `env0`, mapping old symbols to fresh symbols.
    */
  private def inlineLocalAbstraction(exp0: Expr, symbols: List[(MonoAst.FormalParam, Occur)], args: List[OutExpr], ctx0: Context)(implicit sym0: Symbol.DefnSym, root: OccurrenceAst1.Root, sctx: SharedContext, flix: Flix): Expr = {
    bind(exp0, symbols, args, ctx0)
  }

  private def visitPattern(pattern0: Pattern)(implicit flix: Flix): (Pattern, VarSubst) = pattern0 match {
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
      val varSubst = varSubsts.foldLeft(Map.empty[InVar, OutVar])(_ ++ _)
      (Pattern.Tag(sym, ps, tpe, loc), varSubst)

    case Pattern.Tuple(pats, tpe, loc) =>
      val (ps, varSubsts) = pats.map(visitPattern).unzip
      val varSubst = varSubsts.foldLeft(Map.empty[InVar, OutVar])(_ ++ _)
      (Pattern.Tuple(ps, tpe, loc), varSubst)

    case Pattern.Record(pats, pat, tpe, loc) =>
      val (ps, varSubsts) = pats.map(visitRecordLabelPattern).unzip
      val (p, varSubst1) = visitPattern(pat)
      val varSubst2 = varSubsts.foldLeft(varSubst1)(_ ++ _)
      (Pattern.Record(ps, p, tpe, loc), varSubst2)
  }

  private def visitRecordLabelPattern(pattern0: Pattern.Record.RecordLabelPattern)(implicit flix: Flix): (Pattern.Record.RecordLabelPattern, VarSubst) = pattern0 match {
    case Pattern.Record.RecordLabelPattern(label, pat, tpe, loc) =>
      val (p, subst) = visitPattern(pat)
      (Pattern.Record.RecordLabelPattern(label, p, tpe, loc), subst)
  }

  /**
    * Recursively bind each argument in `args` to a let-expression with a fresh symbol
    * Add corresponding symbol from `symbols` to substitution map `env0`, mapping old symbols to fresh symbols.
    */
  private def inlineDef(exp0: Expr, symbols: List[(MonoAst.FormalParam, Occur)], args: List[OutExpr])(implicit sym0: Symbol.DefnSym, root: OccurrenceAst1.Root, sctx: SharedContext, flix: Flix): Expr = {
    bind(exp0, symbols, args, Context.emptyStop)
  }

  /**
    * Recursively bind each argument in `args` to a let-expression with a fresh symbol
    * Add corresponding symbol from `symbols` to substitution map `env0`, mapping old symbols to fresh symbols.
    */
  private def bind(exp0: Expr, symbols: List[(MonoAst.FormalParam, Occur)], args: List[OutExpr], ctx0: Context)(implicit sym0: Symbol.DefnSym, root: OccurrenceAst1.Root, sctx: SharedContext, flix: Flix): Expr = {
    def bnd(syms: List[(MonoAst.FormalParam, Occur)], as: List[OutExpr], env: VarSubst): Expr = (syms, as) match {
      case ((_, occur) :: nextSymbols, e1 :: nextExpressions) if isDeadAndPure(occur, e1.eff) =>
        // If the parameter is unused and the argument is pure, then throw it away.
        bnd(nextSymbols, nextExpressions, env)

      case ((_, occur) :: nextSymbols, e1 :: nextExpressions) if isDead(occur) =>
        // If the parameter is unused and the argument is NOT pure, then put it in a statement.
        val nextLet = bnd(nextSymbols, nextExpressions, env)
        val eff = canonicalEffect(Type.mkUnion(e1.eff, nextLet.eff, e1.loc))
        Expr.Stm(e1, nextLet, nextLet.tpe, eff, exp0.loc)

      case ((MonoAst.FormalParam(sym, _, _, _, _), occur) :: nextSymbols, e1 :: nextExpressions) =>
        val freshVar = Symbol.freshVarSym(sym)
        val env1 = env + (sym -> freshVar)
        val nextLet = bnd(nextSymbols, nextExpressions, env1)
        val eff = canonicalEffect(Type.mkUnion(e1.eff, nextLet.eff, e1.loc))
        Expr.Let(freshVar, e1, nextLet, nextLet.tpe, eff, occur, exp0.loc)

      case _ =>
        val varSubst1 = ctx0.varSubst ++ env
        val ctx = ctx0.copy(varSubst1)
        visitExp(exp0, ctx)
    }

    bnd(symbols, args, Map.empty)
  }

  private def freshFormalParam(fp0: MonoAst.FormalParam)(implicit flix: Flix): (MonoAst.FormalParam, VarSubst) = fp0 match {
    case MonoAst.FormalParam(sym, mod, tpe, src, loc) =>
      val freshVarSym = Symbol.freshVarSym(sym)
      val subst = Map(sym -> freshVarSym)
      (MonoAst.FormalParam(freshVarSym, mod, tpe, src, loc), subst)
  }

  private def freshFormalParams(fparams0: List[MonoAst.FormalParam])(implicit flix: Flix): (List[MonoAst.FormalParam], VarSubst) = {
    val (fps, substs) = fparams0.map(freshFormalParam).unzip
    val subst = substs.reduceLeft(_ ++ _)
    (fps, subst)
  }

  private def refreshBinders(expr0: OutExpr)(implicit subst0: VarSubst, flix: Flix): OutExpr = expr0 match {
    case Expr.Cst(cst, tpe, loc) =>
      Expr.Cst(cst, tpe, loc)

    case Expr.Var(sym, tpe, loc) =>
      val freshVarSym = subst0.getOrElse(sym, sym)
      Expr.Var(freshVarSym, tpe, loc)

    case Expr.Lambda((fparam, occur), exp, tpe, loc) =>
      val (fp, subst1) = freshFormalParam(fparam)
      val subst2 = subst0 ++ subst1
      val e = refreshBinders(exp)(subst2, flix)
      Expr.Lambda((fp, occur), e, tpe, loc)

    case Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
      val es = exps.map(refreshBinders)
      Expr.ApplyAtomic(op, es, tpe, eff, loc)

    case Expr.ApplyClo(exp1, exp2, tpe, eff, loc) =>
      val e1 = refreshBinders(exp1)
      val e2 = refreshBinders(exp2)
      Expr.ApplyClo(e1, e2, tpe, eff, loc)

    case Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
      val es = exps.map(refreshBinders)
      Expr.ApplyDef(sym, es, itpe, tpe, eff, loc)

    case Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
      val freshVarSym = subst0.getOrElse(sym, sym)
      val es = exps.map(refreshBinders)
      Expr.ApplyLocalDef(freshVarSym, es, tpe, eff, loc)

    case Expr.Let(sym, exp1, exp2, tpe, eff, occur, loc) =>
      val freshVarSym = Symbol.freshVarSym(sym)
      val subst1 = subst0 + (sym -> freshVarSym)
      val e1 = refreshBinders(exp1)
      val e2 = refreshBinders(exp2)(subst1, flix)
      Expr.Let(freshVarSym, e1, e2, tpe, eff, occur, loc)

    case Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, occur, loc) =>
      val freshVarSym = Symbol.freshVarSym(sym)
      val subst1 = subst0 + (sym -> freshVarSym)
      val e2 = refreshBinders(exp2)(subst1, flix)
      val (fps, varSubstsTmp) = fparams.map {
        case (fp, fpOccur) =>
          val (freshFp, varSubstTmp) = freshFormalParam(fp)
          ((freshFp, fpOccur), varSubstTmp)
      }.unzip
      val subst2 = varSubstsTmp.reduceLeft(_ ++ _)
      val subst3 = subst1 ++ subst2
      val e1 = refreshBinders(exp1)(subst3, flix)
      Expr.LocalDef(freshVarSym, fps, e1, e2, tpe, eff, occur, loc)

    case Expr.Scope(sym, regionVar, exp, tpe, eff, loc) =>
      val freshVarSym = Symbol.freshVarSym(sym)
      val subst1 = subst0 + (sym -> freshVarSym)
      val e = refreshBinders(exp)(subst1, flix)
      Expr.Scope(freshVarSym, regionVar, e, tpe, eff, loc)

    case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = refreshBinders(exp1)
      val e2 = refreshBinders(exp2)
      val e3 = refreshBinders(exp3)
      Expr.IfThenElse(e1, e2, e3, tpe, eff, loc)

    case Expr.Stm(exp1, exp2, tpe, eff, loc) =>
      val e1 = refreshBinders(exp1)
      val e2 = refreshBinders(exp2)
      Expr.Stm(e1, e2, tpe, eff, loc)

    case Expr.Discard(exp, eff, loc) =>
      val e = refreshBinders(exp)
      Expr.Discard(e, eff, loc)

    case Expr.Match(exp, rules, tpe, eff, loc) =>
      val e = refreshBinders(exp)
      val rs = rules.map {
        case OccurrenceAst1.MatchRule(pat, guard, exp1) =>
          val (p, subst1) = refreshPattern(pat)
          val subst2 = subst0 ++ subst1
          val g = guard.map(refreshBinders(_)(subst2, flix))
          val e = refreshBinders(exp1)(subst2, flix)
          OccurrenceAst1.MatchRule(p, g, e)
      }
      Expr.Match(e, rs, tpe, eff, loc)

    case Expr.VectorLit(exps, tpe, eff, loc) =>
      val es = exps.map(refreshBinders)
      Expr.VectorLit(es, tpe, eff, loc)

    case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = refreshBinders(exp1)
      val e2 = refreshBinders(exp2)
      Expr.VectorLoad(e1, e2, tpe, eff, loc)

    case Expr.VectorLength(exp, loc) =>
      val e = refreshBinders(exp)
      Expr.VectorLength(e, loc)

    case Expr.Ascribe(exp, tpe, eff, loc) =>
      val e = refreshBinders(exp)
      Expr.Ascribe(e, tpe, eff, loc)

    case Expr.Cast(exp, declaredType, declaredEff, tpe, eff, loc) =>
      val e = refreshBinders(exp)
      Expr.Cast(e, declaredType, declaredEff, tpe, eff, loc)

    case Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val e = refreshBinders(exp)
      val rs = rules.map {
        case OccurrenceAst1.CatchRule(sym, clazz, exp1) =>
          val freshVarSym = Symbol.freshVarSym(sym)
          val subst1 = subst0 + (sym -> freshVarSym)
          val e1 = refreshBinders(exp1)(subst1, flix)
          OccurrenceAst1.CatchRule(freshVarSym, clazz, e1)
      }
      Expr.TryCatch(e, rs, tpe, eff, loc)

    case Expr.RunWith(exp, effUse, rules, tpe, eff, loc) =>
      val e = refreshBinders(exp)
      val rs = rules.map {
        case OccurrenceAst1.HandlerRule(op, fparams, exp1, linearity) =>
          val (fps, subst1) = freshFormalParams(fparams)
          val subst2 = subst0 ++ subst1
          val e1 = refreshBinders(exp1)(subst2, flix)
          OccurrenceAst1.HandlerRule(op, fps, e1, linearity)
      }
      Expr.RunWith(e, effUse, rs, tpe, eff, loc)

    case Expr.Do(op, exps, tpe, eff, loc) =>
      val es = exps.map(refreshBinders)
      Expr.Do(op, es, tpe, eff, loc)

    case Expr.NewObject(name, clazz, tpe, eff, methods, loc) =>
      val ms = methods.map {
        case OccurrenceAst1.JvmMethod(ident, fparams, exp, retTpe, eff1, loc1) =>
          val (fps, subst1) = freshFormalParams(fparams)
          val subst2 = subst0 ++ subst1
          val e = refreshBinders(exp)(subst2, flix)
          OccurrenceAst1.JvmMethod(ident, fps, e, retTpe, eff1, loc1)
      }
      Expr.NewObject(name, clazz, tpe, eff, ms, loc)
  }

  private def refreshPattern(pattern0: Pattern)(implicit flix: Flix): (Pattern, VarSubst) = pattern0 match {
    case Pattern.Wild(tpe, loc) =>
      (Pattern.Wild(tpe, loc), Map.empty)

    case Pattern.Var(sym, tpe, occur, loc) =>
      val freshVarSym = Symbol.freshVarSym(sym)
      (Pattern.Var(freshVarSym, tpe, occur, loc), Map(sym -> freshVarSym))

    case Pattern.Cst(cst, tpe, loc) =>
      (Pattern.Cst(cst, tpe, loc), Map.empty)

    case Pattern.Tag(sym, pats, tpe, loc) =>
      val (ps, substs) = pats.map(refreshPattern).unzip
      val subst = substs.foldLeft(Map.empty[Symbol.VarSym, Symbol.VarSym])(_ ++ _)
      (Pattern.Tag(sym, ps, tpe, loc), subst)

    case Pattern.Tuple(pats, tpe, loc) =>
      val (ps, substs) = pats.map(refreshPattern).unzip
      val subst = substs.foldLeft(Map.empty[Symbol.VarSym, Symbol.VarSym])(_ ++ _)
      (Pattern.Tuple(ps, tpe, loc), subst)

    case Pattern.Record(pats, pat, tpe, loc) =>
      val (ps, substs) = pats.map(refreshRecordLabelPattern).unzip
      val (p, subst) = refreshPattern(pat)
      val subst1 = substs.foldLeft(subst)(_ ++ _)
      (Pattern.Record(ps, p, tpe, loc), subst1)
  }

  private def refreshRecordLabelPattern(pattern0: Pattern.Record.RecordLabelPattern)(implicit flix: Flix): (Pattern.Record.RecordLabelPattern, VarSubst) = pattern0 match {
    case Pattern.Record.RecordLabelPattern(label, pat, tpe, loc) =>
      val (p, subst) = refreshPattern(pat)
      (Pattern.Record.RecordLabelPattern(label, p, tpe, loc), subst)
  }

  /**
    * A function below the soft threshold is typically inlined.
    */
  private val SoftInlineThreshold: Int = 4

  /**
    * A function above the hard threshold is never inlined.
    */
  private val HardInlineThreshold: Int = 8

  /**
    * Returns `true` if `def0` should be inlined.
    */
  private def canInlineDef(ctx: DefContext, inlCtx: InliningContext): Boolean = {
    val mayInline = ctx.occur != DontInline && ctx.occur != Dangerous && !ctx.isSelfRecursive && inlCtx != InliningContext.Stop
    val isSomewhereOnce = ctx.occur == Once || ctx.occur == OnceInLambda || ctx.occur == OnceInLocalDef
    val belowSoft = ctx.size - ctx.localDefs * 8 < SoftInlineThreshold
    val belowHard = ctx.size - ctx.localDefs * 8 < HardInlineThreshold
    val shouldInline = belowSoft || (ctx.isDirectCall && belowHard) || (isSomewhereOnce && belowHard)
    mayInline && shouldInline
  }

  private def isPure(eff0: Type): Boolean = {
    eff0 == Type.Pure
  }

  /**
    * Checks if `occur` is Dead.
    */
  private def isDead(occur: OccurrenceAst1.Occur): Boolean = occur match {
    case Dead => true
    case _ => false
  }

  /**
    * Checks if `occur` is Once and `eff` is Pure
    */
  private def isUsedOnceAndPure(occur: OccurrenceAst1.Occur, eff0: Type): Boolean = occur match {
    case Once => isPure(eff0)
    case _ => false
  }

  /**
    * Checks if `exp0` is trivial and `eff` is pure
    */
  private def isTrivialAndPure(exp0: Expr, eff0: Type): Boolean = {
    isPure(eff0) && isTrivialExp(exp0)
  }

  /**
    * Checks if `occur` is dead and `exp` is pure.
    */
  private def isDeadAndPure(occur: OccurrenceAst1.Occur, eff0: Type): Boolean = occur match {
    case Dead => isPure(eff0)
    case _ => false
  }

  /** Returns a canonical effect type equivalent to `eff` */
  private def canonicalEffect(eff: Type): Type = {
    evalToType(eval(eff), eff.loc)
  }

  /** Evaluates a ground, simplified effect type */
  private def eval(eff: Type): CofiniteSet[Symbol.EffectSym] = eff match {
    case Type.Univ => CofiniteSet.universe
    case Type.Pure => CofiniteSet.empty
    case Type.Cst(TypeConstructor.Effect(sym), _) =>
      CofiniteSet.mkSet(sym)
    case Type.Apply(Type.Cst(TypeConstructor.Complement, _), y, _) =>
      CofiniteSet.complement(eval(y))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Union, _), x, _), y, _) =>
      CofiniteSet.union(eval(x), eval(y))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Intersection, _), x, _), y, _) =>
      CofiniteSet.intersection(eval(x), eval(y))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Difference, _), x, _), y, _) =>
      CofiniteSet.difference(eval(x), eval(y))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SymmetricDiff, _), x, _), y, _) =>
      CofiniteSet.xor(eval(x), eval(y))
    case other => throw InternalCompilerException(s"Unexpected effect $other", other.loc)
  }

  /** Returns the [[Type]] representation of `set` with `loc`. */
  private def evalToType(set: CofiniteSet[Symbol.EffectSym], loc: SourceLocation): Type = set match {
    case CofiniteSet.Set(s) => Type.mkUnion(s.toList.map(sym => Type.Cst(TypeConstructor.Effect(sym), loc)), loc)
    case CofiniteSet.Compl(s) => Type.mkComplement(Type.mkUnion(s.toList.map(sym => Type.Cst(TypeConstructor.Effect(sym), loc)), loc), loc)
  }


  /**
    * Returns `true` if `exp0` is considered a trivial expression.
    *
    * An expression is trivial if:
    * It is either a literal (float, string, int, bool, unit), or it is a variable.
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

  /**
    * A context shared across threads.
    *
    * We use a concurrent (non-blocking) linked queue to ensure thread-safety.
    *
    * inlinedDefs is a map where each def `def1` points to a set of defs that have been inlined into `def1`.
    * inlinedVars is a map where each def `def1` points to a set of vars that have been inlined at their use sites in `def1`.
    * betaReductions is a map where each def `def1` points to the number of beta reductions that have been performed in `def1`.
    * eliminatedVars is a map where each def `def1` points to the vars that have been removed from `def1`.
    * simplifiedIfThenElse is a map where each def `def1` points to the number of simplifications of `if (constant) e1 else e2` in `def1`.
    * eliminatedStms is a map where each def `def1` points to the number of removed Stms that were pure `def1`.
    */
  private case class SharedContext(inlinedDefs: ConcurrentLinkedQueue[(Symbol.DefnSym, Symbol.DefnSym)], inlinedVars: ConcurrentLinkedQueue[(Symbol.DefnSym, Symbol.VarSym)], betaReductions: ConcurrentLinkedQueue[(Symbol.DefnSym, Int)], eliminatedVars: ConcurrentLinkedQueue[(Symbol.DefnSym, Symbol.VarSym)], simplifiedIfThenElse: ConcurrentLinkedQueue[(Symbol.DefnSym, Int)], eliminatedStms: ConcurrentLinkedQueue[(Symbol.DefnSym, Int)])

  private object SharedContext {

    /**
      * Returns a fresh shared context.
      */
    def mk(): SharedContext = SharedContext(new ConcurrentLinkedQueue(), new ConcurrentLinkedQueue(), new ConcurrentLinkedQueue(), new ConcurrentLinkedQueue(), new ConcurrentLinkedQueue(), new ConcurrentLinkedQueue())

    def toStats(sctx: SharedContext): Optimizer1.Stats = {
      val inlinedDefs = ListMap.from(sctx.inlinedDefs.asScala)
      val inlinedVars = ListMap.from(sctx.inlinedVars.asScala)
      val betaReductions = Optimizer1.Stats.toCount(sctx.betaReductions.asScala)
      val eliminatedVars = ListMap.from(sctx.eliminatedVars.asScala)
      val simplifiedIfThenElse = Optimizer1.Stats.toCount(sctx.simplifiedIfThenElse.asScala)
      val eliminatedStms = Optimizer1.Stats.toCount(sctx.eliminatedStms.asScala)
      Optimizer1.Stats(inlinedDefs, inlinedVars, betaReductions, eliminatedVars, simplifiedIfThenElse, eliminatedStms)
    }

  }
}
