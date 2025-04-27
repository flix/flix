/*
 * Copyright 2018 Magnus Madsen, Anna Krogh, Patrick Lundvig, Christian Bonde
 * 2025 Jakob Schneider Villumsen
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
import ca.uwaterloo.flix.language.ast.OccurrenceAst.Occur.*
import ca.uwaterloo.flix.language.ast.OccurrenceAst.{DefContext, Expr, Occur}
import ca.uwaterloo.flix.language.ast.{AtomicOp, OccurrenceAst, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.collection.CofiniteSet
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

import java.util.concurrent.ConcurrentLinkedQueue

/**
  * The inliner optionally performs beta-reduction at call-sites.
  */
object Inliner {

  /**
    * Performs inlining on the given AST `root`.
    */
  def run(root: OccurrenceAst.Root)(implicit flix: Flix): OccurrenceAst.Root = {
    implicit val sctx: SharedContext = SharedContext.mk()
    val defs = ParOps.parMapValues(root.defs)(visitDef(_)(root, sctx, flix))
    val newRoot = OccurrenceAst.Root(defs, root.enums, root.structs, root.effects, root.mainEntryPoint, root.entryPoints, root.sources)
    newRoot
  }

  /**
    * Performs expression inlining on the given definition `def0`.
    * Converts definition from [[OccurrenceAst]] to [[OccurrenceAst]].
    */
  private def visitDef(def0: OccurrenceAst.Def)(implicit root: OccurrenceAst.Root, sctx: SharedContext, flix: Flix): OccurrenceAst.Def = def0 match {
    case OccurrenceAst.Def(sym, fparams, spec, exp, ctx, loc) =>
      val e = visitExp(exp, Context.Empty)(sym, root, sctx, flix)
      OccurrenceAst.Def(sym, fparams, spec, e, ctx, loc)
  }

  /**
    * Performs inlining operations on the expression `exp0` from [[Expr]].
    * Returns a [[Expr]]
    */
  private def visitExp(exp0: Expr, ctx0: Context)(implicit sym0: Symbol.DefnSym, root: OccurrenceAst.Root, sctx: SharedContext, flix: Flix): Expr = exp0

  /**
    * Returns `true` if `def0` should be inlined.
    */
  private def canInlineDef(defCtx: DefContext, ctx: Context): Boolean = {
    val mayInline = !defCtx.isSelfRecursive && !ctx.currentlyInlining
    val shouldInline = defCtx.isDirectCall
    mayInline && shouldInline
  }

  private def isPure(eff0: Type): Boolean = {
    eff0 == Type.Pure
  }

  /**
    * Checks if `occur` is Dead.
    */
  private def isDead(occur: OccurrenceAst.Occur): Boolean = occur match {
    case Dead => true
    case _ => false
  }

  /**
    * Checks if `occur` is Once and `eff` is Pure
    */
  private def isUsedOnceAndPure(occur: OccurrenceAst.Occur, eff0: Type): Boolean = occur match {
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
  private def isDeadAndPure(occur: OccurrenceAst.Occur, eff0: Type): Boolean = occur match {
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
    case Expr.ApplyAtomic(AtomicOp.Unary(_), exps, _, _, _) => exps.forall(isTrivialExp) // TODO: Do constant folding?
    case Expr.ApplyAtomic(AtomicOp.Binary(_), exps, _, _, _) => exps.forall(isTrivialExp) // TODO: Do constant folding?
    case Expr.ApplyAtomic(AtomicOp.Tag(_), exps, _, _, _) => exps.forall(isTrivialExp) // TODO: Ensure fully applied
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
  }


  private type InVar = Symbol.VarSym

  private type OutVar = Symbol.VarSym

  private type InExpr = OccurrenceAst.Expr

  private type OutExpr = OccurrenceAst.Expr

  sealed private trait SubstRange

  private object SubstRange {

    case class SuspendedExpr(exp: InExpr) extends SubstRange

    case class DoneExpr(exp: OutExpr) extends SubstRange

  }

  private sealed trait Definition

  private object Definition {

    case class LetBound(expr: OutExpr, occur: Occur) extends Definition

  }

  private type VarSubst = Map[InVar, OutVar]

  private type Subst = Map[InVar, SubstRange]

  private type InScopeVars = Map[OutVar, Definition]


  private case class Context(varSubst: VarSubst, subst: Subst, inScopeVars: InScopeVars, currentlyInlining: Boolean)

  private object Context {
    val Empty: Context = Context(Map.empty, Map.empty, Map.empty, currentlyInlining = false)
  }

}
