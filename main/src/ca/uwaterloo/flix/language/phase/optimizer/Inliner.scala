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
  * Rewrites the body of each def using, applying the following transformations:
  *   - Copy Propagation:
  * {{{
  *     let x = 1;
  *     f(x)
  * }}}
  *     becomes
  * {{{
  *     let x = 1;
  *    f(1)
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
    implicit val sctx: SharedContext = SharedContext.mk()
    val defs = ParOps.parMapValues(root.defs)(visitDef(_)(root, sctx, flix))
    val newRoot = OccurrenceAst.Root(defs, root.enums, root.structs, root.effects, root.mainEntryPoint, root.entryPoints, root.sources)
    newRoot
  }

  /** Performs inlining on the body of `def0`. */
  private def visitDef(def0: OccurrenceAst.Def)(implicit root: OccurrenceAst.Root, sctx: SharedContext, flix: Flix): OccurrenceAst.Def = def0 match {
    case OccurrenceAst.Def(sym, fparams, spec, exp, ctx, loc) =>
      val e = visitExp(exp, Context.Empty)(sym, root, sctx, flix)
      OccurrenceAst.Def(sym, fparams, spec, e, ctx, loc)
  }

  /** Performs inlining operations on the expression `exp0` from [[Expr]]. */
  private def visitExp(exp0: Expr, ctx0: Context)(implicit sym0: Symbol.DefnSym, root: OccurrenceAst.Root, sctx: SharedContext, flix: Flix): Expr = exp0

  /** Returns `true` if `def0` should be inlined. */
  private def shouldInline(defCtx: DefContext, ctx: Context): Boolean = {
    val mayInline = !defCtx.isSelfRecursive && !ctx.currentlyInlining
    val shouldInline = defCtx.isDirectCall
    mayInline && shouldInline
  }

  /** Returns `true` if `eff0` is pure. */
  private def isPure(eff0: Type): Boolean = eff0 match {
    case Type.Cst(TypeConstructor.Pure, _) => true
    case Type.Cst(_, _) => false
    case Type.Apply(_, _, _) => false
    case Type.Var(sym, loc) => throw InternalCompilerException(s"unexpected type variable $sym", loc)
    case Type.Alias(_, _, _, loc) => throw InternalCompilerException("unexpected type 'alias'", loc)
    case Type.AssocType(_, _, _, loc) => throw InternalCompilerException("unexpected type 'assoc type'", loc)
    case Type.JvmToType(_, loc) => throw InternalCompilerException("unexpected type 'jvm to type'", loc)
    case Type.JvmToEff(_, loc) => throw InternalCompilerException("unexpected type 'jvm to eff'", loc)
    case Type.UnresolvedJvmType(_, loc) => throw InternalCompilerException("unexpected type 'unresolved jvm type'", loc)
  }

  /** Checks if `occur` is [[Dead]]. */
  private def isDead(occur: OccurrenceAst.Occur): Boolean = occur match {
    case Occur.Dead => true
    case Occur.Once => false
    case Occur.OnceInLambda => false
    case Occur.OnceInLocalDef => false
    case Occur.ManyBranch => false
    case Occur.Many => false
  }

  /** Checks if `occur` is [[Once]] and `eff` is pure. */
  private def isUsedOnceAndPure(occur: OccurrenceAst.Occur, eff0: Type): Boolean = occur match {
    case Once => isPure(eff0)
    case _ => false
  }

  /** Checks if `exp0` is trivial and `eff` is pure */
  private def isTrivialAndPure(exp0: Expr, eff0: Type): Boolean = {
    isPure(eff0) && isTrivialExp(exp0)
  }

  /** Checks if `occur` is dead and `exp` is pure. */
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
  private sealed trait Definition

  private object Definition {

    /** The right-hand side of a let-bound variable along with its occurrence information. */
    case class LetBound(expr: OccurrenceAst.Expr, occur: Occur) extends Definition

  }

  /**
    * A wrapper class for all the different inlining environments.
    *
    * @param varSubst          a substitution on variables to variables.
    * @param subst             a substitution on variables to expressions.
    * @param inScopeVars       a set of variables considered to be in scope.
    * @param currentlyInlining a flag denoting whether the current traversal is part of an inline-expansion process.
    */
  private case class Context(varSubst: Map[Symbol.VarSym, Symbol.VarSym], subst: Map[Symbol.VarSym, SubstRange], inScopeVars: Map[Symbol.VarSym, Definition], currentlyInlining: Boolean)

  private object Context {

    /** Returns the empty context with `currentlyInlining` set to `false`. */
    val Empty: Context = Context(Map.empty, Map.empty, Map.empty, currentlyInlining = false)

  }
}
