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
import ca.uwaterloo.flix.language.ast.OccurrenceAst.Occur.{Dead, Once, OnceInLambda, OnceInLocalDef, ManyBranch, Many}
import ca.uwaterloo.flix.language.ast.OccurrenceAst.{Expr, Occur}
import ca.uwaterloo.flix.language.ast.{AtomicOp, OccurrenceAst, Symbol, Type}
import ca.uwaterloo.flix.util.ParOps

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

  /** Performs inlining on the expression `exp0`. */
  private def visitExp(exp0: Expr, ctx0: LocalContext)(implicit sym0: Symbol.DefnSym, root: OccurrenceAst.Root, flix: Flix): Expr = exp0

  /** Returns `true` if `eff0` is pure. */
  private def isPure(eff0: Type): Boolean = {
    eff0 == Type.Pure
  }

  /** Checks if `occur` is [[Dead]]. */
  private def isDead(occur: OccurrenceAst.Occur): Boolean = occur match {
    case Dead => true
    case Once => false
    case OnceInLambda => false
    case OnceInLocalDef => false
    case ManyBranch => false
    case Many => false
  }

  /** Checks if `occur` is [[Once]]. */
  private def isOnce(occur: OccurrenceAst.Occur): Boolean = occur match {
    case Dead => false
    case Once => true
    case OnceInLambda => false
    case OnceInLocalDef => false
    case ManyBranch => false
    case Many => false
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

    /** The right-hand side of a let-bound variable along with its occurrence information. */
    case class LetBound(expr: OccurrenceAst.Expr, occur: Occur) extends BoundKind

  }

  /** Denotes the level at which the binder is declared. */
  sealed trait Level

  private object Level {

    /** A [[Def]] declaration. The def can be in modules or top-level. */
    case object Def extends Level

    /** Nested inside a [[Def]]. This can be a lambda or local def. */
    case object Nested extends Level

  }

  /**
    * A wrapper class for all the different inlining environments.
    *
    * @param varSubst          a substitution on variables to variables.
    * @param subst             a substitution on variables to expressions.
    * @param inScopeVars       a set of variables considered to be in scope.
    * @param currentlyInlining a flag denoting whether the current traversal is part of an inline-expansion process.
    */
  private case class LocalContext(varSubst: Map[Symbol.VarSym, Symbol.VarSym], subst: Map[Symbol.VarSym, SubstRange], inScopeVars: Map[Symbol.VarSym, BoundKind], currentlyInlining: Boolean)

  private object LocalContext {

    /** Returns the empty context with `currentlyInlining` set to `false`. */
    val Empty: LocalContext = LocalContext(Map.empty, Map.empty, Map.empty, currentlyInlining = false)

  }
}
