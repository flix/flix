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
  private def visitExp(exp0: OccurrenceAst.Expr)(implicit sym0: Symbol.DefnSym, lctx: LocalContext): (OccurrenceAst.Expr, ExpContext) = (exp0, ExpContext.empty)

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
