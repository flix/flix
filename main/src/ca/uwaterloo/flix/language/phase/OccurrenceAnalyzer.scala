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

/**
  * The occurrence analyzer collects occurrence information on binders according to the definition of [[Occur]].
  * Additionally, it also counts the number of subexpressions in a function to compute its size.
  */
object OccurrenceAnalyzer {

  /**
    * Performs occurrence analysis on the given AST `root`.
    */
  def run(root: OccurrenceAst.Root)(implicit flix: Flix): OccurrenceAst.Root = {
    val (ds, os) = ParOps.parMap(root.defs.values)(visitDef).unzip

    // Combine all `defOccurrences` into one map.
    val defOccur = combineAll(os)

    // Updates the occurrence of every `def` in `ds` based on the occurrence found in `defOccur`.
    val defs = ds.foldLeft(Map.empty[DefnSym, OccurrenceAst.Def]) {
      case (macc, defn) =>
        val occur = if (DangerousFunctions.contains(stripDelimiter(defn.sym))) DontInlineAndDontRewrite else defOccur.getOrElse(defn.sym, Dead)
        val newContext = defn.context.copy(occur = occur)
        val defWithContext = defn.copy(context = newContext)
        macc + (defn.sym -> defWithContext)
    }
    OccurrenceAst.Root(defs, root.enums, root.structs, root.effects, root.mainEntryPoint, root.entryPoints, root.sources)
  }

  /**
    * Performs occurrence analysis on `defn`.
    */
  private def visitDef(defn0: OccurrenceAst.Def): (OccurrenceAst.Def, ExpContext) = {
    val (exp, occurInfo) = visitExp(defn0.exp)(defn0.sym)
    val defContext = DefContext(occurInfo.get(defn0.sym), occurInfo.size, occurInfo.localDefs, isDirectCall(exp), isSelfRecursive(occurInfo))
    val fparams = defn0.fparams.map(fp => fp.copy(occur = occurInfo.get(fp.sym)))
    (OccurrenceAst.Def(defn0.sym, fparams, defn0.spec, exp, defContext, defn0.loc), occurInfo)
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
    * Returns true if `def0` occurs in `occurInfo`.
    */
  private def isSelfRecursive(occurInfo: ExpContext): Boolean = occurInfo.defs.get(defn0.sym) match {
    case None => false
    case Some(o) => o match {
      case Occur.Dead => false
      case Occur.Once => true
      case Occur.OnceInLambda => true
      case Occur.OnceInLocalDef => true
      case Occur.Many => true
      case Occur.ManyBranch => true
      case Occur.DontInline => false
      case Occur.DontInlineAndDontRewrite => true
    }
  }

  /**
    * Performs occurrence analysis on `exp00`
    */
  private def visitExp(exp0: OccurrenceAst.Expr)(implicit sym0: Symbol.DefnSym): (OccurrenceAst.Expr, ExpContext) = (exp0, ExpContext.empty)

  /**
    * Combines objects `o1` and `o2` of the type OccurInfo into a single OccurInfo object.
    */
  private def combineInfoBranch(o1: ExpContext, o2: ExpContext): ExpContext = {
    combineAll(o1, o2, combineBranch)
  }

  /**
    * Combines objects `o1` and `o2` of the type OccurInfo into a single OccurInfo object.
    */
  private def combineInfo(o1: ExpContext, o2: ExpContext): ExpContext = {
    combineAll(o1, o2, combine)
  }

  /**
    * Combines objects `o1` and `o2` of the type OccurInfo into a single OccurInfo object.
    */
  private def combineInfoOpt(o1: Option[ExpContext], o2: ExpContext): ExpContext = {
    o1.map(combineInfo(_, o2)).getOrElse(o2)
  }

  /**
    * Combines objects `o1` and `o2` of the type OccurInfo into a single OccurInfo object.
    */
  private def combineAll(o1: ExpContext, o2: ExpContext, combine: (Occur, Occur) => Occur): ExpContext = {
    val varMap = combineMaps(o1.vars, o2.vars, combine)
    val defMap = combineMaps(o1.defs, o2.defs, combine)
    val localDefs = o1.localDefs + o2.localDefs
    val size = o1.size + o2.size
    ExpContext(defMap, varMap, localDefs, size)
  }

  /**
    * Combines maps `m1` and `m2` of the type (A -> Occur) into a single map of the same type.
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
    * Combines all [[ExpContext]] in `os` and maps each [[DefnSym]] to its corresponding [[ExpContext]].
    */
  private def combineAll(os: Iterable[ExpContext]): Map[DefnSym, Occur] = {
    os.foldLeft(Map.empty[DefnSym, Occur])((acc, o) => combineMaps(acc, o.defs, combine))
  }

  /**
    * Combines two occurrences `o1` and `o2` of type Occur into a single occurrence.
    */
  private def combine(o1: Occur, o2: Occur): Occur = (o1, o2) match {
    case (DontInlineAndDontRewrite, _) => DontInlineAndDontRewrite
    case (_, DontInlineAndDontRewrite) => DontInlineAndDontRewrite
    case (DontInline, _) => DontInline
    case (_, DontInline) => DontInline
    case (Dead, _) => o2
    case (_, Dead) => o1
    case _ => Many
  }

  /**
    * Combines two occurrences `o1` and `o2` of type Occur into a single occurrence based on ManyBranches logic.
    * ManyBranches can be
    * - [[OccurrenceAst.Expr.IfThenElse]]
    * - [[OccurrenceAst.Expr.Match]]
    */
  private def combineBranch(o1: Occur, o2: Occur): Occur = (o1, o2) match {
    case (DontInlineAndDontRewrite, _) => DontInlineAndDontRewrite
    case (_, DontInlineAndDontRewrite) => DontInlineAndDontRewrite
    case (DontInline, _) => DontInline
    case (_, DontInline) => DontInline
    case (Dead, _) => o2
    case (_, Dead) => o1
    case _ => ManyBranch
  }

  private object ExpContext {

    /** Context for an empty sequence of expressions. */
    def empty: ExpContext = ExpContext(Map.empty, Map.empty, 0, 0)

    /** Context for a single expression. */
    def one: ExpContext = ExpContext(Map.empty, Map.empty, 0, 1)

  }

  /**
    * Stores various pieces of information extracted from an expression.
    *
    * @param defs      A map from function symbols to occurrence information.
    *                  If the map does not contain a certain symbol, then symbol is [[Dead]].
    * @param vars      A map from variable symbols to occurrence information (this also includes uses of [[OccurrenceAst.Expr.LocalDef]]).
    *                  If the map does not contain a certain symbol, then symbol is [[Dead]].
    * @param localDefs the number of declared [[OccurrenceAst.Expr.LocalDef]]s in the expression.
    * @param size      The total number of subexpressions (including the expression itself).
    */
  case class ExpContext(defs: Map[DefnSym, Occur], vars: Map[VarSym, Occur], localDefs: Int, size: Int) {

    /**
      * Returns the occurrence information collected on `sym`. If [[defs]] does not contain `sym`, then it is [[Dead]].
      */
    def get(sym: DefnSym): Occur = {
      this.defs.getOrElse(sym, Dead)
    }

    /**
      * Returns the occurrence information collected on `sym`. If [[vars]] does not contain `sym`, then it is [[Dead]].
      */
    def get(sym: VarSym): Occur = {
      this.vars.getOrElse(sym, Dead)
    }

    /**
      * Returns a new [[ExpContext]] with the key-value pair `kv` added to [[defs]].
      */
    def :+(kv: (DefnSym, Occur)): ExpContext = {
      this.copy(defs = this.defs + kv)
    }

    /**
      * Returns a new [[ExpContext]] with the key-value pair `kv` added to [[vars]].
      */
    def +(kv: (VarSym, Occur)): ExpContext = {
      this.copy(vars = this.vars + kv)
    }

    /**
      * Returns a new [[ExpContext]] with `sym` and the corresponding value removed from [[vars]].
      */
    def -(sym: VarSym): ExpContext = {
      this.copy(vars = this.vars - sym)
    }

    /**
      * Returns a new [[ExpContext]] with `syms` and the corresponding values removed from [[vars]].
      */
    def --(syms: Iterable[VarSym]): ExpContext = {
      this.copy(vars = this.vars -- syms)
    }

    /**
      * Returns a new [[ExpContext]] with [[localDefs]] incremented by one.
      */
    def incrementLocalDefs: ExpContext = {
      this.copy(localDefs = localDefs + 1)
    }

    /**
      * Returns a new [[ExpContext]] with [[size]] incremented by one.
      */
    def incrementSize: ExpContext = {
      this.copy(size = size + 1)
    }
  }

  /**
    * A set of functions that contain masked casts.
    *
    * Must be manually maintained since [[Lowering]] erases masked casts.
    */
  private val DangerousFunctions: Set[String] = Set("bug!", "Fixpoint.Debugging.notifyPreSolve", "Fixpoint.Debugging.notifyPostSolve", "Fixpoint.Debugging.notifyPreInterpret", "Assert.eq")

  /**
    * Returns a string with where [[Flix.Delimiter]] is stripped, so membership can be checked in [[DangerousFunctions]].
    */
  private def stripDelimiter(sym: Symbol.DefnSym): String = {
    sym.toString.takeWhile(c => c.toString != Flix.Delimiter)
  }

  /**
    * Returns `true` if `sym` is a dangerous function, i.e., it is a member of [[DangerousFunctions]].
    */
  private def isDangerousFunction(sym: Symbol.DefnSym): Boolean = {
    DangerousFunctions.contains(stripDelimiter(sym))
  }
}
