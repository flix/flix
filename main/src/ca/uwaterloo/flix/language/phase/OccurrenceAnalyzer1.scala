/*
 * Copyright 2022 Anna Krogh, Patrick Lundvig, Christian Bonde
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
import ca.uwaterloo.flix.language.ast.OccurrenceAst.Occur.*
import ca.uwaterloo.flix.language.ast.OccurrenceAst.{DefContext, Expr, Linearity, Occur, Pattern}
import ca.uwaterloo.flix.language.ast.Symbol.{DefnSym, VarSym}
import ca.uwaterloo.flix.language.ast.{AtomicOp, OccurrenceAst, Symbol}
import ca.uwaterloo.flix.util.ParOps

/**
  * The occurrence analyzer collects information on variable and function usage and calculates the weight of the expressions
  * Marks a variable or function as Dead if it is not used, Once if it is used exactly once and Many otherwise
  */
object OccurrenceAnalyzer1 {

  object OccurInfo {

    /**
      * Occurrence information for an empty sequence of expressions
      */
    val Empty: OccurInfo = OccurInfo(Map.empty, Map.empty, 0, 0)

    /**
      * The initial occurrence information for an expression of size 1, i.e. an expression without subexpressions.
      */
    val One: OccurInfo = OccurInfo(Map.empty, Map.empty, 0, 1)
  }

  /**
    * The occurrence of `defs` and `vars` inside the body of a `def`
    * `size` represents the number of expressions in the body of a `def`
    */
  case class OccurInfo(defs: Map[DefnSym, Occur], vars: Map[VarSym, Occur], localDefs: Int, size: Int) {
    def :+(kv: (DefnSym, Occur)): OccurInfo = {
      this.copy(defs = this.defs + kv)
    }

    def +(kv: (VarSym, Occur)): OccurInfo = {
      this.copy(vars = this.vars + kv)
    }

    def -(varSym: VarSym): OccurInfo = {
      this.copy(vars = this.vars - varSym)
    }

    def --(varSyms: Iterable[VarSym]): OccurInfo = {
      this.copy(vars = this.vars -- varSyms)
    }

    def get(defnSym: DefnSym): Occur = {
      this.defs.getOrElse(defnSym, Dead)
    }

    def get(varSym: VarSym): Occur = {
      this.vars.getOrElse(varSym, Dead)
    }

    def incrementLocalDefCount: OccurInfo = {
      this.copy(localDefs = localDefs + 1)
    }
  }

  private def increment(occurInfo: OccurInfo): OccurInfo = {
    occurInfo.copy(size = occurInfo.size + 1)
  }

  /**
    * A set of functions that contain masked casts.
    *
    * Must be manually maintained since Lowering erases the masked cast.
    */
  private val DangerousFunctions: Set[String] = Set("bug!", "Fixpoint.Debugging.notifyPreSolve", "Fixpoint.Debugging.notifyPostSolve", "Fixpoint.Debugging.notifyPreInterpret", "Assert.eq")

  private def toReadableFunction(sym: Symbol.DefnSym): String = {
    sym.toString.takeWhile(c => c.toString != Flix.Delimiter)
  }

  /**
    * Performs occurrence analysis on the given AST `root`.
    */
  def run(root: OccurrenceAst.Root)(implicit flix: Flix): OccurrenceAst.Root = {
    val defs = visitDefs(root.defs) // visitDefs calls parMap internally and has additional handling
    OccurrenceAst.Root(defs, root.enums, root.structs, root.effects, root.mainEntryPoint, root.entryPoints, root.sources)
  }

  /**
    * Performs occurrence analysis on every entry in `defs0` in parallel.
    * Decorates each Def with occurrence information, i.e., how it appears in the program or if it is unused.
    */
  private def visitDefs(defs0: Map[DefnSym, OccurrenceAst.Def])(implicit flix: Flix): Map[DefnSym, OccurrenceAst.Def] = {
    val (ds, os) = ParOps.parMap(defs0.values)(visitDef).unzip

    // Combine all `defOccurrences` into one map.
    val defOccur = combineAll(os)

    // Updates the occurrence of every `def` in `ds` based on the occurrence found in `defOccur`.
    ds.foldLeft(Map.empty[DefnSym, OccurrenceAst.Def]) {
      case (macc, defn) =>
        val occur = if (DangerousFunctions.contains(toReadableFunction(defn.sym))) Dangerous else defOccur.getOrElse(defn.sym, Dead)
        val newContext = defn.context.copy(occur = occur)
        val defWithContext = defn.copy(context = newContext)
        macc + (defn.sym -> defWithContext)
    }
  }

  /**
    * Performs occurrence analysis on `defn`.
    */
  private def visitDef(defn0: OccurrenceAst.Def): (OccurrenceAst.Def, OccurInfo) = {

    /**
      * Returns true if `expr0` is a function call.
      */
    def isDirectCall(expr0: OccurrenceAst.Expr): Boolean = expr0 match {
      case OccurrenceAst.Expr.ApplyDef(_, _, _, _, _, _) => true
      case _ => false
    }

    /**
      * Returns true if `def0` occurs in `occurInfo`.
      */
    def isSelfRecursive(occurInfo: OccurInfo): Boolean = occurInfo.defs.get(defn0.sym) match {
      case None => false
      case Some(o) => o match {
        case Occur.Dead => false
        case Occur.Once => true
        case Occur.OnceInLambda => true
        case Occur.OnceInLocalDef => true
        case Occur.Many => true
        case Occur.ManyBranch => true
        case Occur.DontInline => false
        case Occur.Dangerous => true
      }
    }

    val (exp, occurInfo) = visitExp(defn0.exp)(defn0.sym)
    val defContext = DefContext(isDirectCall(exp), occurInfo.get(defn0.sym), occurInfo.size, occurInfo.localDefs, isSelfRecursive(occurInfo))
    val fparams = defn0.fparams.map { case (p, _) => p -> occurInfo.get(p.sym) }
    (OccurrenceAst.Def(defn0.sym, fparams, defn0.spec, exp, defContext, defn0.loc), occurInfo)
  }

  /**
    * Performs occurrence analysis on `exp00`
    */
  private def visitExp(exp0: OccurrenceAst.Expr)(implicit sym0: Symbol.DefnSym): (OccurrenceAst.Expr, OccurInfo) = exp0 match {
    case Expr.Cst(cst, tpe, loc) =>
      (OccurrenceAst.Expr.Cst(cst, tpe, loc), OccurInfo.One)

    case Expr.Var(sym, tpe, loc) =>
      (OccurrenceAst.Expr.Var(sym, tpe, loc), OccurInfo.One + (sym -> Once))

    case Expr.Lambda((fp, _), exp, tpe, loc) =>
      val (e, o) = visitExp(exp)
      val o1 = captureVarsInLambda(o)
      val occur = o1.get(fp.sym)
      val o2 = o1 - fp.sym
      (OccurrenceAst.Expr.Lambda((fp, occur), e, tpe, loc), increment(o2))

    case Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
      val (es, o) = visitExps(exps)
      val o1 = visitAtomicOp(op, o)
      (OccurrenceAst.Expr.ApplyAtomic(op, es, tpe, eff, loc), increment(o1))

    case Expr.ApplyClo(exp1, exp2, tpe, eff, loc) =>
      val (e1, o1) = visitExp(exp1)
      val (e2, o2) = visitExp(exp2)
      val o3 = combineInfo(o1, o2)
      (OccurrenceAst.Expr.ApplyClo(e1, e2, tpe, eff, loc), increment(o3))

    case Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
      val (es, o1) = visitExps(exps)
      val o2 = o1 :+ sym -> Once
      (OccurrenceAst.Expr.ApplyDef(sym, es, itpe, tpe, eff, loc), increment(o2))

    case Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
      val (es, o1) = visitExps(exps)
      val o2 = o1 + (sym -> Once)
      (OccurrenceAst.Expr.ApplyLocalDef(sym, es, tpe, eff, loc), increment(o2))

    case Expr.Let(sym, exp1, exp2, tpe, eff, _, loc) =>
      val (e1, o1) = visitExp(exp1)
      val (e2, o2) = visitExp(exp2)
      val o3 = combineInfo(o1, o2)
      val occur = o3.get(sym)
      val o4 = o3 - sym
      (OccurrenceAst.Expr.Let(sym, e1, e2, tpe, eff, occur, loc), increment(o4))

    case Expr.LocalDef(sym, fps, exp1, exp2, tpe, eff, _, loc) =>
      val (e1, o10) = visitExp(exp1)
      val o1 = captureVarsInLocalDef(o10)
      val (e2, o2) = visitExp(exp2)
      val o3 = combineInfo(o1, o2)
      val occur = o3.get(sym)
      val o4 = o3 - sym
      val o5 = o4.incrementLocalDefCount
      (OccurrenceAst.Expr.LocalDef(sym, fps, e1, e2, tpe, eff, occur, loc), increment(o5))

    case Expr.Scope(sym, rsym, exp, tpe, eff, loc) =>
      val (e, o1) = visitExp(exp)
      (OccurrenceAst.Expr.Scope(sym, rsym, e, tpe, eff, loc), increment(o1))

    case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val (e1, o1) = visitExp(exp1)
      val (e2, o2) = visitExp(exp2)
      val (e3, o3) = visitExp(exp3)
      val o4 = combineInfo(o1, combineInfoBranch(o2, o3))
      (OccurrenceAst.Expr.IfThenElse(e1, e2, e3, tpe, eff, loc), increment(o4))

    case Expr.Stm(exp1, exp2, tpe, eff, loc) =>
      val (e1, o1) = visitExp(exp1)
      val (e2, o2) = visitExp(exp2)
      val o3 = combineInfo(o1, o2)
      (OccurrenceAst.Expr.Stm(e1, e2, tpe, eff, loc), increment(o3))

    case Expr.Discard(exp, eff, loc) =>
      val (e, o) = visitExp(exp)
      (OccurrenceAst.Expr.Discard(e, eff, loc), increment(o))

    case Expr.Match(exp, rules, tpe, eff, loc) =>
      val (e, o1) = visitExp(exp)
      val (rs, o2) = visitMatchRules(rules)
      val o3 = combineInfo(o1, o2)
      (OccurrenceAst.Expr.Match(e, rs, tpe, eff, loc), increment(o3))

    case Expr.VectorLit(exps, tpe, eff, loc) =>
      val (es, o) = visitExps(exps)
      (OccurrenceAst.Expr.VectorLit(es, tpe, eff, loc), increment(o))

    case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      val (e1, o1) = visitExp(exp1)
      val (e2, o2) = visitExp(exp2)
      val o3 = combineInfo(o1, o2)
      (OccurrenceAst.Expr.VectorLoad(e1, e2, tpe, eff, loc), increment(o3))

    case Expr.VectorLength(exp, loc) =>
      val (e, o) = visitExp(exp)
      (OccurrenceAst.Expr.VectorLength(e, loc), increment(o))

    case Expr.Ascribe(exp, tpe, eff, loc) =>
      val (e, o) = visitExp(exp)
      (OccurrenceAst.Expr.Ascribe(e, tpe, eff, loc), increment(o))

    case Expr.Cast(exp, declaredType, declaredEff, tpe, eff, loc) =>
      val (e, o) = visitExp(exp)
      val o1 = if (declaredEff.isDefined) o :+ sym0 -> Dangerous else o
      (OccurrenceAst.Expr.Cast(e, declaredType, declaredEff, tpe, eff, loc), increment(o1))

    case Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val (e, o1) = visitExp(exp)
      val (rs, o2) = visitTryCatchRules(rules)
      // Nesting try-catch breaks the backend so don't inline
      val o3 = combineInfoBranch(o1, o2) :+ sym0 -> DontInline
      (OccurrenceAst.Expr.TryCatch(e, rs, tpe, eff, loc), increment(o3))

    case Expr.RunWith(exp, effUse, rules, tpe, eff, loc) =>
      val (e, o1) = visitExp(exp)
      val (rs, o2) = visitTryWithRules(rules)
      val o3 = combineInfo(o1, o2) :+ sym0 -> DontInline
      (OccurrenceAst.Expr.RunWith(e, effUse, rs, tpe, eff, loc), increment(o3))

    case Expr.Do(op, exps, tpe, eff, loc) =>
      val (es, o) = visitExps(exps)
      (OccurrenceAst.Expr.Do(op, es, tpe, eff, loc), increment(o))

    case Expr.NewObject(name, clazz, tpe, eff, methods, loc) =>
      val (ms, o) = visitJvmMethods(methods)
      (OccurrenceAst.Expr.NewObject(name, clazz, tpe, eff, ms, loc), increment(o))

  }

  /**
    * Performs occurrence analysis on a list of expressions `exps` and merges occurrences.
    * Captures `sym0`.
    */
  private def visitExps(exps: List[Expr])(implicit sym0: Symbol.DefnSym): (List[OccurrenceAst.Expr], OccurInfo) = {
    val (es, o1) = exps.map(visitExp).unzip
    val o2 = o1.foldLeft(OccurInfo.Empty)(combineInfo)
    (es, o2)
  }

  private def visitAtomicOp(op0: AtomicOp, occurInfo0: OccurInfo)(implicit sym0: Symbol.DefnSym): OccurInfo = op0 match {
    case AtomicOp.Is(sym) if sym.name == "Choice" =>
      occurInfo0 :+ sym0 -> DontInline

    case AtomicOp.Cast => occurInfo0 :+ sym0 -> Dangerous

    case _ => occurInfo0
  }

  private def visitMatchRules(rules0: List[OccurrenceAst.MatchRule])(implicit sym0: Symbol.DefnSym): (List[OccurrenceAst.MatchRule], OccurInfo) = {
    val (rs, o) = rules0.map {
      case OccurrenceAst.MatchRule(pat, guard, exp) =>
        val (g, o1) = guard.map(visitExp).unzip
        val (e, o2) = visitExp(exp)
        val o3 = combineInfoOpt(o1, o2)
        val (p, syms) = visitPattern(pat)(o3)
        val o4 = o3 -- syms
        (OccurrenceAst.MatchRule(p, g, e), o4)
    }.unzip
    val o1 = o.foldLeft(OccurInfo.Empty)(combineInfoBranch)
    (rs, o1)
  }

  private def visitTryCatchRules(rules0: List[OccurrenceAst.CatchRule])(implicit sym0: Symbol.DefnSym): (List[OccurrenceAst.CatchRule], OccurInfo) = {
    val (rs, o) = rules0.map {
      case OccurrenceAst.CatchRule(sym, clazz, exp) =>
        val (e, o) = visitExp(exp)
        (OccurrenceAst.CatchRule(sym, clazz, e), o)
    }.unzip
    val o1 = o.foldLeft(OccurInfo.Empty)(combineInfo)
    (rs, o1)
  }

  private def visitTryWithRules(rules0: List[OccurrenceAst.HandlerRule])(implicit sym0: Symbol.DefnSym): (List[OccurrenceAst.HandlerRule], OccurInfo) = {
    val (rs, o) = rules0.map {
      case OccurrenceAst.HandlerRule(op, fps, exp, _) =>
        val (e, o) = visitExp(exp)
        val continuation = fps.last
        val occurrence = o.get(continuation.sym)
        occurrence match {
          case Dead => (OccurrenceAst.HandlerRule(op, fps, e, Linearity.Dead), o)
          case Once => (OccurrenceAst.HandlerRule(op, fps, e, Linearity.Once), o)
          case OnceInLocalDef
               | OnceInLambda
               | Many
               | Dangerous
               | DontInline
               | ManyBranch => (OccurrenceAst.HandlerRule(op, fps, e, Linearity.Many), o)
        }
    }.unzip
    val o1 = o.foldLeft(OccurInfo.Empty)(combineInfo)
    (rs, o1)
  }

  private def visitJvmMethods(methods0: List[OccurrenceAst.JvmMethod])(implicit sym0: Symbol.DefnSym): (List[OccurrenceAst.JvmMethod], OccurInfo) = {
    val (ms, o) = methods0.map {
      case OccurrenceAst.JvmMethod(ident, fparams, exp, retTpe, eff, loc) =>
        val (c, o) = visitExp(exp)
        (OccurrenceAst.JvmMethod(ident, fparams, c, retTpe, eff, loc), increment(o))
    }.unzip
    val o1 = o.foldLeft(OccurInfo.Empty)(combineInfoBranch)
    (ms, o1)
  }

  private def visitPattern(pattern0: Pattern)(implicit occurInfo: OccurInfo): (Pattern, Set[VarSym]) = pattern0 match {
    case Pattern.Wild(tpe, loc) =>
      (Pattern.Wild(tpe, loc), Set.empty)

    case Pattern.Var(sym, tpe, _, loc) =>
      (Pattern.Var(sym, tpe, occurInfo.get(sym), loc), Set(sym))

    case Pattern.Cst(cst, tpe, loc) =>
      (Pattern.Cst(cst, tpe, loc), Set.empty)

    case Pattern.Tag(sym, pats, tpe, loc) =>
      val (ps, listOfSyms) = pats.map(visitPattern).unzip
      val syms = Set.from(listOfSyms.flatten)
      (Pattern.Tag(sym, ps, tpe, loc), syms)

    case Pattern.Tuple(pats, tpe, loc) =>
      val (ps, listOfSyms) = pats.map(visitPattern).unzip
      val syms = Set.from(listOfSyms.flatten)
      (Pattern.Tuple(ps, tpe, loc), syms)

    case Pattern.Record(pats, pat, tpe, loc) =>
      val (ps, listOfSyms) = pats.map(visitRecordLabelPattern).unzip
      val (p, syms0) = visitPattern(pat)
      val syms = Set.from(listOfSyms.flatten) ++ syms0
      (Pattern.Record(ps, p, tpe, loc), syms)

  }

  private def visitRecordLabelPattern(pattern0: Pattern.Record.RecordLabelPattern)(implicit occurInfo: OccurInfo): (Pattern.Record.RecordLabelPattern, Set[VarSym]) = pattern0 match {
    case Pattern.Record.RecordLabelPattern(label, pat, tpe, loc) =>
      val (p, syms) = visitPattern(pat)
      (Pattern.Record.RecordLabelPattern(label, p, tpe, loc), syms)
  }

  /**
    * Combines objects `o1` and `o2` of the type OccurInfo into a single OccurInfo object.
    */
  private def combineInfoBranch(o1: OccurInfo, o2: OccurInfo): OccurInfo = {
    combineAll(o1, o2, combineBranch)
  }

  /**
    * Combines objects `o1` and `o2` of the type OccurInfo into a single OccurInfo object.
    */
  private def combineInfo(o1: OccurInfo, o2: OccurInfo): OccurInfo = {
    combineAll(o1, o2, combine)
  }

  /**
    * Combines objects `o1` and `o2` of the type OccurInfo into a single OccurInfo object.
    */
  private def combineInfoOpt(o1: Option[OccurInfo], o2: OccurInfo): OccurInfo = {
    o1.map(combineInfo(_, o2)).getOrElse(o2)
  }


  /**
    * Combines objects `o1` and `o2` of the type OccurInfo into a single OccurInfo object.
    */
  private def combineAll(o1: OccurInfo, o2: OccurInfo, combine: (Occur, Occur) => Occur): OccurInfo = {
    val varMap = combineMaps(o1.vars, o2.vars, combine)
    val defMap = combineMaps(o1.defs, o2.defs, combine)
    val localDefs = o1.localDefs + o2.localDefs
    val size = o1.size + o2.size
    OccurInfo(defMap, varMap, localDefs, size)
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
    * Combines all [[OccurInfo]] in `os` and maps each [[DefnSym]] to its corresponding [[OccurInfo]].
    */
  private def combineAll(os: Iterable[OccurInfo]): Map[DefnSym, Occur] = {
    os.foldLeft(Map.empty[DefnSym, Occur])((acc, o) => combineMaps(acc, o.defs, combine))
  }

  /**
    * Combines two occurrences `o1` and `o2` of type Occur into a single occurrence.
    */
  private def combine(o1: Occur, o2: Occur): Occur = (o1, o2) match {
    case (Dangerous, _) => Dangerous
    case (_, Dangerous) => Dangerous
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
    case (Dangerous, _) => Dangerous
    case (_, Dangerous) => Dangerous
    case (DontInline, _) => DontInline
    case (_, DontInline) => DontInline
    case (Dead, _) => o2
    case (_, Dead) => o1
    case _ => ManyBranch
  }

  // TODO: Add doc
  private def captureVarsInLambda(occurInfo: OccurInfo): OccurInfo = {
    update(occurInfo) {
      case Once => OnceInLambda
    }
  }

  // TODO: Add doc
  private def captureVarsInLocalDef(occurInfo: OccurInfo): OccurInfo = {
    update(occurInfo) {
      case Once => OnceInLocalDef
    }
  }

  // TODO: Add doc
  private def update(occurInfo: OccurInfo)(f: PartialFunction[Occur, Occur]): OccurInfo = {
    val defs = occurInfo.defs.map {
      case (k, v) =>
        if (f.isDefinedAt(v))
          (k, f(v))
        else
          (k, v)
    }
    val vars = occurInfo.vars.map {
      case (k, v) =>
        if (f.isDefinedAt(v))
          (k, f(v))
        else
          (k, v)
    }
    occurInfo.copy(defs = defs, vars = vars)
  }
}


