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
import ca.uwaterloo.flix.language.ast.OccurrenceAst1.Occur.*
import ca.uwaterloo.flix.language.ast.OccurrenceAst1.{DefContext, Occur}
import ca.uwaterloo.flix.language.ast.Symbol.{DefnSym, VarSym}
import ca.uwaterloo.flix.language.ast.{AtomicOp, OccurrenceAst1, Symbol}
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
    val Empty: OccurInfo = OccurInfo(Map.empty, Map.empty, 0)

    /**
      * The initial occurrence information for an expression of size 1, i.e. an expression without subexpressions.
      */
    val One: OccurInfo = OccurInfo(Map.empty, Map.empty, 1)
  }

  /**
    * The occurrence of `defs` and `vars` inside the body of a `def`
    * `size` represents the number of expressions in the body of a `def`
    */
  case class OccurInfo(defs: Map[DefnSym, Occur], vars: Map[VarSym, Occur], size: Int) {
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
  }

  private def increment(occurInfo: OccurInfo): OccurInfo = {
    occurInfo.copy(size = occurInfo.size + 1)
  }

  /**
    * Performs occurrence analysis on the given AST `root`.
    */
  def run(root: OccurrenceAst1.Root)(implicit flix: Flix): OccurrenceAst1.Root = {
    val defs = visitDefs(root.defs) // visitDefs call parMap internally and has additional handling
    OccurrenceAst1.Root(defs, root.enums, root.structs, root.effects, root.entryPoint, root.reachable, root.sources)
  }

  /**
    * Performs occurrence analysis on every entry in `defs0` in parallel.
    * Decorates each Def with occurrence information, i.e., how it appears in the program or if it is unused.
    */
  private def visitDefs(defs0: Map[DefnSym, OccurrenceAst1.Def])(implicit flix: Flix): Map[DefnSym, OccurrenceAst1.Def] = {
    val (ds, os) = ParOps.parMap(defs0.values)(visitDef).unzip

    // Combine all `defOccurrences` into one map.
    val defOccur = combineAll(os)

    // Updates the occurrence of every `def` in `ds` based on the occurrence found in `defOccur`.
    ds.foldLeft(Map.empty[DefnSym, OccurrenceAst1.Def]) {
      case (macc, defn) =>
        val occur = defOccur.getOrElse(defn.sym, Dead)
        val newContext = defn.context.copy(occur = occur)
        val defWithContext = defn.copy(context = newContext)
        macc + (defn.sym -> defWithContext)
    }
  }

  /**
    * Performs occurrence analysis on `defn`.
    */
  private def visitDef(defn0: OccurrenceAst1.Def): (OccurrenceAst1.Def, OccurInfo) = {

    /**
      * Returns true if `expr0` is a function call.
      */
    def isDirectCall(expr0: OccurrenceAst1.Expr): Boolean = expr0 match {
      case OccurrenceAst1.Expr.ApplyDef(_sym, _exps, _, _, _, _) => true
      // sym != defn0.sym // && exps.forall(isTrivial) // see doc comment for DefContext

      case OccurrenceAst1.Expr.ApplyClo(clo, _exps, _, _, _) =>
        clo match {
          case OccurrenceAst1.Expr.ApplyAtomic(AtomicOp.Closure(_sym), _, _, _, _) => true
          // sym != defn0.sym // && exps.forall(isTrivial) // see doc comment for DefContext
          case _ => false
        }
      case _ => false
    }

    /**
      * Returns true if `def0` occurs in `occurInfo` without being captured.
      */
    def isSelfRecursive(occurInfo: OccurInfo): Boolean = occurInfo.defs.get(defn0.sym) match {
      case None => false
      case Some(o) => o match {
        case Occur.Dead => false
        case Occur.Once => true
        case Occur.OnceInAbstraction => true
        case Occur.Many => true
        case Occur.ManyBranch => true
        case Occur.DontInline => false
      }
    }

    val (exp, occurInfo) = visitExp(defn0.sym, defn0.exp)
    val defContext = DefContext(isDirectCall(exp), occurInfo.get(defn0.sym), occurInfo.size, isSelfRecursive(occurInfo))
    val fparams = defn0.fparams.map { case (fp, _) => fp -> occurInfo.get(fp.sym) }
    (OccurrenceAst1.Def(defn0.sym, fparams, defn0.spec, exp, defContext, defn0.loc), occurInfo)
  }

  /**
    * Performs occurrence analysis on `exp00`
    */
  private def visitExp(sym0: Symbol.DefnSym, exp00: OccurrenceAst1.Expr): (OccurrenceAst1.Expr, OccurInfo) = {

    /**
      * Local visitor that captures `sym0` since it never changes.
      * The implicit parameter `letBinding` is `Some(sym)` if visiting the right-hand side
      * of a let-binding, i.e., in `let sym = exp` it recurses on `exp` with `letBinding = Some(sym)`.
      */
    def visit(exp0: OccurrenceAst1.Expr)(implicit letBinding: Option[VarSym]): (OccurrenceAst1.Expr, OccurInfo) = exp0 match {
      case OccurrenceAst1.Expr.Cst(cst, tpe, loc) =>
        (OccurrenceAst1.Expr.Cst(cst, tpe, loc), OccurInfo.One)

      case OccurrenceAst1.Expr.Var(sym, tpe, loc) =>
        (OccurrenceAst1.Expr.Var(sym, tpe, loc), OccurInfo.One + (sym -> Once))

      case OccurrenceAst1.Expr.Lambda((fparam, _), exp, tpe, loc) =>
        val (e, o) = visit(exp)
        val o1 = captureVars(o)
        val occur = o1.get(fparam.sym)
        val o2 = o1 - fparam.sym
        (OccurrenceAst1.Expr.Lambda((fparam, occur), e, tpe, loc), increment(o2))

      case OccurrenceAst1.Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
        val (es, o) = visitExps(exps)
        val o1 = visitAtomicOp(op, o)
        (OccurrenceAst1.Expr.ApplyAtomic(op, es, tpe, eff, loc), increment(o1))

      case OccurrenceAst1.Expr.ApplyClo(exp, exps, tpe, eff, loc) =>
        val (e, o1) = visit(exp)
        val (es, o2) = visitExps(exps)
        val o3 = combineInfo(o1, o2)
        val o4 = combineApplyCloInfo(o3, exp)
        (OccurrenceAst1.Expr.ApplyClo(e, es, tpe, eff, loc), increment(o4))

      case OccurrenceAst1.Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
        val (es, o1) = visitExps(exps)
        val o2 = o1 :+ sym -> Once
        (OccurrenceAst1.Expr.ApplyDef(sym, es, itpe, tpe, eff, loc), increment(o2))

      case OccurrenceAst1.Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
        val (es, o1) = visitExps(exps)
        val o2 = o1 + (sym -> Once)
        (OccurrenceAst1.Expr.ApplyLocalDef(sym, es, tpe, eff, loc), increment(o2))

      case OccurrenceAst1.Expr.Let(sym, exp1, exp2, tpe, eff, _, loc) =>
        val (e1, o1) = visit(exp1)(Some(sym))
        val (e2, o2) = visit(exp2)
        val o3 = combineInfo(o1, o2)
        val occur = o3.get(sym)
        val o4 = o3 - sym
        (OccurrenceAst1.Expr.Let(sym, e1, e2, tpe, eff, occur, loc), increment(o4))

      case OccurrenceAst1.Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, _, loc) =>
        val (e1, o10) = visit(exp1)
        val o1 = captureVars(o10)
        val (e2, o2) = visit(exp2)
        val o3 = combineInfo(o1, o2)
        val occur = o3.get(sym)
        val o4 = o3 - sym
        (OccurrenceAst1.Expr.LocalDef(sym, fparams, e1, e2, tpe, eff, occur, loc), increment(o4))

      case OccurrenceAst1.Expr.Scope(sym, rvar, exp, tpe, eff, loc) =>
        val (e, o1) = visit(exp)
        val o2 = o1 :+ sym0 -> DontInline
        (OccurrenceAst1.Expr.Scope(sym, rvar, e, tpe, eff, loc), increment(o2))

      case OccurrenceAst1.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
        val (e1, o1) = visit(exp1)
        val (e2, o2) = visit(exp2)
        val (e3, o3) = visit(exp3)
        val o4 = combineInfo(o1, combineInfoBranch(o2, o3))
        (OccurrenceAst1.Expr.IfThenElse(e1, e2, e3, tpe, eff, loc), increment(o4))

      case OccurrenceAst1.Expr.Stm(exp1, exp2, tpe, eff, loc) =>
        val (e1, o1) = visit(exp1)
        val (e2, o2) = visit(exp2)
        val o3 = combineInfo(o1, o2)
        (OccurrenceAst1.Expr.Stm(e1, e2, tpe, eff, loc), increment(o3))

      case OccurrenceAst1.Expr.Discard(exp, eff, loc) =>
        val (e, o) = visit(exp)
        (OccurrenceAst1.Expr.Discard(e, eff, loc), increment(o))

      case OccurrenceAst1.Expr.Match(exp, rules, tpe, eff, loc) =>
        val (e, o1) = visit(exp)
        val (rs, o2) = visitMatchRules(rules)
        val o3 = combineInfo(o1, o2)
        (OccurrenceAst1.Expr.Match(e, rs, tpe, eff, loc), increment(o3))

      case OccurrenceAst1.Expr.VectorLit(exps, tpe, eff, loc) =>
        val (es, o) = visitExps(exps)
        (OccurrenceAst1.Expr.VectorLit(es, tpe, eff, loc), increment(o))

      case OccurrenceAst1.Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
        val (e1, o1) = visit(exp1)
        val (e2, o2) = visit(exp2)
        val o3 = combineInfo(o1, o2)
        (OccurrenceAst1.Expr.VectorLoad(e1, e2, tpe, eff, loc), increment(o3))

      case OccurrenceAst1.Expr.VectorLength(exp, loc) =>
        val (e, o) = visit(exp)
        (OccurrenceAst1.Expr.VectorLength(e, loc), increment(o))

      case OccurrenceAst1.Expr.Ascribe(exp, tpe, eff, loc) =>
        val (e, o) = visit(exp)
        (OccurrenceAst1.Expr.Ascribe(e, tpe, eff, loc), increment(o))

      case OccurrenceAst1.Expr.Cast(exp, declaredType, declaredEff, tpe, eff, loc) =>
        val (e, o) = visit(exp)
        (OccurrenceAst1.Expr.Cast(e, declaredType, declaredEff, tpe, eff, loc), increment(o))

      case OccurrenceAst1.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
        val (e, o1) = visit(exp)
        val (rs, o2) = visitTryCatchRules(rules)
        val o3 = combineInfo(o1, o2) :+ sym0 -> DontInline
        (OccurrenceAst1.Expr.TryCatch(e, rs, tpe, eff, loc), increment(o3))

      case OccurrenceAst1.Expr.TryWith(exp, effUse, rules, tpe, eff, loc) =>
        val (e, o1) = visit(exp)
        val (rs, o2) = visitTryWithRules(rules)
        val o3 = combineInfo(o1, o2) :+ sym0 -> DontInline
        (OccurrenceAst1.Expr.TryWith(e, effUse, rs, tpe, eff, loc), increment(o3))

      case OccurrenceAst1.Expr.Do(op, exps, tpe, eff, loc) =>
        val (es, o) = visitExps(exps)
        (OccurrenceAst1.Expr.Do(op, es, tpe, eff, loc), increment(o))

      case OccurrenceAst1.Expr.NewObject(name, clazz, tpe, eff, methods, loc) =>
        val (ms, o) = visitJvmMethods(methods)
        (OccurrenceAst1.Expr.NewObject(name, clazz, tpe, eff, ms, loc), increment(o))

    }

    /**
      * Performs occurrence analysis on a list of expressions `exps` and merges occurrences.
      * Captures `sym0`.
      */
    def visitExps(exps: List[OccurrenceAst1.Expr])(implicit letBinding: Option[VarSym]): (List[OccurrenceAst1.Expr], OccurInfo) = {
      val (es, o1) = exps.map(visit).unzip
      val o2 = o1.foldLeft(OccurInfo.Empty)(combineInfo)
      (es, o2)
    }

    def combineApplyCloInfo(occurInfo0: OccurInfo, exp0: OccurrenceAst1.Expr): OccurInfo = exp0 match {
      case OccurrenceAst1.Expr.ApplyAtomic(AtomicOp.Closure(sym), _, _, _, _) =>
        occurInfo0 :+ sym -> Once
      case _ => occurInfo0
    }

    def visitAtomicOp(op0: AtomicOp, occurInfo0: OccurInfo)(implicit letBinding: Option[VarSym]): OccurInfo = op0 match {
      case AtomicOp.Is(sym) if sym.name == "Choice" =>
        occurInfo0 :+ sym0 -> DontInline

      case AtomicOp.HoleError(_) => letBinding match {
        case Some(varSym) => occurInfo0 + (varSym -> DontInline)
        case None => occurInfo0
      }

      case AtomicOp.MatchError => letBinding match {
        case Some(varSym) => occurInfo0 + (varSym -> DontInline)
        case None => occurInfo0
      }

      case _ => occurInfo0
    }

    def visitMatchRules(rules0: List[OccurrenceAst1.MatchRule])(implicit letBinding: Option[VarSym]): (List[OccurrenceAst1.MatchRule], OccurInfo) = {
      val (rs, o) = rules0.map {
        case OccurrenceAst1.MatchRule(pat, guard, exp) =>
          val (g, o1) = guard.map(visit).unzip
          val (e, o2) = visit(exp)
          val o3 = combineInfoOpt(o1, o2)
          val (p, syms) = visitPattern(pat, o3)
          val o4 = o3 -- syms
          (OccurrenceAst1.MatchRule(p, g, e), o4)
      }.unzip
      val o1 = o.foldLeft(OccurInfo.Empty)(combineInfoBranch)
      (rs, o1)
    }

    def visitTryCatchRules(rules0: List[OccurrenceAst1.CatchRule])(implicit letBinding: Option[VarSym]): (List[OccurrenceAst1.CatchRule], OccurInfo) = {
      val (rs, o) = rules0.map {
        case OccurrenceAst1.CatchRule(sym, clazz, exp) =>
          val (e, o) = visit(exp)
          (OccurrenceAst1.CatchRule(sym, clazz, e), o)
      }.unzip
      val o1 = o.foldLeft(OccurInfo.Empty)(combineInfo)
      (rs, o1)
    }

    def visitTryWithRules(rules0: List[OccurrenceAst1.HandlerRule])(implicit letBinding: Option[VarSym]): (List[OccurrenceAst1.HandlerRule], OccurInfo) = {
      val (rs, o) = rules0.map {
        case OccurrenceAst1.HandlerRule(op, fparams, exp) =>
          val (e, o) = visit(exp)
          (OccurrenceAst1.HandlerRule(op, fparams, e), o)
      }.unzip
      val o1 = o.foldLeft(OccurInfo.Empty)(combineInfo)
      (rs, o1)
    }

    def visitJvmMethods(methods0: List[OccurrenceAst1.JvmMethod])(implicit letBinding: Option[VarSym]): (List[OccurrenceAst1.JvmMethod], OccurInfo) = {
      val (ms, o) = methods0.map {
        case OccurrenceAst1.JvmMethod(ident, fparams, clo, retTpe, eff, loc) =>
          val (c, o) = visit(clo)
          (OccurrenceAst1.JvmMethod(ident, fparams, c, retTpe, eff, loc), increment(o))
      }.unzip
      val o1 = o.foldLeft(OccurInfo.Empty)(combineInfo)
      (ms, o1)
    }

    visit(exp00)(None)

  }

  private def visitPattern(pattern00: OccurrenceAst1.Pattern, occurInfo: OccurInfo): (OccurrenceAst1.Pattern, Set[VarSym]) = {

    def visit(pattern0: OccurrenceAst1.Pattern): (OccurrenceAst1.Pattern, Set[VarSym]) = pattern0 match {
      case OccurrenceAst1.Pattern.Wild(tpe, loc) =>
        (OccurrenceAst1.Pattern.Wild(tpe, loc), Set.empty)

      case OccurrenceAst1.Pattern.Var(sym, tpe, _, loc) =>
        (OccurrenceAst1.Pattern.Var(sym, tpe, occurInfo.get(sym), loc), Set(sym))

      case OccurrenceAst1.Pattern.Cst(cst, tpe, loc) =>
        (OccurrenceAst1.Pattern.Cst(cst, tpe, loc), Set.empty)

      case OccurrenceAst1.Pattern.Tag(sym, pat, tpe, loc) =>
        val (p, syms) = visit(pat)
        (OccurrenceAst1.Pattern.Tag(sym, p, tpe, loc), syms)

      case OccurrenceAst1.Pattern.Tuple(pats, tpe, loc) =>
        val (ps, listOfSyms) = pats.map(visit).unzip
        val syms = Set.from(listOfSyms.flatten)
        (OccurrenceAst1.Pattern.Tuple(ps, tpe, loc), syms)

      case OccurrenceAst1.Pattern.Record(pats, pat, tpe, loc) =>
        val (ps, listOfSyms) = pats.map(visitRecordLabelPattern).unzip
        val (p, syms0) = visit(pat)
        val syms = Set.from(listOfSyms.flatten) ++ syms0
        (OccurrenceAst1.Pattern.Record(ps, p, tpe, loc), syms)

      case OccurrenceAst1.Pattern.RecordEmpty(tpe, loc) =>
        (OccurrenceAst1.Pattern.RecordEmpty(tpe, loc), Set.empty)
    }

    def visitRecordLabelPattern(pattern0: OccurrenceAst1.Pattern.Record.RecordLabelPattern): (OccurrenceAst1.Pattern.Record.RecordLabelPattern, Set[VarSym]) = pattern0 match {
      case OccurrenceAst1.Pattern.Record.RecordLabelPattern(label, pat, tpe, loc) =>
        val (p, syms) = visit(pat)
        (OccurrenceAst1.Pattern.Record.RecordLabelPattern(label, p, tpe, loc), syms)
    }

    visit(pattern00)
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
    val size = o1.size + o2.size
    OccurInfo(defMap, varMap, size)
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
    case (DontInline, _) => DontInline
    case (_, DontInline) => DontInline
    case (Dead, _) => o2
    case (_, Dead) => o1
    case _ => Many
  }

  /**
    * Combines two occurrences `o1` and `o2` of type Occur into a single occurrence based on ManyBranches logic.
    * ManyBranches can be
    * - [[OccurrenceAst1.Expr.IfThenElse]]
    * - [[OccurrenceAst1.Expr.Match]]
    */
  private def combineBranch(o1: Occur, o2: Occur): Occur = (o1, o2) match {
    case (DontInline, _) => DontInline
    case (_, DontInline) => DontInline
    case (Dead, _) => o2
    case (_, Dead) => o1
    case _ => ManyBranch
  }

  // TODO: Add doc
  private def captureVars(occurInfo: OccurInfo): OccurInfo = {
    update(occurInfo) {
      case Once => OnceInAbstraction
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


