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
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Expr
import ca.uwaterloo.flix.language.ast.OccurrenceAst1.Occur.*
import ca.uwaterloo.flix.language.ast.OccurrenceAst1.{DefContext, Occur}
import ca.uwaterloo.flix.language.ast.Symbol.{DefnSym, LabelSym, VarSym}
import ca.uwaterloo.flix.language.ast.{AtomicOp, OccurrenceAst1, SimplifiedAst, Symbol}
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

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
    /**
      * Increments number of expressions by one
      */
    def increaseSizeByOne(): OccurInfo = this.copy(size = this.size + 1)
  }


  /**
    * Performs occurrence analysis on the given AST `root`.
    */
  def run(root: SimplifiedAst.Root)(implicit flix: Flix): OccurrenceAst1.Root = {

    val defs = visitDefs(root.defs)
    val effects = root.effects.map { case (k, v) => k -> visitEffect(v) }

    OccurrenceAst1.Root(defs, effects, root.entryPoint, root.reachable, root.sources)
  }

  private def visitEffect(effect: SimplifiedAst.Effect): OccurrenceAst1.Effect = effect match {
    case SimplifiedAst.Effect(ann, mod, sym, ops0, loc) =>
      val ops = ops0.map(visitEffectOp)
      OccurrenceAst1.Effect(ann, mod, sym, ops, loc)
  }

  private def visitEffectOp(op: SimplifiedAst.Op): OccurrenceAst1.Op = op match {
    case SimplifiedAst.Op(sym, ann, mod, fparams0, tpe, purity, loc) =>
      val fparams = fparams0.map(visitFormalParam)
      OccurrenceAst1.Op(sym, ann, mod, fparams, tpe, purity, loc)
  }

  /**
    * Performs occurrence analysis on every entry in `defs0` in parallel.
    * Sets the occurrence of each `def` based on the occurrence found in `defOccur`. TODO: What does this mean?
    */
  private def visitDefs(defs0: Map[DefnSym, SimplifiedAst.Def])(implicit flix: Flix): Map[DefnSym, OccurrenceAst1.Def] = {
    val (ds, os) = ParOps.parMap(defs0.values)((d: SimplifiedAst.Def) => visitDef(d)).unzip

    // Combine all `defOccurrences` into 1 map.
    val defOccur = os.foldLeft(Map.empty[DefnSym, Occur])((acc, o) => combineMaps(acc, o.defs, combineSeq))

    // Updates the occurrence of every `def` in `ds` based on the occurrence found in `defOccur`.
    ds.foldLeft(Map.empty[DefnSym, OccurrenceAst1.Def]) {
      case (macc, defn) => macc + (defn.sym -> defn.copy(context = defn.context.copy(occur = defOccur.getOrElse(defn.sym, Dead))))
    }
  }

  /**
    * Performs occurrence analysis on `defn`.
    */
  private def visitDef(defn: SimplifiedAst.Def): (OccurrenceAst1.Def, OccurInfo) = {
    val (e, oi) = visitExp(defn.sym, defn.exp)
    val fparams = defn.fparams.map(visitFormalParam).map(p => (p, oi.vars.getOrElse(p.sym, Dead)))
    /// Def consists of a single direct call to a def
    val isDirectCall = e match {
      case OccurrenceAst1.Expr.ApplyDef(_, _, _, _, _) => true
      case OccurrenceAst1.Expr.ApplyClo(clo, _, _, _, _) =>
        clo match {
          case OccurrenceAst1.Expr.ApplyAtomic(AtomicOp.Closure(_), _, _, _, _) => true
          case _ => false
        }
      case _ => false

    }
    val isSelfRecursive = oi.defs.get(defn.sym) match {
      case None => false
      case Some(o) => o match {
        case Occur.Dead => false
        case Occur.Once => true
        case Occur.Many => true
        case Occur.ManyBranch => true
        case Occur.DontInline => false
      }
    }
    val defContext = DefContext(isDirectCall, oi.defs.getOrElse(defn.sym, Dead), oi.size, isSelfRecursive)
    (OccurrenceAst1.Def(defn.ann, defn.mod, defn.sym, fparams, e, defContext, defn.tpe, defn.purity, defn.loc), oi)
  }

  /**
    * Translates the given formal param `p` to the OccurrenceAst1.
    */
  private def visitFormalParam(p: SimplifiedAst.FormalParam): OccurrenceAst1.FormalParam =
    OccurrenceAst1.FormalParam(p.sym, p.mod, p.tpe, p.loc)

  /**
    * Performs occurrence analysis on `exp0`
    */
  private def visitExp(sym0: Symbol.DefnSym, exp0: SimplifiedAst.Expr): (OccurrenceAst1.Expr, OccurInfo) = exp0 match {
    case Expr.Cst(cst, tpe, loc) => (OccurrenceAst1.Expr.Cst(cst, tpe, loc), OccurInfo.One)

    case Expr.Var(sym, tpe, loc) => (OccurrenceAst1.Expr.Var(sym, tpe, loc), OccurInfo(Map.empty, Map(sym -> Once), 1))

    case Expr.Lambda(fparams, exp, tpe, loc) => ???

    case Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
      val (es, o) = visitExps(sym0, exps)
      val o1 = op match {
        case AtomicOp.Is(sym) if sym.name == "Choice" => o.copy(defs = o.defs + (sym0 -> DontInline)).increaseSizeByOne()
        case _ => o.increaseSizeByOne()
      }
      (OccurrenceAst1.Expr.ApplyAtomic(op, es, tpe, purity, loc), o1)

    case Expr.ApplyClo(exp, exps, tpe, purity, loc) =>
      val (e, o1) = visitExp(sym0, exp)
      val (es, o2) = visitExps(sym0, exps)
      val o3 = combineAllSeq(o1, o2)
      exp match {
        case Expr.ApplyAtomic(AtomicOp.Closure(sym), _, _, _, _) =>
          val o4 = OccurInfo(Map(sym -> Once), Map.empty, 0)
          val o5 = combineAllSeq(o3, o4)
          (OccurrenceAst1.Expr.ApplyClo(e, es, tpe, purity, loc), o5.increaseSizeByOne())
        case _ => (OccurrenceAst1.Expr.ApplyClo(e, es, tpe, purity, loc), o3.increaseSizeByOne())
      }

    case Expr.ApplyDef(sym, exps, tpe, purity, loc) =>
      val (es, o1) = visitExps(sym0, exps)
      val o2 = OccurInfo(Map(sym -> Once), Map.empty, 0)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst1.Expr.ApplyDef(sym, es, tpe, purity, loc), o3.increaseSizeByOne())

    case Expr.ApplyLocalDef(sym, exps, tpe, purity, loc) =>
      val (es, o1) = visitExps(sym0, exps)
      val o2 = OccurInfo(Map.empty, Map(sym -> Once), 1)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst1.Expr.ApplyLocalDef(sym, es, tpe, purity, loc), o3)

    case Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val (e1, o1) = visitExp(sym0, exp1)
      val (e2, o2) = visitExp(sym0, exp2)
      val (e3, o3) = visitExp(sym0, exp3)
      val o4 = combineAllSeq(o1, combineAllBranch(o2, o3))
      (OccurrenceAst1.Expr.IfThenElse(e1, e2, e3, tpe, purity, loc), o4.increaseSizeByOne())

    case Expr.Branch(exp, branches, tpe, purity, loc) =>
      val (e1, o1) = visitExp(sym0, exp)
      val (o2, bs) = branches.foldLeft(OccurInfo.Empty, Map[LabelSym, OccurrenceAst1.Expr]())((acc, b) => {
        val (oacc, bsacc) = acc
        b match {
          case (sym, exp1) =>
            val (e2, o3) = visitExp(sym0, exp1)
            val o4 = combineAllBranch(oacc, o3)
            val bs = bsacc + (sym -> e2)
            (o4, bs)
        }
      })
      val o5 = combineAllSeq(o1, o2)
      (OccurrenceAst1.Expr.Branch(e1, bs, tpe, purity, loc), o5.increaseSizeByOne())

    case Expr.JumpTo(sym, tpe, purity, loc) => (OccurrenceAst1.Expr.JumpTo(sym, tpe, purity, loc), OccurInfo.One)

    case Expr.Let(sym, exp1, exp2, tpe, purity, loc) =>
      val (e1, o1) = visitExp(sym0, exp1)
      val (e2, o2) = visitExp(sym0, exp2)
      val o3 = combineAllSeq(o1, o2)
      val occur = o3.vars.getOrElse(sym, Dead)
      val o4 = o3.copy(vars = o3.vars - sym)
      (OccurrenceAst1.Expr.Let(sym, e1, e2, occur, tpe, purity, loc), o4.increaseSizeByOne())

    case Expr.LocalDef(sym, fparams, exp1, exp2, tpe, purity, loc) => ???

    case Expr.Stm(exp1, exp2, tpe, purity, loc) =>
      val (e1, o1) = visitExp(sym0, exp1)
      val (e2, o2) = visitExp(sym0, exp2)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst1.Expr.Stmt(e1, e2, tpe, purity, loc), o3.increaseSizeByOne())

    case Expr.Scope(sym, exp, tpe, purity, loc) =>
      val (e, o) = visitExp(sym0, exp)
      (OccurrenceAst1.Expr.Scope(sym, e, tpe, purity, loc), o.copy(defs = o.defs + (sym0 -> DontInline)).increaseSizeByOne())

    case Expr.TryCatch(exp, rules, tpe, purity, loc) =>
      val (e, o1) = visitExp(sym0, exp)
      val (rs, o2) = rules.map {
        case SimplifiedAst.CatchRule(sym, clazz, exp) =>
          val (e, o3) = visitExp(sym0, exp)
          (OccurrenceAst1.CatchRule(sym, clazz, e), o3)
      }.unzip
      val o4 = o2.foldLeft(o1)((acc, o5) => combineAllSeq(acc, o5))
      (OccurrenceAst1.Expr.TryCatch(e, rs, tpe, purity, loc), o4.copy(defs = o4.defs + (sym0 -> DontInline)).increaseSizeByOne())

    case Expr.TryWith(exp, effUse, rules, tpe, purity, loc) =>
      val (e, o1) = visitExp(sym0, exp)
      val (rs, o2) = rules.map {
        case SimplifiedAst.HandlerRule(op, fparams, exp) =>
          val (e, o3) = visitExp(sym0, exp)
          val fps = fparams.map(visitFormalParam)
          (OccurrenceAst1.HandlerRule(op, fps, e), o3)
      }.unzip
      val o4 = o2.foldLeft(o1)((acc, o5) => combineAllSeq(acc, o5))
      (OccurrenceAst1.Expr.TryWith(e, effUse, rs, tpe, purity, loc), o4.copy(defs = o4.defs + (sym0 -> DontInline)).increaseSizeByOne())

    case Expr.Do(op, exps, tpe, purity, loc) =>
      val (es, o1) = visitExps(sym0, exps)
      (OccurrenceAst1.Expr.Do(op, es, tpe, purity, loc), o1.increaseSizeByOne())

    case Expr.NewObject(name, clazz, tpe, purity, methods, loc) =>
      val (ms, o1) = methods.map {
        case SimplifiedAst.JvmMethod(ident, fparams, clo, retTpe, purity, loc) => {
          val f = fparams.map {
            case SimplifiedAst.FormalParam(sym, mod, tpe, loc) => OccurrenceAst1.FormalParam(sym, mod, tpe, loc)
          }
          val (c, o) = visitExp(sym0, clo)
          (OccurrenceAst1.JvmMethod(ident, f, c, retTpe, purity, loc), o.increaseSizeByOne())
        }
      }.unzip
      val o2 = o1.foldLeft(OccurInfo.Empty)((acc, o3) => combineAllSeq(acc, o3))
      (OccurrenceAst1.Expr.NewObject(name, clazz, tpe, purity, ms, loc), o2.increaseSizeByOne())

    case Expr.LambdaClosure(_, _, _, _, _, loc) => throw InternalCompilerException("unexpected lambda closure expression", loc)

  }

  /**
    * Performs occurrence analysis on a list of expressions `exps`` and merges occurrences.
    */
  private def visitExps(sym0: Symbol.DefnSym, exps: List[SimplifiedAst.Expr]): (List[OccurrenceAst1.Expr], OccurInfo) = {
    val (es, o1) = exps.map(visitExp(sym0, _)).unzip
    val o2 = o1.foldLeft(OccurInfo.Empty)((acc, o3) => combineAllSeq(acc, o3))
    (es, o2)
  }

  /**
    * Combines the 2 objects `o1` and `o2` of the type OccurInfo into a single OccurInfo object.
    */
  private def combineAllBranch(o1: OccurInfo, o2: OccurInfo): OccurInfo = {
    combineAll(o1, o2, combineBranch)
  }

  /**
    * Combines the 2 objects `o1` and `o2` of the type OccurInfo into a single OccurInfo object.
    */
  private def combineAllSeq(o1: OccurInfo, o2: OccurInfo): OccurInfo = {
    combineAll(o1, o2, combineSeq)
  }

  /**
    * Combines the 2 objects `o1` and `o2` of the type OccurInfo into a single OccurInfo object.
    */
  private def combineAll(o1: OccurInfo, o2: OccurInfo, combine: (Occur, Occur) => Occur): OccurInfo = {
    val varMap = combineMaps(o1.vars, o2.vars, combine)
    val defMap = combineMaps(o1.defs, o2.defs, combine)
    val size = o1.size + o2.size
    OccurInfo(defMap, varMap, size)
  }

  /**
    * Combines the 2 maps `m1` and `m2` of the type (A -> Occur) into a single map of the same type.
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
    * Combines two occurrences `o1` and `o2` of type Occur into a single occurrence.
    */
  private def combineSeq(o1: Occur, o2: Occur): Occur = (o1, o2) match {
    case (DontInline, _) => DontInline
    case (_, DontInline) => DontInline
    case (Dead, _) => o2
    case (_, Dead) => o1
    case _ => Many
  }

  /**
    * Combines two occurrences `o1` and `o2` of type Occur into a single occurrence based on ManyBranches logic.
    * ManyBranches can be IfThenElse, Branches, and SelectChannel
    */
  private def combineBranch(o1: Occur, o2: Occur): Occur = (o1, o2) match {
    case (DontInline, _) => DontInline
    case (_, DontInline) => DontInline
    case (Dead, _) => o2
    case (_, Dead) => o1
    case (Once, Once) => ManyBranch
    case (Once, ManyBranch) => ManyBranch
    case (ManyBranch, Once) => ManyBranch
    case (ManyBranch, ManyBranch) => ManyBranch
    case _ => Many
  }
}


