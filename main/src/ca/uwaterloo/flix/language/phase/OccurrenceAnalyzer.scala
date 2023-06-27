/*
 * Copyright 2022 Anna Krogh, Patrick Lundvig, Christian Bonde
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
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.LiftedAst.Expression
import ca.uwaterloo.flix.language.ast.OccurrenceAst.Occur._
import ca.uwaterloo.flix.language.ast.OccurrenceAst.{DefContext, Occur}
import ca.uwaterloo.flix.language.ast.Symbol.{DefnSym, LabelSym, VarSym}
import ca.uwaterloo.flix.language.ast.{Ast, AtomicOp, LiftedAst, OccurrenceAst, Symbol, Type}
import ca.uwaterloo.flix.util.Validation.ToSuccess
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Validation}

/**
  * The occurrence analyzer collects information on variable and function usage and calculates the weight of the expressions
  * Marks a variable or function as Dead if it is not used, Once if it is used exactly once and Many otherwise
  */
object OccurrenceAnalyzer {

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
  def run(root: LiftedAst.Root)(implicit flix: Flix): Validation[OccurrenceAst.Root, CompilationMessage] = flix.subphase("OccurrenceAnalyzer") {

    // Visit every definition in the program in parallel and transform to type 'OccurrenceAst.Def'
    val defs = visitDefs(root.defs)

    // Visit every enum in the program and transform to type 'OccurrenceAst.Enum'
    val enums = root.enums.map { case (k, v) => k -> visitEnum(v) }

    // Reassemble the ast root.
    val result = OccurrenceAst.Root(defs, enums, root.entryPoint, root.sources)

    result.toSuccess
  }

  /**
    * Translates the given enum `enum0` to the OccurrenceAst.
    */
  private def visitEnum(enum0: LiftedAst.Enum): OccurrenceAst.Enum = {
    val cases = enum0.cases.map {
      case (k, v) =>
        k -> visitCase(v)
    }
    OccurrenceAst.Enum(enum0.ann, enum0.mod, enum0.sym, cases, enum0.tpe, enum0.loc)
  }

  /**
    * Translates the given case `case0` to the OccurrenceAst.
    */
  private def visitCase(case0: LiftedAst.Case): OccurrenceAst.Case = {
    OccurrenceAst.Case(case0.sym, case0.tpe, case0.loc)
  }

  /**
    * Visits every definition in the program in parallel and perform occurrence analysis.
    * Set the occurrence of each `def` based on the occurrence found in `defOccur`
    */
  private def visitDefs(defs0: Map[DefnSym, LiftedAst.Def])(implicit flix: Flix): Map[DefnSym, OccurrenceAst.Def] = {
    val (ds, os) = ParOps.parMap(defs0.values)((d: LiftedAst.Def) => visitDef(d)).unzip

    // Combine all `defOccurrences` into 1 map.
    val defOccur = os.foldLeft(Map.empty[DefnSym, Occur])((acc, o) => combineMaps(acc, o.defs, combineSeq))

    // Updates the occurrence of every `def` in `ds` based on the occurrence found in `defOccur`.
    ds.foldLeft(Map.empty[DefnSym, OccurrenceAst.Def]) {
      case (macc, defn) => macc + (defn.sym -> defn.copy(context = defn.context.copy(occur = defOccur.getOrElse(defn.sym, Dead))))
    }
  }

  /**
    * Visits a definition in the program and performs occurrence analysis
    */
  private def visitDef(defn: LiftedAst.Def): (OccurrenceAst.Def, OccurInfo) = {
    val cparams = defn.cparams.map(visitFormalParam)
    val fparams = defn.fparams.map(visitFormalParam)
    val (e, oi) = visitExp(defn.sym, defn.exp)
    /// Def consists of a single direct call to a def
    val isDirectCall = e match {
      case OccurrenceAst.Expression.ApplyDefTail(_, _, _, _, _) => true
      case OccurrenceAst.Expression.ApplyCloTail(clo, _, _, _, _) =>
        clo match {
          case OccurrenceAst.Expression.Closure(_, _, _, _) => true
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
    (OccurrenceAst.Def(defn.ann, defn.mod, defn.sym, cparams, fparams, e, defContext, defn.tpe, defn.purity, defn.loc), oi)
  }

  /**
    * Translates the given formal param `p` to the OccurrenceAst.
    */
  private def visitFormalParam(p: LiftedAst.FormalParam): OccurrenceAst.FormalParam =
    OccurrenceAst.FormalParam(p.sym, p.mod, p.tpe, p.loc)

  /**
    * Performs occurrence analysis on the given expression `exp0`
    */
  private def visitExp(sym0: Symbol.DefnSym, exp0: LiftedAst.Expression): (OccurrenceAst.Expression, OccurInfo) = exp0 match {
    case Expression.Cst(cst, tpe, loc) => (OccurrenceAst.Expression.Constant(cst, tpe, loc), OccurInfo.One)

    case Expression.Var(sym, tpe, loc) => (OccurrenceAst.Expression.Var(sym, tpe, loc), OccurInfo(Map.empty, Map(sym -> Once), 1))

    case Expression.ApplyAtomic(op, exps, tpe, purity, loc) =>
      val (es, o) = visitExps(sym0, exps)

      op match { // Will be removed later
        case AtomicOp.Closure(sym) => (OccurrenceAst.Expression.Closure(sym, es, tpe, loc), o)

        case AtomicOp.Unary(sop) => (OccurrenceAst.Expression.Unary(sop, es.head, tpe, purity, loc), o.increaseSizeByOne())

        case AtomicOp.Binary(sop) =>
          val List(e1, e2) = es
          (OccurrenceAst.Expression.Binary(sop, e1, e2, tpe, purity, loc), o.increaseSizeByOne())

        case AtomicOp.Region => (OccurrenceAst.Expression.Region(tpe, loc), OccurInfo.One)

        case AtomicOp.ScopeExit =>
          val List(e1, e2) = es
          (OccurrenceAst.Expression.ScopeExit(e1, e2, tpe, purity, loc), o.increaseSizeByOne())

        case _ => throw InternalCompilerException("Unexpected AtomicOp", loc)
      }

    case Expression.ApplyClo(exp, args, tpe, purity, loc) =>
      val (e, o1) = visitExp(sym0, exp)
      val (as, o2) = visitExps(sym0, args)
      val o3 = combineAllSeq(o1, o2)
      exp match {
        case Expression.ApplyAtomic(AtomicOp.Closure(sym), _, _, _, _) =>
          val o4 = OccurInfo(Map(sym -> Once), Map.empty, 0)
          val o5 = combineAllSeq(o3, o4)
          (OccurrenceAst.Expression.ApplyClo(e, as, tpe, purity, loc), o5.increaseSizeByOne())
        case _ => (OccurrenceAst.Expression.ApplyClo(e, as, tpe, purity, loc), o3.increaseSizeByOne())
      }

    case Expression.ApplyDef(sym, args, tpe, purity, loc) =>
      val (as, o1) = visitExps(sym0, args)
      val o2 = OccurInfo(Map(sym -> Once), Map.empty, 0)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.ApplyDef(sym, as, tpe, purity, loc), o3.increaseSizeByOne())

    case Expression.ApplyCloTail(exp, args, tpe, purity, loc) =>
      val (e, o1) = visitExp(sym0, exp)
      val (as, o2) = visitExps(sym0, args)
      val o3 = combineAllSeq(o1, o2)
      exp match {
        case Expression.ApplyAtomic(AtomicOp.Closure(sym), _, _, _, _) =>
          val o4 = OccurInfo(Map(sym -> Once), Map.empty, 0)
          val o5 = combineAllSeq(o3, o4)
          (OccurrenceAst.Expression.ApplyCloTail(e, as, tpe, purity, loc), o5.increaseSizeByOne())
        case _ => (OccurrenceAst.Expression.ApplyCloTail(e, as, tpe, purity, loc), o3.increaseSizeByOne())
      }

    case Expression.ApplyDefTail(sym, args, tpe, purity, loc) =>
      val (as, o1) = visitExps(sym0, args)
      val o2 = OccurInfo(Map(sym -> Once), Map.empty, 0)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.ApplyDefTail(sym, as, tpe, purity, loc), o3.increaseSizeByOne())

    case Expression.ApplySelfTail(sym, formals, actuals, tpe, purity, loc) =>
      val (as, o1) = visitExps(sym0, actuals)
      val f = formals.map {
        case LiftedAst.FormalParam(sym, mod, tpe, loc) => OccurrenceAst.FormalParam(sym, mod, tpe, loc)
      }
      val o2 = OccurInfo(Map(sym -> Once), Map.empty, 0)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.ApplySelfTail(sym, f, as, tpe, purity, loc), o3.increaseSizeByOne())

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val (e1, o1) = visitExp(sym0, exp1)
      val (e2, o2) = visitExp(sym0, exp2)
      val (e3, o3) = visitExp(sym0, exp3)
      val o4 = combineAllSeq(o1, combineAllBranch(o2, o3))
      (OccurrenceAst.Expression.IfThenElse(e1, e2, e3, tpe, purity, loc), o4.increaseSizeByOne())

    case Expression.Branch(exp, branches, tpe, purity, loc) =>
      val (e1, o1) = visitExp(sym0, exp)
      val (o2, bs) = branches.foldLeft(OccurInfo.Empty, Map[LabelSym, OccurrenceAst.Expression]())((acc, b) => {
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
      (OccurrenceAst.Expression.Branch(e1, bs, tpe, purity, loc), o5.increaseSizeByOne())

    case Expression.JumpTo(sym, tpe, purity, loc) => (OccurrenceAst.Expression.JumpTo(sym, tpe, purity, loc), OccurInfo.One)

    case Expression.Let(sym, exp1, exp2, tpe, purity, loc) =>
      val (e1, o1) = visitExp(sym0, exp1)
      val (e2, o2) = visitExp(sym0, exp2)
      val o3 = combineAllSeq(o1, o2)
      val occur = o3.vars.getOrElse(sym, Dead)
      val o4 = o3.copy(vars = o3.vars - sym)
      (OccurrenceAst.Expression.Let(sym, e1, e2, occur, tpe, purity, loc), o4.increaseSizeByOne())

    case Expression.LetRec(varSym, index, defSym, exp1, exp2, tpe, purity, loc) =>
      val (e1, o1) = visitExp(sym0, exp1)
      val (e2, o2) = visitExp(sym0, exp2)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.LetRec(varSym, index, defSym, e1, e2, tpe, purity, loc), o3.increaseSizeByOne())

    case Expression.Scope(sym, exp, tpe, purity, loc) =>
      val (e, o) = visitExp(sym0, exp)
      (OccurrenceAst.Expression.Scope(sym, e, tpe, purity, loc), o.copy(defs = o.defs + (sym0 -> DontInline)).increaseSizeByOne())

    case Expression.Is(sym, exp, purity, loc) =>
      val (e, o) = visitExp(sym0, exp)
      if (sym.name == "Choice")
        (OccurrenceAst.Expression.Is(sym, e, purity, loc), o.copy(defs = o.defs + (sym0 -> DontInline)).increaseSizeByOne())
      else
        (OccurrenceAst.Expression.Is(sym, e, purity, loc), o.increaseSizeByOne())

    case Expression.Tag(sym, exp, tpe, purity, loc) =>
      val (e, o) = visitExp(sym0, exp)
      (OccurrenceAst.Expression.Tag(sym, e, tpe, purity, loc), o.increaseSizeByOne())

    case Expression.Untag(sym, exp, tpe, purity, loc) =>
      val (e, o) = visitExp(sym0, exp)
      (OccurrenceAst.Expression.Untag(sym, e, tpe, purity, loc), o.increaseSizeByOne())

    case Expression.Index(base, offset, tpe, purity, loc) =>
      val (b, o) = visitExp(sym0, base)
      (OccurrenceAst.Expression.Index(b, offset, tpe, purity, loc), o.increaseSizeByOne())

    case Expression.Tuple(elms, tpe, purity, loc) =>
      val (es, o) = visitExps(sym0, elms)
      (OccurrenceAst.Expression.Tuple(es, tpe, purity, loc), o.increaseSizeByOne())

    case Expression.RecordEmpty(tpe, loc) => (OccurrenceAst.Expression.RecordEmpty(tpe, loc), OccurInfo.One)

    case Expression.RecordSelect(exp, field, tpe, purity, loc) =>
      val (e, o) = visitExp(sym0, exp)
      (OccurrenceAst.Expression.RecordSelect(e, field, tpe, purity, loc), o.increaseSizeByOne())

    case Expression.RecordExtend(field, value, rest, tpe, purity, loc) =>
      val (v, o1) = visitExp(sym0, value)
      val (r, o2) = visitExp(sym0, rest)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.RecordExtend(field, v, r, tpe, purity, loc), o3.increaseSizeByOne())

    case Expression.RecordRestrict(field, rest, tpe, purity, loc) =>
      val (r, o) = visitExp(sym0, rest)
      (OccurrenceAst.Expression.RecordRestrict(field, r, tpe, purity, loc), o.increaseSizeByOne())

    case Expression.ArrayLit(elms, tpe, loc) =>
      val (es, o) = visitExps(sym0, elms)
      (OccurrenceAst.Expression.ArrayLit(es, tpe, loc), o.increaseSizeByOne())

    case Expression.ArrayNew(elm, len, tpe, loc) =>
      val (e, o1) = visitExp(sym0, elm)
      val (l, o2) = visitExp(sym0, len)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.ArrayNew(e, l, tpe, loc), o3.increaseSizeByOne())

    case Expression.ArrayLoad(base, index, tpe, loc) =>
      val (b, o1) = visitExp(sym0, base)
      val (i, o2) = visitExp(sym0, index)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.ArrayLoad(b, i, tpe, loc), o3.increaseSizeByOne())

    case Expression.ArrayStore(base, index, elm, tpe, loc) =>
      val (b, o1) = visitExp(sym0, base)
      val (i, o2) = visitExp(sym0, index)
      val (e, o3) = visitExp(sym0, elm)
      val o4 = combineAllSeq(o1, combineAllSeq(o2, o3))
      (OccurrenceAst.Expression.ArrayStore(b, i, e, tpe, loc), o4.increaseSizeByOne())

    case Expression.ArrayLength(base, tpe, _, loc) =>
      val (b, o) = visitExp(sym0, base)
      val purity = b.purity
      (OccurrenceAst.Expression.ArrayLength(b, tpe, purity, loc), o.increaseSizeByOne())

    case Expression.Ref(exp, tpe, loc) =>
      val (e, o) = visitExp(sym0, exp)
      (OccurrenceAst.Expression.Ref(e, tpe, loc), o.increaseSizeByOne())

    case Expression.Deref(exp, tpe, loc) =>
      val (e, o) = visitExp(sym0, exp)
      (OccurrenceAst.Expression.Deref(e, tpe, loc), o.increaseSizeByOne())

    case Expression.Assign(exp1, exp2, tpe, loc) =>
      val (e1, o1) = visitExp(sym0, exp1)
      val (e2, o2) = visitExp(sym0, exp2)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.Assign(e1, e2, tpe, loc), o3.increaseSizeByOne())

    case Expression.InstanceOf(exp, clazz, loc) =>
      val (e, o) = visitExp(sym0, exp)
      (OccurrenceAst.Expression.InstanceOf(e, clazz, loc), o.increaseSizeByOne())

    case Expression.Cast(exp, tpe, purity, loc) =>
      val (e, o) = visitExp(sym0, exp)
      (OccurrenceAst.Expression.Cast(e, tpe, purity, loc), o.increaseSizeByOne())

    case Expression.TryCatch(exp, rules, tpe, purity, loc) =>
      val (e, o1) = visitExp(sym0, exp)
      val (rs, o2) = rules.map {
        case LiftedAst.CatchRule(sym, clazz, exp) =>
          val (e, o3) = visitExp(sym0, exp)
          (OccurrenceAst.CatchRule(sym, clazz, e), o3)
      }.unzip
      val o4 = o2.foldLeft(o1)((acc, o5) => combineAllSeq(acc, o5))
      (OccurrenceAst.Expression.TryCatch(e, rs, tpe, purity, loc), o4.copy(defs = o4.defs + (sym0 -> DontInline)).increaseSizeByOne())

    case Expression.TryWith(exp, effUse, rules, tpe, purity, loc) =>
      // TODO AE erasing to unit for now
      (OccurrenceAst.Expression.Constant(Ast.Constant.Unit, Type.Unit, loc), OccurInfo.Empty)

    case Expression.Do(op, exps, tpe, purity, loc) =>
      // TODO AE erasing to unit for now
      (OccurrenceAst.Expression.Constant(Ast.Constant.Unit, Type.Unit, loc), OccurInfo.Empty)

    case Expression.Resume(exp, tpe, loc) =>
      // TODO AE erasing to unit for now
      (OccurrenceAst.Expression.Constant(Ast.Constant.Unit, Type.Unit, loc), OccurInfo.Empty)

    case Expression.InvokeConstructor(constructor, args, tpe, purity, loc) =>
      val (as, o) = visitExps(sym0, args)
      (OccurrenceAst.Expression.InvokeConstructor(constructor, as, tpe, purity, loc), o.increaseSizeByOne())

    case Expression.InvokeMethod(method, exp, args, tpe, purity, loc) =>
      val (e, o1) = visitExp(sym0, exp)
      val (as, o2) = visitExps(sym0, args)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.InvokeMethod(method, e, as, tpe, purity, loc), o3.increaseSizeByOne())

    case Expression.InvokeStaticMethod(method, args, tpe, purity, loc) =>
      val (as, o) = visitExps(sym0, args)
      (OccurrenceAst.Expression.InvokeStaticMethod(method, as, tpe, purity, loc), o.increaseSizeByOne())

    case Expression.GetField(field, exp, tpe, purity, loc) =>
      val (e, o) = visitExp(sym0, exp)
      (OccurrenceAst.Expression.GetField(field, e, tpe, purity, loc), o.increaseSizeByOne())

    case Expression.PutField(field, exp1, exp2, tpe, purity, loc) =>
      val (e1, o1) = visitExp(sym0, exp1)
      val (e2, o2) = visitExp(sym0, exp2)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.PutField(field, e1, e2, tpe, purity, loc), o3.increaseSizeByOne())

    case Expression.GetStaticField(field, tpe, purity, loc) =>
      (OccurrenceAst.Expression.GetStaticField(field, tpe, purity, loc), OccurInfo.One)

    case Expression.PutStaticField(field, exp, tpe, purity, loc) =>
      val (e, o) = visitExp(sym0, exp)
      (OccurrenceAst.Expression.PutStaticField(field, e, tpe, purity, loc), o.increaseSizeByOne())

    case Expression.NewObject(name, clazz, tpe, purity, methods, loc) =>
      val (ms, o1) = methods.map {
        case LiftedAst.JvmMethod(ident, fparams, clo, retTpe, purity, loc) => {
          val f = fparams.map {
            case LiftedAst.FormalParam(sym, mod, tpe, loc) => OccurrenceAst.FormalParam(sym, mod, tpe, loc)
          }
          val (c, o) = visitExp(sym0, clo)
          (OccurrenceAst.JvmMethod(ident, f, c, retTpe, purity, loc), o.increaseSizeByOne())
        }
      }.unzip
      val o2 = o1.foldLeft(OccurInfo.Empty)((acc, o3) => combineAllSeq(acc, o3))
      (OccurrenceAst.Expression.NewObject(name, clazz, tpe, purity, ms, loc), o2.increaseSizeByOne())

    case Expression.Spawn(exp1, exp2, tpe, loc) =>
      val (e1, o1) = visitExp(sym0, exp1)
      val (e2, o2) = visitExp(sym0, exp2)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.Spawn(e1, e2, tpe, loc), o3.increaseSizeByOne())

    case Expression.Lazy(exp, tpe, loc) =>
      val (e, o1) = visitExp(sym0, exp)
      (OccurrenceAst.Expression.Lazy(e, tpe, loc), o1.increaseSizeByOne())

    case Expression.Force(exp, tpe, loc) =>
      val (e, o1) = visitExp(sym0, exp)
      (OccurrenceAst.Expression.Force(e, tpe, loc), o1.increaseSizeByOne())

    case Expression.HoleError(sym, tpe, loc) =>
      (OccurrenceAst.Expression.HoleError(sym, tpe, loc), OccurInfo.One)

    case Expression.MatchError(tpe, loc) =>
      (OccurrenceAst.Expression.MatchError(tpe, loc), OccurInfo.One)
  }

  /**
    * Performs occurrence analysis on a list of expressions 'exps' and merges occurrences
    */
  private def visitExps(sym0: Symbol.DefnSym, exps: List[LiftedAst.Expression]): (List[OccurrenceAst.Expression], OccurInfo) = {
    val (es, o1) = exps.map(visitExp(sym0, _)).unzip
    val o2 = o1.foldLeft(OccurInfo.Empty)((acc, o3) => combineAllSeq(acc, o3))
    (es, o2)
  }

  /**
    * Combines the 2 objects `o1` and `o2` of the type OccurInfo into a single OccurInfo object using the function `combineBranch`.
    */
  private def combineAllBranch(o1: OccurInfo, o2: OccurInfo): OccurInfo = {
    combineAll(o1, o2, combineBranch)
  }

  /**
    * Combines the 2 objects `o1` and `o2` of the type OccurInfo into a single OccurInfo object using the function `combineSeq`.
    */
  private def combineAllSeq(o1: OccurInfo, o2: OccurInfo): OccurInfo = {
    combineAll(o1, o2, combineSeq)
  }

  /**
    * Combines the 2 objects `o1` and `o2` of the type OccurInfo into a single OccurInfo object using the argument `combine`.
    */
  private def combineAll(o1: OccurInfo, o2: OccurInfo, combine: (Occur, Occur) => Occur): OccurInfo = {
    val varMap = combineMaps(o1.vars, o2.vars, combine)
    val defMap = combineMaps(o1.defs, o2.defs, combine)
    val size = o1.size + o2.size
    OccurInfo(defMap, varMap, size)
  }

  /**
    * Combines the 2 maps `m1` and `m2` of the type (A -> Occur) into a single map of the same type using the argument `combine`.
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


