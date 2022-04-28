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
import ca.uwaterloo.flix.language.ast.OccurrenceAst.{DefContext, Occur}
import ca.uwaterloo.flix.language.ast.OccurrenceAst.Occur._
import ca.uwaterloo.flix.language.ast.Symbol.{DefnSym, LabelSym, VarSym}
import ca.uwaterloo.flix.language.ast.{LiftedAst, OccurrenceAst, Symbol}
import ca.uwaterloo.flix.util.Validation.ToSuccess
import ca.uwaterloo.flix.util.{ParOps, Validation}

import scala.collection.mutable

/**
 * The occurrence analyzer collects information on variable and function usage and calculates the weight of the expressions
 * Marks a variable or function as Dead if it is not used, Once if it is used exactly once and Many otherwise
 */
object OccurrenceAnalyzer {

  case class OccurInfo(varOccurrence: Map[VarSym, OccurrenceAst.Occur], defOccurrence: Map[DefnSym, OccurrenceAst.Occur], codeSize: Int)

  private val emptyOccur: OccurInfo = OccurInfo(Map.empty, Map.empty, 0)

  private val baseOccur: OccurInfo = OccurInfo(Map.empty, Map.empty, 1)

  /**
   * Performs occurrence analysis on the given AST `root`.
   */
  def run(root: LiftedAst.Root)(implicit flix: Flix): Validation[OccurrenceAst.Root, CompilationMessage] = flix.subphase("OccurrenceAnalyzer") {

    // Visit every definition in the program in parallel and transform to type 'OccurrenceAst.Def'
    val defs = visitDefs(root.defs)

    // Visit every enum in the program and transform to type 'OccurrenceAst.Enum'
    val enums = root.enums.map { case (k, v) => k -> visitEnum(v) }

    // Reassemble the ast root.
    val result = OccurrenceAst.Root(defs, enums, root.entryPoint, root.reachable, root.sources)

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
    OccurrenceAst.Enum(enum0.ann, enum0.mod, enum0.sym, cases, enum0.tpeDeprecated, enum0.loc)
  }

  /**
   * Translates the given case `case0` to the OccurrenceAst.
   */
  private def visitCase(case0: LiftedAst.Case): OccurrenceAst.Case = {
    OccurrenceAst.Case(case0.sym, case0.tag, case0.tpeDeprecated, case0.loc)
  }

  /**
   * Visits every definition in the program in parallel and perform occurrence analysis.
   * Set the occurrence of each `def` based on the occurrence found in `defOccur`
   */
  private def visitDefs(defs0: Map[DefnSym, LiftedAst.Def])(implicit flix: Flix): Map[DefnSym, OccurrenceAst.Def] = {
    val (d1, os) = ParOps.parMap(defs0.values)((d: LiftedAst.Def) => visitDef(d)).unzip

    // Combine all defOccurrences into 1 map
    val baseMap = collection.mutable.Map[DefnSym, Occur]()
    os.foreach(o => combineDefMaps(baseMap, o.defOccurrence))

    d1.map { kv =>
      val o = baseMap.getOrElse(kv.sym, Dead)
      val c = kv.context.copy(occur = o)
      val d2 = kv.copy(context = c)
      kv.sym -> d2
    }.toMap
  }

  /**
   * Visits a definition in the program and performs occurrence analysis
   */
  private def visitDef(defn: LiftedAst.Def): (OccurrenceAst.Def, OccurInfo) = {
    val fparams = defn.fparams.map(visitFormalParam)
    val (e, o) = visitExp(defn.exp)(defn.sym)
    /// Def consists of a single non-self function call.
    val isNonSelfCall = e match {
      case OccurrenceAst.Expression.ApplyDefTail(sym, _, _, _, _) =>
        sym != defn.sym
      case OccurrenceAst.Expression.ApplyCloTail(clo, _, _, _, _) =>
        clo match {
          case OccurrenceAst.Expression.Closure(sym, _, _, _) =>
            sym != defn.sym
          case _ => false
        }
      case _ => false

    }
    val defContext = DefContext(isNonSelfCall, o.defOccurrence.getOrElse(defn.sym, Dead), o.codeSize)
    (OccurrenceAst.Def(defn.ann, defn.mod, defn.sym, fparams, e, defContext, defn.tpe, defn.loc), o)
  }

  /**
   * Translates the given formal param `p` to the OccurrenceAst.
   */
  private def visitFormalParam(p: LiftedAst.FormalParam): OccurrenceAst.FormalParam =
    OccurrenceAst.FormalParam(p.sym, p.mod, p.tpe, p.loc)

  /**
   * Performs occurrence analysis on the given expression `exp0`
   */
  private def visitExp(exp0: LiftedAst.Expression)(implicit sym0: Symbol.DefnSym): (OccurrenceAst.Expression, OccurInfo) = exp0 match {
    case Expression.Unit(loc) => (OccurrenceAst.Expression.Unit(loc), baseOccur)

    case Expression.Null(tpe, loc) => (OccurrenceAst.Expression.Null(tpe, loc), baseOccur)

    case Expression.True(loc) => (OccurrenceAst.Expression.True(loc), baseOccur)

    case Expression.False(loc) => (OccurrenceAst.Expression.False(loc), baseOccur)

    case Expression.Char(lit, loc) => (OccurrenceAst.Expression.Char(lit, loc), baseOccur)

    case Expression.Float32(lit, loc) => (OccurrenceAst.Expression.Float32(lit, loc), baseOccur)

    case Expression.Float64(lit, loc) => (OccurrenceAst.Expression.Float64(lit, loc), baseOccur)

    case Expression.Int8(lit, loc) => (OccurrenceAst.Expression.Int8(lit, loc), baseOccur)

    case Expression.Int16(lit, loc) => (OccurrenceAst.Expression.Int16(lit, loc), baseOccur)

    case Expression.Int32(lit, loc) => (OccurrenceAst.Expression.Int32(lit, loc), baseOccur)

    case Expression.Int64(lit, loc) => (OccurrenceAst.Expression.Int64(lit, loc), baseOccur)

    case Expression.BigInt(lit, loc) => (OccurrenceAst.Expression.BigInt(lit, loc), baseOccur)

    case Expression.Str(lit, loc) => (OccurrenceAst.Expression.Str(lit, loc), baseOccur)

    case Expression.Var(sym, tpe, loc) => (OccurrenceAst.Expression.Var(sym, tpe, loc), OccurInfo(Map(sym -> Once), Map.empty, 1))

    case Expression.Closure(sym, freeVars, tpe, loc) =>
      val (fv, o) = freeVars.foldRight[(List[OccurrenceAst.FreeVar], Map[Symbol.VarSym, Occur])]((List.empty, Map.empty)) {
        case (LiftedAst.FreeVar(sym, tpe), (fvs, o)) =>
          (OccurrenceAst.FreeVar(sym, tpe) :: fvs, o + (sym -> DontInline))
      }
      (OccurrenceAst.Expression.Closure(sym, fv, tpe, loc), OccurInfo(o, Map.empty, 1 + fv.length))

    case Expression.ApplyClo(exp, args, tpe, purity, loc) =>
      val (e, o1) = visitExp(exp)
      val (as, o2) = visitExps(args)
      val o3 = combineAllSeq(o1, o2)
      exp match {
        case Expression.Closure(sym, _, _, _) =>
          val o4 = OccurInfo(Map.empty, Map(sym -> Once), 0)
          val o5 = combineAllSeq(o3, o4)
          (OccurrenceAst.Expression.ApplyClo(e, as, tpe, purity, loc), o5.copy(codeSize = o5.codeSize + 1))
        case _ => (OccurrenceAst.Expression.ApplyClo(e, as, tpe, purity, loc), o3.copy(codeSize = o3.codeSize + 1))
      }

    case Expression.ApplyDef(sym, args, tpe, purity, loc) =>
      val (as, o1) = visitExps(args)
      val o2 = OccurInfo(Map.empty, Map(sym -> Once), 0)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.ApplyDef(sym, as, tpe, purity, loc), o3.copy(codeSize = o3.codeSize + 1))

    case Expression.ApplyCloTail(exp, args, tpe, purity, loc) =>
      val (e, o1) = visitExp(exp)
      val (as, o2) = visitExps(args)
      val o3 = combineAllSeq(o1, o2)
      exp match {
        case Expression.Closure(sym, _, _, _) =>
          val o4 = OccurInfo(Map.empty, Map(sym -> Once), 0)
          val o5 = combineAllSeq(o3, o4)
          (OccurrenceAst.Expression.ApplyCloTail(e, as, tpe, purity, loc), o5.copy(codeSize = o5.codeSize + 1))
        case _ => (OccurrenceAst.Expression.ApplyCloTail(e, as, tpe, purity, loc), o3.copy(codeSize = o3.codeSize + 1))
      }

    case Expression.ApplyDefTail(sym, args, tpe, purity, loc) =>
      val (as, o1) = visitExps(args)
      val o2 = OccurInfo(Map.empty, Map(sym -> Once), 0)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.ApplyDefTail(sym, as, tpe, purity, loc), o3.copy(codeSize = o3.codeSize + 1))

    case Expression.ApplySelfTail(sym, formals, actuals, tpe, purity, loc) =>
      val (as, o1) = visitExps(actuals)
      val f = formals.map {
        case LiftedAst.FormalParam(sym, mod, tpe, loc) => OccurrenceAst.FormalParam(sym, mod, tpe, loc)
      }
      val o2 = OccurInfo(Map.empty, Map(sym -> Once), 0)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.ApplySelfTail(sym, f, as, tpe, purity, loc), o3.copy(codeSize = o3.codeSize + 1))

    case Expression.Unary(sop, op, exp, tpe, purity, loc) =>
      val (e, o) = visitExp(exp)
      (OccurrenceAst.Expression.Unary(sop, op, e, tpe, purity, loc), o.copy(codeSize = o.codeSize + 1))

    case Expression.Binary(sop, op, exp1, exp2, tpe, purity, loc) =>
      val (e1, o1) = visitExp(exp1)
      val (e2, o2) = visitExp(exp2)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.Binary(sop, op, e1, e2, tpe, purity, loc), o3.copy(codeSize = o3.codeSize + 1))

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val (e1, o1) = visitExp(exp1)
      val (e2, o2) = visitExp(exp2)
      val (e3, o3) = visitExp(exp3)
      val o4 = combineAllBranch(o1, combineAllBranch(o2, o3))
      (OccurrenceAst.Expression.IfThenElse(e1, e2, e3, tpe, purity, loc), o4.copy(codeSize = o4.codeSize + 1))

    case Expression.Branch(exp, branches, tpe, purity, loc) =>
      val (e1, o1) = visitExp(exp)
      val (o2, bs) = branches.foldLeft(emptyOccur, Map[LabelSym, OccurrenceAst.Expression]())((acc, b) => {
        val (oacc, bsacc) = acc
        b match {
          case (sym, exp1) =>
            val (e2, o3) = visitExp(exp1)
            val o4 = combineAllBranch(oacc, o3)
            val bs = bsacc + (sym -> e2)
            (o4, bs)
        }
      })
      val o5 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.Branch(e1, bs, tpe, purity, loc), o5.copy(codeSize = o5.codeSize + 1))

    case Expression.JumpTo(sym, tpe, purity, loc) => (OccurrenceAst.Expression.JumpTo(sym, tpe, purity, loc), baseOccur)

    case Expression.Let(sym, exp1, exp2, tpe, purity, loc) =>
      val (e1, o1) = visitExp(exp1)
      val (e2, o2) = visitExp(exp2)
      val o3 = combineAllSeq(o1, o2)
      val occur = o3.varOccurrence.getOrElse(sym, Dead)
      val o4 = o3.copy(varOccurrence = o3.varOccurrence - sym)
      (OccurrenceAst.Expression.Let(sym, e1, e2, occur, tpe, purity, loc), o4.copy(codeSize = o4.codeSize + 1))

    case Expression.LetRec(varSym, index, defSym, exp1, exp2, tpe, purity, loc) =>
      val (e1, o1) = visitExp(exp1)
      val (e2, o2) = visitExp(exp2)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.LetRec(varSym, index, defSym, e1, e2, tpe, purity, loc), o3.copy(codeSize = o3.codeSize + 1))

    case Expression.Is(sym, tag, exp, purity, loc) =>
      val (e, o) = visitExp(exp)
      if (sym.name == "Choice")
        (OccurrenceAst.Expression.Is(sym, tag, e, purity, loc), o.copy(defOccurrence = o.defOccurrence + (sym0 -> DontInline), codeSize = o.codeSize + 1))
      else
        (OccurrenceAst.Expression.Is(sym, tag, e, purity, loc), o.copy(codeSize = o.codeSize + 1))

    case Expression.Tag(sym, tag, exp, tpe, purity, loc) =>
      val (e, o) = visitExp(exp)
      (OccurrenceAst.Expression.Tag(sym, tag, e, tpe, purity, loc), o.copy(codeSize = o.codeSize + 1))

    case Expression.Untag(sym, tag, exp, tpe, purity, loc) =>
      val (e, o) = visitExp(exp)
      (OccurrenceAst.Expression.Untag(sym, tag, e, tpe, purity, loc), o.copy(codeSize = o.codeSize + 1))

    case Expression.Index(base, offset, tpe, purity, loc) =>
      val (b, o) = visitExp(base)
      (OccurrenceAst.Expression.Index(b, offset, tpe, purity, loc), o.copy(codeSize = o.codeSize + 1))

    case Expression.Tuple(elms, tpe, purity, loc) =>
      val (es, o) = visitExps(elms)
      (OccurrenceAst.Expression.Tuple(es, tpe, purity, loc), o.copy(codeSize = o.codeSize + 1))

    case Expression.RecordEmpty(tpe, loc) => (OccurrenceAst.Expression.RecordEmpty(tpe, loc), baseOccur)

    case Expression.RecordSelect(exp, field, tpe, purity, loc) =>
      val (e, o) = visitExp(exp)
      (OccurrenceAst.Expression.RecordSelect(e, field, tpe, purity, loc), o.copy(codeSize = o.codeSize + 1))

    case Expression.RecordExtend(field, value, rest, tpe, purity, loc) =>
      val (v, o1) = visitExp(value)
      val (r, o2) = visitExp(rest)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.RecordExtend(field, v, r, tpe, purity, loc), o3.copy(codeSize = o3.codeSize + 1))

    case Expression.RecordRestrict(field, rest, tpe, purity, loc) =>
      val (r, o) = visitExp(rest)
      (OccurrenceAst.Expression.RecordRestrict(field, r, tpe, purity, loc), o.copy(codeSize = o.codeSize + 1))

    case Expression.ArrayLit(elms, tpe, loc) =>
      val (es, o) = visitExps(elms)
      (OccurrenceAst.Expression.ArrayLit(es, tpe, loc), o.copy(codeSize = o.codeSize + 1))

    case Expression.ArrayNew(elm, len, tpe, loc) =>
      val (e, o1) = visitExp(elm)
      val (l, o2) = visitExp(len)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.ArrayNew(e, l, tpe, loc), o3.copy(codeSize = o3.codeSize + 1))

    case Expression.ArrayLoad(base, index, tpe, loc) =>
      val (b, o1) = visitExp(base)
      val (i, o2) = visitExp(index)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.ArrayLoad(b, i, tpe, loc), o3.copy(codeSize = o3.codeSize + 1))

    case Expression.ArrayStore(base, index, elm, tpe, loc) =>
      val (b, o1) = visitExp(base)
      val (i, o2) = visitExp(index)
      val (e, o3) = visitExp(elm)
      val o4 = combineAllSeq(o1, combineAllSeq(o2, o3))
      (OccurrenceAst.Expression.ArrayStore(b, i, e, tpe, loc), o4.copy(codeSize = o4.codeSize + 1))

    case Expression.ArrayLength(base, tpe, _, loc) =>
      val (b, o) = visitExp(base)
      val purity = b.purity
      (OccurrenceAst.Expression.ArrayLength(b, tpe, purity, loc), o.copy(codeSize = o.codeSize + 1))

    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
      val (b, o1) = visitExp(base)
      val (i1, o2) = visitExp(beginIndex)
      val (i2, o3) = visitExp(endIndex)
      val o4 = combineAllSeq(o1, combineAllSeq(o2, o3))
      (OccurrenceAst.Expression.ArraySlice(b, i1, i2, tpe, loc), o4.copy(codeSize = o4.codeSize + 1))

    case Expression.Ref(exp, tpe, loc) =>
      val (e, o) = visitExp(exp)
      (OccurrenceAst.Expression.Ref(e, tpe, loc), o.copy(codeSize = o.codeSize + 1))

    case Expression.Deref(exp, tpe, loc) =>
      val (e, o) = visitExp(exp)
      (OccurrenceAst.Expression.Deref(e, tpe, loc), o.copy(codeSize = o.codeSize + 1))

    case Expression.Assign(exp1, exp2, tpe, loc) =>
      val (e1, o1) = visitExp(exp1)
      val (e2, o2) = visitExp(exp2)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.Assign(e1, e2, tpe, loc), o3.copy(codeSize = o3.codeSize + 1))

    case Expression.Cast(exp, tpe, purity, loc) =>
      val (e, o) = visitExp(exp)
      (OccurrenceAst.Expression.Cast(e, tpe, purity, loc), o.copy(codeSize = o.codeSize + 1))

    case Expression.TryCatch(exp, rules, tpe, purity, loc) =>
      val (e, o1) = visitExp(exp)
      val (rs, o2) = rules.map {
        case LiftedAst.CatchRule(sym, clazz, exp) =>
          val (e, o3) = visitExp(exp)
          (OccurrenceAst.CatchRule(sym, clazz, e), o3)
      }.unzip
      val o4 = o2.foldLeft(o1)((acc, o5) => combineAllSeq(acc, o5))
      (OccurrenceAst.Expression.TryCatch(e, rs, tpe, purity, loc), o4.copy(defOccurrence = o4.defOccurrence + (sym0 -> DontInline), codeSize = o4.codeSize + 1))

    case Expression.InvokeConstructor(constructor, args, tpe, purity, loc) =>
      val (as, o) = visitExps(args)
      (OccurrenceAst.Expression.InvokeConstructor(constructor, as, tpe, purity, loc), o.copy(codeSize = o.codeSize + 1))

    case Expression.InvokeMethod(method, exp, args, tpe, purity, loc) =>
      val (e, o1) = visitExp(exp)
      val (as, o2) = visitExps(args)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.InvokeMethod(method, e, as, tpe, purity, loc), o3.copy(codeSize = o3.codeSize + 1))

    case Expression.InvokeStaticMethod(method, args, tpe, purity, loc) =>
      val (as, o) = visitExps(args)
      (OccurrenceAst.Expression.InvokeStaticMethod(method, as, tpe, purity, loc), o.copy(codeSize = o.codeSize + 1))

    case Expression.GetField(field, exp, tpe, purity, loc) =>
      val (e, o) = visitExp(exp)
      (OccurrenceAst.Expression.GetField(field, e, tpe, purity, loc), o.copy(codeSize = o.codeSize + 1))

    case Expression.PutField(field, exp1, exp2, tpe, purity, loc) =>
      val (e1, o1) = visitExp(exp1)
      val (e2, o2) = visitExp(exp2)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.PutField(field, e1, e2, tpe, purity, loc), o3.copy(codeSize = o3.codeSize + 1))

    case Expression.GetStaticField(field, tpe, purity, loc) =>
      (OccurrenceAst.Expression.GetStaticField(field, tpe, purity, loc), baseOccur)

    case Expression.PutStaticField(field, exp, tpe, purity, loc) =>
      val (e, o) = visitExp(exp)
      (OccurrenceAst.Expression.PutStaticField(field, e, tpe, purity, loc), o.copy(codeSize = o.codeSize + 1))

    case Expression.NewChannel(exp, tpe, loc) =>
      val (e, o) = visitExp(exp)
      (OccurrenceAst.Expression.NewChannel(e, tpe, loc), o.copy(codeSize = o.codeSize + 1))

    case Expression.GetChannel(exp, tpe, loc) =>
      val (e, o) = visitExp(exp)
      (OccurrenceAst.Expression.GetChannel(e, tpe, loc), o.copy(codeSize = o.codeSize + 1))

    case Expression.PutChannel(exp1, exp2, tpe, loc) =>
      val (e1, o1) = visitExp(exp1)
      val (e2, o2) = visitExp(exp2)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.PutChannel(e1, e2, tpe, loc), o3.copy(codeSize = o3.codeSize + 1))

    case Expression.SelectChannel(rules, default, tpe, loc) =>
      val (rs, o1, o2) = rules.map(r => {
        val (c, o3) = visitExp(r.chan)
        val (e, o4) = visitExp(r.exp)
        (OccurrenceAst.SelectChannelRule(r.sym, c, e), o3, o4)
      }).unzip3

      val o5 = o1.foldLeft(emptyOccur)((acc, o6) => combineAllSeq(acc, o6))
      val o7 = o2.foldLeft(emptyOccur)((acc, o8) => combineAllBranch(acc, o8))

      val (d1, o9) = default.map(visitExp).unzip
      val o10 = combineAllBranch(o9.getOrElse(emptyOccur), o7)

      val o11 = combineAllSeq(o5, o10)
      (OccurrenceAst.Expression.SelectChannel(rs, d1, tpe, loc), o11.copy(codeSize = o11.codeSize + 1))

    case Expression.Spawn(exp, tpe, loc) =>
      val (e, o1) = visitExp(exp)
      (OccurrenceAst.Expression.Spawn(e, tpe, loc), o1.copy(codeSize = o1.codeSize + 1))

    case Expression.Lazy(exp, tpe, loc) =>
      val (e, o1) = visitExp(exp)
      (OccurrenceAst.Expression.Lazy(e, tpe, loc), o1.copy(codeSize = o1.codeSize + 1))

    case Expression.Force(exp, tpe, loc) =>
      val (e, o1) = visitExp(exp)
      (OccurrenceAst.Expression.Force(e, tpe, loc), o1.copy(codeSize = o1.codeSize + 1))

    case Expression.HoleError(sym, tpe, loc) =>
      (OccurrenceAst.Expression.HoleError(sym, tpe, loc), baseOccur)

    case Expression.MatchError(tpe, loc) =>
      (OccurrenceAst.Expression.MatchError(tpe, loc), baseOccur)
  }

  /**
   * Performs occurrence analysis on a list of expressions 'exps' and merges occurrences
   */
  private def visitExps(exps: List[LiftedAst.Expression])(implicit sym0: Symbol.DefnSym): (List[OccurrenceAst.Expression], OccurInfo) = {
    val (es, o1) = exps.map(visitExp).unzip
    val o2 = o1.foldLeft(emptyOccur)((acc, o3) => combineAllSeq(acc, o3))
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
    val varMap = combineMaps(o1.varOccurrence, o2.varOccurrence, combine)
    val defMap = combineMaps(o1.defOccurrence, o2.defOccurrence, combine)
    val codeSize = o1.codeSize + o2.codeSize
    OccurInfo(varMap, defMap, codeSize)
  }

  /**
   * Combines the 2 maps `m1` and `m2` of the type (A -> Occur) into a single map of the same type using the argument `combine`.
   */
  private def combineMaps[A](m1: Map[A, Occur], m2: Map[A, Occur], combine: (Occur, Occur) => Occur): Map[A, Occur] = {
    (m1.keys ++ m2.keys).foldLeft[Map[A, Occur]](Map.empty) {
      case (acc, k) =>
        val occur = combine(m1.getOrElse(k, Dead), m2.getOrElse(k, Dead))
        acc + (k -> occur)
    }
  }

  /**
   * Combines the 2 maps `acc` and `m2` of the type (A -> Occur) into a single map.
   */
  private def combineDefMaps[A](acc: mutable.Map[A, Occur], m: Map[A, Occur]): Unit = {
    m.foreach {
      case (k, v) =>
        val o = combineSeq(acc.getOrElse(k, Dead), v)
        acc += (k -> o)
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


