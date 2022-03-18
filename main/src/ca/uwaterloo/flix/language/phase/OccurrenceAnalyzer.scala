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
import ca.uwaterloo.flix.language.ast.OccurrenceAst.Occur
import ca.uwaterloo.flix.language.ast.OccurrenceAst.Occur._
import ca.uwaterloo.flix.language.ast.Symbol.VarSym
import ca.uwaterloo.flix.language.ast.{LiftedAst, OccurrenceAst, Symbol}
import ca.uwaterloo.flix.util.Validation.ToSuccess
import ca.uwaterloo.flix.util.{ParOps, Validation}

/**
 * The occurrence analyzer collects information on variable usage
 * Marks a variable as Dead if it is not used, Once if it is used exactly once and Many otherwise
 */
object OccurrenceAnalyzer {

  /**
   * Performs occurrence analysis on the given AST `root`.
   */
  def run(root: LiftedAst.Root)(implicit flix: Flix): Validation[OccurrenceAst.Root, CompilationMessage] = flix.subphase("OccurrenceAnalyzer") {

    // Visit every definition in the program in parallel and transform to type 'OccurrenceAst.Def'
    val defs = ParOps.parMap(root.defs.values)((d: LiftedAst.Def) => visitDef(d))
    val newDefs = defs.map(kv => kv.sym -> kv).toMap

    // Visit every enum in the program and transform to type 'OccurrenceAst.Enum'
    val enums = root.enums.map {
      case (sym, enum) =>
        val cases = enum.cases.map {
          case (tag, caze) =>
            tag -> OccurrenceAst.Case(caze.sym, tag, caze.tpeDeprecated, caze.loc)
        }
        sym -> OccurrenceAst.Enum(enum.mod, enum.sym, cases, enum.tpeDeprecated, enum.loc)
    }

    // Reassemble the ast root.
    val result = OccurrenceAst.Root(newDefs, enums, root.reachable, root.sources)

    result.toSuccess
  }

  /**
   * Visits a definition in the program and performs occurrence analysis
   */
  private def visitDef(defn: LiftedAst.Def): OccurrenceAst.Def = {
    val fparams = defn.fparams.map { case LiftedAst.FormalParam(sym, mod, tpe, loc) => OccurrenceAst.FormalParam(sym, mod, tpe, loc) }
    val (e, _) = visitExp(defn.exp)
    OccurrenceAst.Def(defn.ann, defn.mod, defn.sym, fparams, e, defn.tpe, defn.loc)
  }

  /**
   * Performs occurrence analysis on the given expression `exp0`
   */
  private def visitExp(exp0: LiftedAst.Expression): (OccurrenceAst.Expression, Map[Symbol.VarSym, OccurrenceAst.Occur]) = exp0 match {
    case Expression.Unit(loc) => (OccurrenceAst.Expression.Unit(loc), Map.empty)

    case Expression.Null(tpe, loc) => (OccurrenceAst.Expression.Null(tpe, loc), Map.empty)

    case Expression.True(loc) => (OccurrenceAst.Expression.True(loc), Map.empty)

    case Expression.False(loc) => (OccurrenceAst.Expression.False(loc), Map.empty)

    case Expression.Char(lit, loc) => (OccurrenceAst.Expression.Char(lit, loc), Map.empty)

    case Expression.Float32(lit, loc) => (OccurrenceAst.Expression.Float32(lit, loc), Map.empty)

    case Expression.Float64(lit, loc) => (OccurrenceAst.Expression.Float64(lit, loc), Map.empty)

    case Expression.Int8(lit, loc) => (OccurrenceAst.Expression.Int8(lit, loc), Map.empty)

    case Expression.Int16(lit, loc) => (OccurrenceAst.Expression.Int16(lit, loc), Map.empty)

    case Expression.Int32(lit, loc) => (OccurrenceAst.Expression.Int32(lit, loc), Map.empty)

    case Expression.Int64(lit, loc) => (OccurrenceAst.Expression.Int64(lit, loc), Map.empty)

    case Expression.BigInt(lit, loc) => (OccurrenceAst.Expression.BigInt(lit, loc), Map.empty)

    case Expression.Str(lit, loc) => (OccurrenceAst.Expression.Str(lit, loc), Map.empty)

    case Expression.Var(sym, tpe, loc) => (OccurrenceAst.Expression.Var(sym, tpe, loc), Map(sym -> Once))

    case Expression.Closure(sym, freeVars, tpe, loc) =>
      val fv = freeVars.map {
        case LiftedAst.FreeVar(sym, tpe) => OccurrenceAst.FreeVar(sym, tpe)
      }
      (OccurrenceAst.Expression.Closure(sym, fv, tpe, loc), Map.empty)

    case Expression.ApplyClo(exp, args, tpe, loc) =>
      val (e, o1) = visitExp(exp)
      val (as, o2) = visitExps(args)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.ApplyClo(e, as, tpe, loc), o3)

    case Expression.ApplyDef(sym, args, tpe, loc) =>
      val (as, o1) = visitExps(args)
      (OccurrenceAst.Expression.ApplyDef(sym, as, tpe, loc), o1)

    case Expression.ApplyCloTail(exp, args, tpe, loc) =>
      val (e, o1) = visitExp(exp)
      val (as, o2) = visitExps(args)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.ApplyCloTail(e, as, tpe, loc), o3)

    case Expression.ApplyDefTail(sym, args, tpe, loc) =>
      val (as, o) = visitExps(args)
      (OccurrenceAst.Expression.ApplyDefTail(sym, as, tpe, loc), o)

    case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
      val (as, o) = visitExps(actuals)
      val f = formals.map {
        case LiftedAst.FormalParam(sym, mod, tpe, loc) => OccurrenceAst.FormalParam(sym, mod, tpe, loc)
      }
      (OccurrenceAst.Expression.ApplySelfTail(sym, f, as, tpe, loc), o)

    case Expression.Unary(sop, op, exp, tpe, loc) =>
      val (e, o) = visitExp(exp)
      (OccurrenceAst.Expression.Unary(sop, op, e, tpe, loc), o)

    case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
      val (e1, o1) = visitExp(exp1)
      val (e2, o2) = visitExp(exp2)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.Binary(sop, op, e1, e2, tpe, loc), o3)

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      val (e1, o1) = visitExp(exp1)
      val (e2, o2) = visitExp(exp2)
      val (e3, o3) = visitExp(exp3)
      val o4 = combineAllBranch(o1, combineAllBranch(o2, o3))
      (OccurrenceAst.Expression.IfThenElse(e1, e2, e3, tpe, loc), o4)

    case Expression.Branch(exp, branches, tpe, loc) =>
      val (e1, o1) = visitExp(exp)
      var (o2, bs) = branches.foldLeft(Map[VarSym, Occur](), Map[LabelSym, OccurrenceAst.Expression]())((acc, b) => b match {
        case (sym, exp1) =>
          val (e2, o3) = visitExp(exp1)
          val o4 = combineAllBranch(acc._1, o3)
          val bs = acc._2 + (sym -> e2)
          (o4, bs)
      })
      o2 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.Branch(e1, bs, tpe, loc), o2)

    case Expression.JumpTo(sym, tpe, loc) => (OccurrenceAst.Expression.JumpTo(sym, tpe, loc), Map.empty)

    case Expression.Let(sym, exp1, exp2, tpe, purity, loc) =>
      val (e1, o1) = visitExp(exp1)
      val (e2, o2) = visitExp(exp2)
      val o3 = combineAllSeq(o1, o2)
      val occur = o3.getOrElse(sym, Dead)
      val o4 = o3 - sym
      (OccurrenceAst.Expression.Let(sym, e1, e2, occur, tpe, purity, loc), o4)

    case Expression.LetRec(varSym, index, defSym, exp1, exp2, tpe, loc) =>
      val (e1, o1) = visitExp(exp1)
      val (e2, o2) = visitExp(exp2)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.LetRec(varSym, index, defSym, e1, e2, tpe, loc), o3)

    case Expression.Is(sym, tag, exp, loc) =>
      val (e, o) = visitExp(exp)
      (OccurrenceAst.Expression.Is(sym, tag, e, loc), o)

    case Expression.Tag(sym, tag, exp, tpe, loc) =>
      val (e, o) = visitExp(exp)
      (OccurrenceAst.Expression.Tag(sym, tag, e, tpe, loc), o)

    case Expression.Untag(sym, tag, exp, tpe, loc) =>
      val (e, o) = visitExp(exp)
      (OccurrenceAst.Expression.Untag(sym, tag, e, tpe, loc), o)

    case Expression.Index(base, offset, tpe, loc) =>
      val (b, o) = visitExp(base)
      (OccurrenceAst.Expression.Index(b, offset, tpe, loc), o)

    case Expression.Tuple(elms, tpe, loc) =>
      val (es, o) = visitExps(elms)
      (OccurrenceAst.Expression.Tuple(es, tpe, loc), o)

    case Expression.RecordEmpty(tpe, loc) => (OccurrenceAst.Expression.RecordEmpty(tpe, loc), Map.empty)

    case Expression.RecordSelect(exp, field, tpe, loc) =>
      val (e, o) = visitExp(exp)
      (OccurrenceAst.Expression.RecordSelect(e, field, tpe, loc), o)

    case Expression.RecordExtend(field, value, rest, tpe, loc) =>
      val (v, o1) = visitExp(value)
      val (r, o2) = visitExp(rest)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.RecordExtend(field, v, r, tpe, loc), o3)

    case Expression.RecordRestrict(field, rest, tpe, loc) =>
      val (r, o) = visitExp(rest)
      (OccurrenceAst.Expression.RecordRestrict(field, r, tpe, loc), o)

    case Expression.ArrayLit(elms, tpe, loc) =>
      val (es, o) = visitExps(elms)
      (OccurrenceAst.Expression.ArrayLit(es, tpe, loc), o)

    case Expression.ArrayNew(elm, len, tpe, loc) =>
      val (e, o1) = visitExp(elm)
      val (l, o2) = visitExp(len)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.ArrayNew(e, l, tpe, loc), o3)

    case Expression.ArrayLoad(base, index, tpe, loc) =>
      val (b, o1) = visitExp(base)
      val (i, o2) = visitExp(index)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.ArrayLoad(b, i, tpe, loc), o3)

    case Expression.ArrayStore(base, index, elm, tpe, loc) =>
      val (b, o1) = visitExp(base)
      val (i, o2) = visitExp(index)
      val (e, o3) = visitExp(elm)
      val o4 = combineAllSeq(o1, combineAllSeq(o2, o3))
      (OccurrenceAst.Expression.ArrayStore(b, i, e, tpe, loc), o4)

    case Expression.ArrayLength(base, tpe, loc) =>
      val (b, o) = visitExp(base)
      (OccurrenceAst.Expression.ArrayLength(b, tpe, loc), o)

    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
      val (b, o1) = visitExp(base)
      val (i1, o2) = visitExp(beginIndex)
      val (i2, o3) = visitExp(endIndex)
      val o4 = combineAllSeq(o1, combineAllSeq(o2, o3))
      (OccurrenceAst.Expression.ArraySlice(b, i1, i2, tpe, loc), o4)

    case Expression.Ref(exp, tpe, loc) =>
      val (e, o) = visitExp(exp)
      (OccurrenceAst.Expression.Ref(e, tpe, loc), o)

    case Expression.Deref(exp, tpe, loc) =>
      val (e, o) = visitExp(exp)
      (OccurrenceAst.Expression.Deref(e, tpe, loc), o)

    case Expression.Assign(exp1, exp2, tpe, loc) =>
      val (e1, o1) = visitExp(exp1)
      val (e2, o2) = visitExp(exp2)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.Assign(e1, e2, tpe, loc), o3)

    case Expression.Cast(exp, tpe, loc) =>
      val (e, o) = visitExp(exp)
      (OccurrenceAst.Expression.Cast(e, tpe, loc), o)

    case Expression.TryCatch(exp, rules, tpe, loc) =>
      val (e, o1) = visitExp(exp)
      val (rs, o2) = rules.foldRight(List[OccurrenceAst.CatchRule](), o1)((r, acc) => {
        val (e, o3) = visitExp(r.exp)
        val o4 = combineAllSeq(acc._2, o3)
        val rs = OccurrenceAst.CatchRule(r.sym, r.clazz, e) :: acc._1
        (rs, o4)
      })
      (OccurrenceAst.Expression.TryCatch(e, rs, tpe, loc), o2)

    case Expression.InvokeConstructor(constructor, args, tpe, loc) =>
      val (as, o) = visitExps(args)
      (OccurrenceAst.Expression.InvokeConstructor(constructor, as, tpe, loc), o)

    case Expression.InvokeMethod(method, exp, args, tpe, loc) =>
      val (e, o1) = visitExp(exp)
      val (as, o2) = visitExps(args)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.InvokeMethod(method, e, as, tpe, loc), o3)

    case Expression.InvokeStaticMethod(method, args, tpe, loc) =>
      val (as, o) = visitExps(args)
      (OccurrenceAst.Expression.InvokeStaticMethod(method, as, tpe, loc), o)

    case Expression.GetField(field, exp, tpe, loc) =>
      val (e, o) = visitExp(exp)
      (OccurrenceAst.Expression.GetField(field, e, tpe, loc), o)

    case Expression.PutField(field, exp1, exp2, tpe, loc) =>
      val (e1, o1) = visitExp(exp1)
      val (e2, o2) = visitExp(exp2)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.PutField(field, e1, e2, tpe, loc), o3)

    case Expression.GetStaticField(field, tpe, loc) =>
      (OccurrenceAst.Expression.GetStaticField(field, tpe, loc), Map.empty)

    case Expression.PutStaticField(field, exp, tpe, loc) =>
      val (e, o) = visitExp(exp)
      (OccurrenceAst.Expression.PutStaticField(field, e, tpe, loc), o)

    case Expression.NewChannel(exp, tpe, loc) =>
      val (e, o) = visitExp(exp)
      (OccurrenceAst.Expression.NewChannel(e, tpe, loc), o)

    case Expression.GetChannel(exp, tpe, loc) =>
      val (e, o) = visitExp(exp)
      (OccurrenceAst.Expression.GetChannel(e, tpe, loc), o)

    case Expression.PutChannel(exp1, exp2, tpe, loc) =>
      val (e1, o1) = visitExp(exp1)
      val (e2, o2) = visitExp(exp2)
      val o3 = combineAllSeq(o1, o2)
      (OccurrenceAst.Expression.PutChannel(e1, e2, tpe, loc), o3)

    case Expression.SelectChannel(rules, default, tpe, loc) =>
      val (rs, o1, o2) = rules.map(r => {
        val (c, o3) = visitExp(r.chan)
        val (e, o4) = visitExp(r.exp)
        (OccurrenceAst.SelectChannelRule(r.sym, c, e), o3, o4)
      }).unzip3

      val o5 =  o1.foldLeft(Map[VarSym, Occur]())((acc, o6) => combineAllSeq(acc, o6))
      val o7 =  o2.foldLeft(Map[VarSym, Occur]())((acc, o8) => combineAllBranch(acc, o8))

      val (d1, o9) = default.map(visitExp).unzip
      val o8 = combineAllBranch(o9.getOrElse(Map.empty), o7)

      val o10 = combineAllSeq(o5, o8)
      (OccurrenceAst.Expression.SelectChannel(rs, d1, tpe, loc), o10)

    case Expression.Spawn(exp, tpe, loc) =>
      val (e, o1) = visitExp(exp)
      (OccurrenceAst.Expression.Spawn(e, tpe, loc), o1)

    case Expression.Lazy(exp, tpe, loc) =>
      val (e, o1) = visitExp(exp)
      (OccurrenceAst.Expression.Lazy(e, tpe, loc), o1)

    case Expression.Force(exp, tpe, loc) =>
      val (e, o1) = visitExp(exp)
      (OccurrenceAst.Expression.Force(e, tpe, loc), o1)

    case Expression.HoleError(sym, tpe, loc) =>
      (OccurrenceAst.Expression.HoleError(sym, tpe, loc), Map.empty)

    case Expression.MatchError(tpe, loc) =>
      (OccurrenceAst.Expression.MatchError(tpe, loc), Map.empty)
  }

  /**
   * Performs occurrence analysis on a list of expressions 'exps' and merges occurrences
   */
  private def visitExps(exps: List[LiftedAst.Expression]): (List[OccurrenceAst.Expression], Map[Symbol.VarSym, Occur]) = {
    val (es, o1) = exps.map(visitExp).unzip
    val o2 =  o1.foldLeft(Map[VarSym, Occur]())((acc, o3) => combineAllSeq(acc, o3))
    (es, o2)
  }

  /**
   * Combines the 2 maps `m1` and `m2` of the type (Symbol -> Occur) into a single map of same type using the function `combineBranches`.
   */
  private def combineAllBranch(m1: Map[VarSym, Occur], m2: Map[VarSym, Occur]): Map[VarSym, Occur] = {
    combineAll(m1, m2, combineBranch)
  }

  /**
   * Combines the 2 maps `m1` and `m2` of the type (Symbol -> Occur) into a single map of same type using the function `combine`.
   */
  private def combineAllSeq(m1: Map[VarSym, Occur], m2: Map[VarSym, Occur]): Map[VarSym, Occur] = {
    combineAll(m1, m2, combineSeq)
  }

  /**
   * Combines the 2 maps `m1` and `m2` of the type (Symbol -> Occur) into a single map of same type using the argument `combine`.
   */
  private def combineAll(m1: Map[VarSym, Occur], m2: Map[VarSym, Occur], combine: (Occur, Occur) => Occur): Map[VarSym, Occur] = {
    (m1.keys ++ m2.keys).foldLeft[Map[VarSym, Occur]](Map.empty) {
      case (acc, k) =>
        val occur = combine(m1.getOrElse(k, Dead), m2.getOrElse(k, Dead))
        acc + (k -> occur)
    }
  }

  /**
   * Combines two occurrences `o1` and `o2` of type Occur into a single occurrence.
   */
  private def combineSeq(o1: Occur, o2: Occur): Occur = (o1, o2) match {
    case (Dead, _) => o2
    case (_, Dead) => o1
    case _ => Many
  }

  /**
   * Combines two occurrences `o1` and `o2` of type Occur into a single occurrence based on ManyBranches logic.
   * ManyBranches can be IfThenElse, Branches, and SelectChannel
   */
  private def combineBranch(o1: Occur, o2: Occur): Occur = (o1, o2) match {
    case (Dead, _) => o2
    case (_, Dead) => o1
    case (Once, Once) => ManyBranch
    case (Once, ManyBranch) => ManyBranch
    case (ManyBranch, Once) => ManyBranch
    case (ManyBranch, ManyBranch) => ManyBranch
    case _ => Many
  }
}


