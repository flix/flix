package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.LiftedAst.Expression
import ca.uwaterloo.flix.language.ast.{LiftedAst, OccurrenceAst}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation.ToSuccess

object OccurrenceAnalyzer {


  def run(root: LiftedAst.Root)(implicit flix: Flix): Validation[OccurrenceAst.Root, CompilationMessage] = flix.phase("OccurrenceAnalyzer") {
    // TODO: Implement occurrence analysis.
    val defs = root.defs.map {
      case (sym, defn) => sym -> defn.copy(exp = visitExp(defn.exp, Map.empty))
    }

    // Reassemble the ast root.
    val result = root.copy(defs = defs)

    return result.toSuccess
  }
  def visitExp(exp0: LiftedAst.Expression, occurences: Map[Symbol, OccurrenceAst.Occur]): (OccurrenceAst.Expression, Map[Symbol, OccurrenceAst.Occur]) = exp0 match {
    case Expression.Unit(loc) => (OccurrenceAst.Expression.Unit(loc), occurences)
    case Expression.Null(tpe, loc) => (OccurrenceAst.Expression.Null(tpe,loc), occurences)
    case Expression.True(loc) => (OccurrenceAst.Expression.True(loc), occurences)
    case Expression.False(loc) => (OccurrenceAst.Expression.False(loc), occurences)
    case Expression.Char(lit, loc) => (OccurrenceAst.Expression.Char(lit,loc), occurences)
    case Expression.Float32(lit, loc) => (OccurrenceAst.Expression.Float32(lit,loc), occurences)
    case Expression.Float64(lit, loc) => (OccurrenceAst.Expression.Float64(lit,loc), occurences)
    case Expression.Int8(lit, loc) => (OccurrenceAst.Expression.Int8(lit,loc), occurences)
    case Expression.Int16(lit, loc) => (OccurrenceAst.Expression.Int16(lit,loc), occurences)
    case Expression.Int32(lit, loc) => (OccurrenceAst.Expression.Int32(lit,loc), occurences)
    case Expression.Int64(lit, loc) => (OccurrenceAst.Expression.Int64(lit,loc), occurences)
    case Expression.BigInt(lit, loc) => (OccurrenceAst.Expression.BigInt(lit,loc), occurences)
    case Expression.Str(lit, loc) => (OccurrenceAst.Expression.Str(lit,loc), occurences)
    case Expression.Var(sym, tpe, loc) => ???
    case Expression.Closure(sym, freeVars, tpe, loc) =>
      val freeVars1 = freeVars.map((sym, tpe) -> OccurrenceAst.FreeVar(sym, tpe))
      (OccurrenceAst.Expression.Closure(sym, freeVars1, tpe, loc), occurences)
    case Expression.ApplyClo(exp, args, tpe, loc) => ???
    case Expression.ApplyDef(sym, args, tpe, loc) => ???
    case Expression.ApplyCloTail(exp, args, tpe, loc) => ???
    case Expression.ApplyDefTail(sym, args, tpe, loc) => ???
    case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) => ???
    case Expression.Unary(sop, op, exp, tpe, loc) => ???
    case Expression.Binary(sop, op, exp1, exp2, tpe, loc) => ???
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      val (e1, occurences1) = visitExp(exp1, occurences)
      val (e2, occurences2) = visitExp(exp2, occurences)
      merge(occurences1, occurence2)

      val (e1, occurences1) = visitExp(exp1, occurences)
      val (e2, occurences2) = visitExp(exp2, occurences1)

    case Expression.Branch(exp, branches, tpe, loc) => ???
    case Expression.JumpTo(sym, tpe, loc) => ???
    case Expression.Let(sym, exp1, exp2, tpe, loc) => ???
    case Expression.LetRec(varSym, index, defSym, exp1, exp2, tpe, loc) => ???
    case Expression.Is(sym, tag, exp, loc) => ???
    case Expression.Tag(sym, tag, exp, tpe, loc) => ???
    case Expression.Untag(sym, tag, exp, tpe, loc) => ???
    case Expression.Index(base, offset, tpe, loc) => ???
    case Expression.Tuple(elms, tpe, loc) => ???
    case Expression.RecordEmpty(tpe, loc) => ???
    case Expression.RecordSelect(exp, field, tpe, loc) => ???
    case Expression.RecordExtend(field, value, rest, tpe, loc) => ???
    case Expression.RecordRestrict(field, rest, tpe, loc) => ???
    case Expression.ArrayLit(elms, tpe, loc) => ???
    case Expression.ArrayNew(elm, len, tpe, loc) => ???
    case Expression.ArrayLoad(base, index, tpe, loc) => ???
    case Expression.ArrayStore(base, index, elm, tpe, loc) => ???
    case Expression.ArrayLength(base, tpe, loc) => ???
    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) => ???
    case Expression.Ref(exp, tpe, loc) => ???
    case Expression.Deref(exp, tpe, loc) => ???
    case Expression.Assign(exp1, exp2, tpe, loc) => ???
    case Expression.Cast(exp, tpe, loc) => ???
    case Expression.TryCatch(exp, rules, tpe, loc) => ???
    case Expression.InvokeConstructor(constructor, args, tpe, loc) => ???
    case Expression.InvokeMethod(method, exp, args, tpe, loc) => ???
    case Expression.InvokeStaticMethod(method, args, tpe, loc) => ???
    case Expression.GetField(field, exp, tpe, loc) => ???
    case Expression.PutField(field, exp1, exp2, tpe, loc) => ???
    case Expression.GetStaticField(field, tpe, loc) => ???
    case Expression.PutStaticField(field, exp, tpe, loc) => ???
    case Expression.NewChannel(exp, tpe, loc) => ???
    case Expression.GetChannel(exp, tpe, loc) => ???
    case Expression.PutChannel(exp1, exp2, tpe, loc) => ???
    case Expression.SelectChannel(rules, default, tpe, loc) =>
      val rs = rules.map {
      case LiftedAst.SelectChannelRule(sym, chan, exp) =>
        val (c, occurrences1) = visitExp(chan, occurences)
        val e = visitExp(exp, subst0)
        LiftedAst.SelectChannelRule(sym, c, e)
    }
      ???
    case Expression.Spawn(exp, tpe, loc) =>
      val (e, occurences1) = visitExp(exp, occurences)
      (OccurrenceAst.Expression.Spawn(e, tpe, loc), occurences1)
    case Expression.Lazy(exp, tpe, loc) =>
      val (e, occurences1) = visitExp(exp, occurences)
      (OccurrenceAst.Expression.Lazy(e, tpe, loc), occurences1)
    case Expression.Force(exp, tpe, loc) =>
      val (e, occurences1) = visitExp(exp, occurences)
      (OccurrenceAst.Expression.Force(e, tpe, loc),occurences1)
    case Expression.HoleError(sym, tpe, loc) => (OccurrenceAst.Expression.HoleError(sym, tpe, loc), occurences)
    case Expression.MatchError(tpe, loc) => (OccurrenceAst.Expression.MatchError(tpe, loc), occurences)
  }

/*
  def merge(m1: Map[Symbol, OccurrenceAst.Occur], m2: Map[Symbol, OccurrenceAst.Occur]): Map[Symbol, OccurrenceAst.Occur] {
    val m3 = m1.map {
    case (sym, Once) => ???
    case (sym, Many) => ???
    case (sym, Dead) => ???
    case (sym, OnceInLam) => ???
    case (sym, ManyBranch) => ???
  }
  m3
  }
*/

}


