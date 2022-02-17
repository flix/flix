package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.LiftedAst.Expression
import ca.uwaterloo.flix.language.ast.{LiftedAst, OccurrenceAst}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation.ToSuccess
import ca.uwaterloo.flix.language.ast.OccurrenceAst.Occur.{Dead, Once, OnceInLam, Many, ManyBranch}
import ca.uwaterloo.flix.language.ast.OccurrenceAst.Occur
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
  def visitExp(exp0: LiftedAst.Expression): (OccurrenceAst.Expression, Map[Symbol, OccurrenceAst.Occur]) = exp0 match {
    case Expression.Unit(loc) => (OccurrenceAst.Expression.Unit(loc), Map.empty)
    case Expression.Null(tpe, loc) => (OccurrenceAst.Expression.Null(tpe,loc), Map.empty)
    case Expression.True(loc) => (OccurrenceAst.Expression.True(loc), Map.empty)
    case Expression.False(loc) => (OccurrenceAst.Expression.False(loc), Map.empty)
    case Expression.Char(lit, loc) => (OccurrenceAst.Expression.Char(lit,loc), Map.empty)
    case Expression.Float32(lit, loc) => (OccurrenceAst.Expression.Float32(lit,loc), Map.empty)
    case Expression.Float64(lit, loc) => (OccurrenceAst.Expression.Float64(lit,loc), Map.empty)
    case Expression.Int8(lit, loc) => (OccurrenceAst.Expression.Int8(lit,loc), Map.empty)
    case Expression.Int16(lit, loc) => (OccurrenceAst.Expression.Int16(lit,loc), Map.empty)
    case Expression.Int32(lit, loc) => (OccurrenceAst.Expression.Int32(lit,loc), Map.empty)
    case Expression.Int64(lit, loc) => (OccurrenceAst.Expression.Int64(lit,loc), Map.empty)
    case Expression.BigInt(lit, loc) => (OccurrenceAst.Expression.BigInt(lit,loc), Map.empty)
    case Expression.Str(lit, loc) => (OccurrenceAst.Expression.Str(lit,loc), Map.empty)
    case Expression.Var(sym, tpe, loc) => ???
    case Expression.Closure(sym, freeVars, tpe, loc) =>
      val freeVars1 = freeVars.map {
        case LiftedAst.FreeVar(sym, tpe) => OccurrenceAst.FreeVar(sym, tpe)
      }
      (OccurrenceAst.Expression.Closure(sym, freeVars1, tpe, loc), Map.empty)

    case Expression.ApplyClo(exp, args, tpe, loc) =>
      val (e, o1) = visitExp(exp)
      val (as, o2) = visitArgs(args)val m = merge(o1, o2)
      (OccurrenceAst.Expression.ApplyClo(e, as, tpe, loc), m)

    case Expression.ApplyDef(sym, args, tpe, loc) =>
      val (as, o1) = visitArgs(args)
      (OccurrenceAst.Expression.ApplyDef(sym, as, tpe, loc), o1)

    case Expression.ApplyCloTail(exp, args, tpe, loc) =>
      val (e, o1) = visitExp(exp)
      val (as, o2) = visitArgs(args)
      val o3 = merge(o1, o2)
      (OccurrenceAst.Expression.ApplyCloTail(e, as, tpe, loc), o3)

    case Expression.ApplyDefTail(sym, args, tpe, loc) =>
      val (as, o) = visitArgs(args)
      (OccurrenceAst.Expression.ApplyDefTail(sym, as, tpe, loc), o)

    case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
      val (as, o) = visitArgs(actuals)
      val f = formals.map {
        case LiftedAst.FormalParam(sym, mod, tpe, loc) => OccurrenceAst.FormalParam(sym, mod, tpe, loc)
      }
      (OccurrenceAst.Expression.ApplySelfTail(sym, f, as, tpe, loc), o)
    case Expression.Unary(sop, op, exp, tpe, loc) => ???
    case Expression.Binary(sop, op, exp1, exp2, tpe, loc) => ???
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      val (e1, o1) = visitExp(exp1)
      val (e2, o2) = visitExp(exp2)
      val (e3, o3) = visitExp(exp3)
      val o4 = merge(o1, merge(o2,o3))
      (OccurrenceAst.Expression.IfThenElse(e1,e2,e3,tpe,loc),o4)

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
    case Expression.ArrayLength(base, tpe, loc) =>
      val (b, o) = visitExp(base)
      (OccurrenceAst.Expression.ArrayLenght(b, tpe, loc), o)
    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
      val (b, o1) = visitExp(base)
      val (i1, o2) = visitExp(beginIndex)
      val (i2, o3) = visitExp(endIndex)
      val o4 = merge(o1, merge(o2, o3))
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
      val o3 = merge(o1, o2)
      (OccurrenceAst.Expression.Assign(e1, e2, tpe, loc), o3)
    case Expression.Cast(exp, tpe, loc) =>
      val (e, o) = visitExp(exp)
      (OccurrenceAst.Expression.Cast(e, tpe, loc), o)
    case Expression.TryCatch(exp, rules, tpe, loc) =>
      var (e, o1) = visitExp(exp)
      var rs: List[OccurrenceAst.CatchRule] = List.empty
      for (r <- rules ) {
        val (e, o2) = visitExp(r.exp)
        rs =  rs :+ OccurrenceAst.CatchRule(r.sym, r.clazz, e)
        o1 = merge(o1, o2)
      }
    case Expression.InvokeConstructor(constructor, args, tpe, loc) =>
      val (as, o) = visitArgs(args)
      (OccurrenceAst.Expression.InvokeConstructor(constructor, as, tpe, loc), o)
    case Expression.InvokeMethod(method, exp, args, tpe, loc) =>
      val (e, o1) = visitExp(exp)
      val (as, o2) = visitArgs(args)
      val o3 = merge(o1, o2)
      (OccurrenceAst.Expression.InvokeMethod(method, e, as, tpe, loc), o3)
    case Expression.InvokeStaticMethod(method, args, tpe, loc) =>
      val (as, o) = visitArgs(args)
      (OccurrenceAst.Expression.InvokeStaticMethod(method, as, tpe, loc),o)
    case Expression.GetField(field, exp, tpe, loc) =>
      val (e, o) = visitExp(exp)
      (OccurrenceAst.Expression.GetField(field, e, tpe, loc), o)
    case Expression.PutField(field, exp1, exp2, tpe, loc) =>
      val (e1, o1) = visitExp(exp1)
      val (e2, o2) = visitExp(exp2)
      val o3 = merge(o1,o2)
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
      (OccurrenceAst.Expression.GetChannel(e,tpe,loc), o)
    case Expression.PutChannel(exp1, exp2, tpe, loc) =>
      val (e1, o1) = visitExp(exp1)
      val (e2, o2) = visitExp(exp2)
      val o3 = merge(o1,o2)
      (OccurrenceAst.Expression.PutChannel(e1,e2,tpe,loc), o3)
    case Expression.SelectChannel(rules, default, tpe, loc) =>
      var rs: List[OccurrenceAst.SelectChannelRule] = List.empty
      var o1: Map[Symbol, Occur] = Map.empty
      for (r <- rules ) {
        val (c, o2) = visitExp(r.chan)
        val (e, o3) = visitExp(r.exp)
        rs =  rs :+ OccurrenceAst.SelectChannelRule(r.sym, c, e)
        o1 = merge(o1, merge(o2,o3))
      }
      val (d, o4) = default match {
        case Some(exp) =>
          val (d, o5) = visitExp(exp)
          (Some (d), merge(o5, o1))
        case None => (None, o1)
      }
      (OccurrenceAst.Expression.SelectChannel(rs, d, tpe, loc),o4)
    case Expression.Spawn(exp, tpe, loc) =>
      val (e, o1) = visitExp(exp)
      (OccurrenceAst.Expression.Spawn(e, tpe, loc), o1)
    case Expression.Lazy(exp, tpe, loc) =>
      val (e, o1) = visitExp(exp)
      (OccurrenceAst.Expression.Lazy(e, tpe, loc), o1)
    case Expression.Force(exp, tpe, loc) =>
      val (e, o1) = visitExp(exp)
      (OccurrenceAst.Expression.Force(e, tpe, loc),o1)
    case Expression.HoleError(sym, tpe, loc) => (OccurrenceAst.Expression.HoleError(sym, tpe, loc), Map.empty)
    case Expression.MatchError(tpe, loc) => (OccurrenceAst.Expression.MatchError(tpe, loc), Map.empty)
  }

  def visitArgs(args: List[LiftedAst.Expression]) :(List[OccurrenceAst.Expression],Map[Symbol, Occur]) = {
    args.foldLeft((List[OccurrenceAst.Expression](), Map[Symbol, OccurrenceAst.Occur]()))((acc,arg) => {
      val (e, o) = visitExp(arg)
      val m = merge(o, acc._2)
      (acc._1 :+e, m)
    })
    // TODO Anna: reverse listen med argumenter efter fold. Hvordan gøres det bedst?
  }

  def merge(m1: Map[Symbol, OccurrenceAst.Occur], m2: Map[Symbol, OccurrenceAst.Occur]): Map[Symbol, OccurrenceAst.Occur] = {
    val m3 = m1.map {
      case (sym, occur) => (sym, mergeAux(occur, m2.apply(sym)))
    }

    def mergeAux(o1: OccurrenceAst.Occur, o2: OccurrenceAst.Occur): OccurrenceAst.Occur = (o1,o2) match {
      case (Dead, Dead) => Dead
      case (Once, Once) => Many
      case (Dead, Once) => Once
      case (Once, Dead) => Once
      case (Many, _) => Many
      case (_, Many) => Many
      case (Once, OnceInLam) => Many
      case (OnceInLam, Once) => Many
      case (ManyBranch, OnceInLam) => Many
      case (OnceInLam, ManyBranch) => Many
      case (OnceInLam, OnceInLam) => Many
      case (ManyBranch, ManyBranch) => ManyBranch

      case (_, _) => o1 //sym not in m2
    }

    m2.foreach{
      case (sym, o) => if(!m1.contains(sym)) m3 += (sym -> o)
    }
    m3

  }


}


