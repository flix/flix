package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.LiftedAst.Expression
import ca.uwaterloo.flix.language.ast.{LiftedAst, OccurrenceAst, Symbol}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation.ToSuccess
import ca.uwaterloo.flix.language.ast.OccurrenceAst.Occur._
import ca.uwaterloo.flix.language.ast.OccurrenceAst.Occur

object OccurrenceAnalyzer {


  def run(root: LiftedAst.Root)(implicit flix: Flix): Validation[OccurrenceAst.Root, CompilationMessage] = flix.phase("OccurrenceAnalyzer") {
    // TODO: Implement occurrence analysis.
    val defs = root.defs.map {
      case (sym, defn) =>
        val fparams = defn.fparams.map { case LiftedAst.FormalParam(sym, mod, tpe, loc) => OccurrenceAst.FormalParam(sym, mod, tpe, loc) }
        val (e, _) = visitExp(defn.exp)
        sym -> OccurrenceAst.Def(defn.ann, defn.mod, defn.sym, fparams, e, defn.tpe, defn.loc)
    }
    val enums = root.enums.map {
      case (sym, enum) =>
        val cases = enum.cases.map {
          case (tag, caze) =>
            tag -> OccurrenceAst.Case(caze.sym, tag, caze.tpeDeprecated, caze.loc)
        }
        sym -> OccurrenceAst.Enum(enum.mod, enum.sym, cases, enum.tpeDeprecated, enum.loc)
    }
    // Reassemble the ast root.
    val result = OccurrenceAst.Root(defs, enums, root.reachable, root.sources)
    return result.toSuccess
  }

  def visitExp(exp0: LiftedAst.Expression): (OccurrenceAst.Expression, Map[Symbol.VarSym, OccurrenceAst.Occur]) = exp0 match {
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
      val freeVars1 = freeVars.map {
        case LiftedAst.FreeVar(sym, tpe) => OccurrenceAst.FreeVar(sym, tpe)
      }
      (OccurrenceAst.Expression.Closure(sym, freeVars1, tpe, loc), Map.empty)

    case Expression.ApplyClo(exp, args, tpe, loc) =>
      val (e, o1) = visitExp(exp)
      val (as, o2) = visitExps(args)
      val m = merge(o1, o2)
      (OccurrenceAst.Expression.ApplyClo(e, as, tpe, loc), m)

    case Expression.ApplyDef(sym, args, tpe, loc) =>
      val (as, o1) = visitExps(args)
      (OccurrenceAst.Expression.ApplyDef(sym, as, tpe, loc), o1)

    case Expression.ApplyCloTail(exp, args, tpe, loc) =>
      val (e, o1) = visitExp(exp)
      val (as, o2) = visitExps(args)
      val o3 = merge(o1, o2)
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
      val o3 = merge(o1, o2)
      (OccurrenceAst.Expression.Binary(sop, op, e1, e2, tpe, loc), o3)

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      val (e1, o1) = visitExp(exp1)
      val (e2, o2) = visitExp(exp2)
      val (e3, o3) = visitExp(exp3)
      val o4 = merge(o1, merge(o2, o3))
      (OccurrenceAst.Expression.IfThenElse(e1, e2, e3, tpe, loc), o4)

    case Expression.Branch(exp, branches, tpe, loc) =>
      var (e1, o1) = visitExp(exp)
      var bs = Map[Symbol.LabelSym, OccurrenceAst.Expression]()
      for ((sym, exp1) <- branches) {
        val (e2, o2) = visitExp(exp1)
        o1 = merge(o1, o2)
        bs += (sym -> e2)
      }
      (OccurrenceAst.Expression.Branch(e1, bs, tpe, loc), o1)

    case Expression.JumpTo(sym, tpe, loc) => (OccurrenceAst.Expression.JumpTo(sym, tpe, loc), Map.empty)

    case Expression.Let(sym, exp1, exp2, tpe, loc) =>
      val (e1, o1) = visitExp(exp1)
      val (e2, o2) = visitExp(exp2)
      val o3 = merge(o1, o2)
      val occur = o3.get(sym) match {
        case Some(occ) => occ
        case None => Dead
      }
      (OccurrenceAst.Expression.Let(sym, e1, e2, occur, tpe, loc), o3)

    case Expression.LetRec(varSym, index, defSym, exp1, exp2, tpe, loc) =>
      val (e1, o1) = visitExp(exp1)
      val (e2, o2) = visitExp(exp2)
      val o3 = merge(o1, o2)
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
      val o3 = merge(o1, o2)
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
      val o3 = merge(o1, o2)
      (OccurrenceAst.Expression.ArrayNew(e, l, tpe, loc), o3)

    case Expression.ArrayLoad(base, index, tpe, loc) =>
      val (b, o1) = visitExp(base)
      val (i, o2) = visitExp(base)
      val o3 = merge(o1, o2)
      (OccurrenceAst.Expression.ArrayLoad(b, i, tpe, loc), o3)

    case Expression.ArrayStore(base, index, elm, tpe, loc) =>
      val (b, o1) = visitExp(base)
      val (i, o2) = visitExp(base)
      val (e, o3) = visitExp(elm)
      val o4 = merge(o1, merge(o2, o3))
      (OccurrenceAst.Expression.ArrayStore(b, i, e, tpe, loc), o4)

    case Expression.ArrayLength(base, tpe, loc) =>
      val (b, o) = visitExp(base)
      (OccurrenceAst.Expression.ArrayLength(b, tpe, loc), o)

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
      for (r <- rules) {
        val (e, o2) = visitExp(r.exp)
        rs = rs :+ OccurrenceAst.CatchRule(r.sym, r.clazz, e)
        o1 = merge(o1, o2)
      }
      (OccurrenceAst.Expression.TryCatch(e, rs, tpe, loc), o1)

    case Expression.InvokeConstructor(constructor, args, tpe, loc) =>
      val (as, o) = visitExps(args)
      (OccurrenceAst.Expression.InvokeConstructor(constructor, as, tpe, loc), o)

    case Expression.InvokeMethod(method, exp, args, tpe, loc) =>
      val (e, o1) = visitExp(exp)
      val (as, o2) = visitExps(args)
      val o3 = merge(o1, o2)
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
      val o3 = merge(o1, o2)
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
      val o3 = merge(o1, o2)
      (OccurrenceAst.Expression.PutChannel(e1, e2, tpe, loc), o3)

    case Expression.SelectChannel(rules, default, tpe, loc) =>
      var rs: List[OccurrenceAst.SelectChannelRule] = List.empty
      var o1: Map[Symbol.VarSym, Occur] = Map.empty
      for (r <- rules) {
        val (c, o2) = visitExp(r.chan)
        val (e, o3) = visitExp(r.exp)
        rs = rs :+ OccurrenceAst.SelectChannelRule(r.sym, c, e)
        o1 = merge(o1, merge(o2, o3))
      }
      val (d, o4) = default match {
        case Some(exp) =>
          val (d, o5) = visitExp(exp)
          (Some(d), merge(o5, o1))
        case None => (None, o1)
      }
      (OccurrenceAst.Expression.SelectChannel(rs, d, tpe, loc), o4)

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

  def visitExps(args: List[LiftedAst.Expression]): (List[OccurrenceAst.Expression], Map[Symbol.VarSym, Occur]) = {
    args.foldLeft((List[OccurrenceAst.Expression](), Map[Symbol.VarSym, OccurrenceAst.Occur]()))((acc, arg) => {
      val (e, o) = visitExp(arg)
      val m = merge(o, acc._2)
      (acc._1 :+ e, m)
    })
  }

  def merge(m1: Map[Symbol.VarSym, Occur], m2: Map[Symbol.VarSym, Occur]): Map[Symbol.VarSym, Occur] = {
    var m3 = m1.map {
      case (sym, o1) =>
        val opt = m2.get(sym)
        opt match {
          case Some(o2) => (sym, combine(o1, o2))
          case None => (sym, o1)
        }
    }

    def combine(o1: Occur, o2: Occur): Occur = (o1, o2) match {
      case (Dead, _) => o2
      case (_, Dead) => o1
      case _ => Many
    }

    m2.foreach {
      case (sym, o) => if (!m1.contains(sym)) m3 += (sym -> o)
    }
    m3
  }
}


