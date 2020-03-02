package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.SimplifiedAst._
import ca.uwaterloo.flix.language.ast.{SimplifiedAst, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.debug.PrettyPrinter
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.vt.TerminalContext
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

object ControlFlowAnalysis extends Phase[SimplifiedAst.Root, SimplifiedAst.Root] {

  type AbstractCache = Map[LabeledExpression, Set[Expression.Closure]]
  type AbstractEnvironment = Map[Symbol.VarSym, Set[Expression.Closure]]

  def run(root: SimplifiedAst.Root)(implicit flix: Flix): Validation[SimplifiedAst.Root, CompilationError] = flix.phase("CFA") {
    var label = 0

    def labelExp(exp: Expression): LabeledExpression = {
      val thisLabel = label
      label = label + 1
      val lExp = exp match {
        case Expression.Unit => exp
        case Expression.True => exp
        case Expression.False => exp
        case Expression.Char(lit) => exp
        case Expression.Float32(lit) => exp
        case Expression.Float64(lit) => exp
        case Expression.Int8(lit) => exp
        case Expression.Int16(lit) => exp
        case Expression.Int32(lit) => exp
        case Expression.Int64(lit) => exp
        case Expression.BigInt(lit) => exp
        case Expression.Str(lit) => exp

        case Expression.Var(sym, tpe, loc) => Expression.Var(sym, tpe, loc)

        case Expression.Def(sym, tpe, loc) => Expression.Def(sym, tpe, loc)

        case Expression.Eff(sym, tpe, loc) => Expression.Eff(sym, tpe, loc)

        case Expression.Closure(sym, freeVars, tpe, loc) => Expression.Closure(sym, freeVars, tpe, loc)

        case Expression.ApplyClo(exp, args, tpe, loc) =>
          val e = labelExp(exp)
          val as = args map labelExp
          Expression.ApplyClo(e, as, tpe, loc)

        case Expression.ApplyDef(sym, args, tpe, loc) =>
          val as = args map labelExp
          Expression.ApplyDef(sym, as, tpe, loc)

        case Expression.ApplyEff(sym, args, tpe, loc) =>
          val as = args map labelExp
          Expression.ApplyEff(sym, as, tpe, loc)

        case Expression.ApplyCloTail(exp, args, tpe, loc) =>
          val e = labelExp(exp)
          val as = args map labelExp
          Expression.ApplyCloTail(e, as, tpe, loc)

        case Expression.ApplyDefTail(sym, args, tpe, loc) =>
          val as = args map labelExp
          Expression.ApplyDefTail(sym, as, tpe, loc)

        case Expression.ApplyEffTail(sym, args, tpe, loc) =>
          val as = args map labelExp
          Expression.ApplyEffTail(sym, as, tpe, loc)

        case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
          val as = actuals map labelExp
          Expression.ApplySelfTail(sym, formals, as, tpe, loc)

        case Expression.Unary(sop, op, exp, tpe, loc) =>
          val e = labelExp(exp)
          Expression.Unary(sop, op, e, tpe, loc)

        case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
          val e1 = labelExp(exp1)
          val e2 = labelExp(exp2)
          Expression.Binary(sop, op, e1, e2, tpe, loc)

        case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
          val cond = labelExp(exp1)
          val consequent = labelExp(exp2)
          val alternative = labelExp(exp3)
          Expression.IfThenElse(cond, consequent, alternative, tpe, loc)

        case Expression.Branch(exp, branches, tpe, loc) =>
          val e = labelExp(exp)
          val bs = branches map {
            case (sym, br) => sym -> labelExp(br)
          }
          Expression.Branch(e, bs, tpe, loc)

        case Expression.JumpTo(sym, tpe, loc) =>
          Expression.JumpTo(sym, tpe, loc)

        case Expression.Let(sym, exp1, exp2, tpe, loc) =>
          // Visit the value expression.
          val e1 = labelExp(exp1)
          val e2 = labelExp(exp2)
          Expression.Let(sym, e1, e2, tpe, loc)

        case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
          val e1 = labelExp(exp1)
          val e2 = labelExp(exp2)
          Expression.LetRec(sym, e1, e2, tpe, loc)

        case Expression.Is(sym, tag, exp, loc) =>
          val e = labelExp(exp)
          Expression.Is(sym, tag, e, loc)

        case Expression.Tag(sym, tag, exp, tpe, loc) =>
          val e = labelExp(exp)
          Expression.Tag(sym, tag, e, tpe, loc)

        case Expression.Untag(sym, tag, exp, tpe, loc) =>
          val e = labelExp(exp)
          Expression.Untag(sym, tag, e, tpe, loc)

        case Expression.Index(base, offset, tpe, loc) =>
          val b = labelExp(base)
          Expression.Index(b, offset, tpe, loc)

        case Expression.Tuple(elms, tpe, loc) =>
          val es = elms map labelExp
          Expression.Tuple(es, tpe, loc)

        case Expression.RecordEmpty(tpe, loc) =>
          Expression.RecordEmpty(tpe, loc)

        case Expression.RecordSelect(exp, label, tpe, loc) =>
          val e = labelExp(exp)
          Expression.RecordSelect(e, label, tpe, loc)

        case Expression.RecordExtend(label, value, rest, tpe, loc) =>
          val v = labelExp(value)
          val r = labelExp(rest)
          Expression.RecordExtend(label, v, r, tpe, loc)

        case Expression.RecordRestrict(label, rest, tpe, loc) =>
          val r = labelExp(rest)
          Expression.RecordRestrict(label, r, tpe, loc)

        case Expression.ArrayLit(elms, tpe, loc) =>
          val es = elms map labelExp
          Expression.ArrayLit(es, tpe, loc)

        case Expression.ArrayNew(elm, len, tpe, loc) =>
          val e = labelExp(elm)
          val ln = labelExp(len)
          Expression.ArrayNew(e, ln, tpe, loc)

        case Expression.ArrayLoad(base, index, tpe, loc) =>
          val b = labelExp(base)
          val i = labelExp(index)
          Expression.ArrayLoad(b, i, tpe, loc)

        case Expression.ArrayStore(base, index, elm, tpe, loc) =>
          val b = labelExp(base)
          val i = labelExp(index)
          val e = labelExp(elm)
          Expression.ArrayStore(b, i, e, tpe, loc)

        case Expression.ArrayLength(base, tpe, loc) =>
          val b = labelExp(base)
          Expression.ArrayLength(b, tpe, loc)

        case Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) =>
          val b = labelExp(base)
          val i1 = labelExp(startIndex)
          val i2 = labelExp(endIndex)
          Expression.ArraySlice(b, i1, i2, tpe, loc)

        case Expression.Ref(exp, tpe, loc) =>
          val e = labelExp(exp)
          Expression.Ref(e, tpe, loc)

        case Expression.Deref(exp, tpe, loc) =>
          val e = labelExp(exp)
          Expression.Deref(e, tpe, loc)

        case Expression.Assign(exp1, exp2, tpe, loc) =>
          val e1 = labelExp(exp1)
          val e2 = labelExp(exp2)
          Expression.Assign(e1, e2, tpe, loc)

        case Expression.HandleWith(exp, bindings, tpe, loc) =>
          val e = labelExp(exp)
          val bs = bindings map {
            case HandlerBinding(sym, handler) => HandlerBinding(sym, labelExp(handler))
          }
          Expression.HandleWith(e, bs, tpe, loc)

        case Expression.Existential(fparam, exp, loc) =>
          val e = labelExp(exp)
          Expression.Existential(fparam, e, loc)

        case Expression.Universal(fparam, exp, loc) =>
          val e = labelExp(exp)
          Expression.Universal(fparam, e, loc)

        case Expression.TryCatch(exp, rules, tpe, loc) =>
          val e = labelExp(exp)
          val rs = rules map {
            case CatchRule(sym, clazz, body) =>
              val b = labelExp(body)
              CatchRule(sym, clazz, b)
          }
          Expression.TryCatch(e, rs, tpe, loc)

        case Expression.NativeConstructor(constructor, args, tpe, loc) =>
          val as = args map labelExp
          Expression.NativeConstructor(constructor, as, tpe, loc)

        case Expression.NativeField(field, tpe, loc) =>
          Expression.NativeField(field, tpe, loc)

        case Expression.NativeMethod(method, args, tpe, loc) =>
          val as = args map labelExp
          Expression.NativeMethod(method, as, tpe, loc)

        case Expression.NewChannel(exp, tpe, loc) =>
          val e = labelExp(exp)
          Expression.NewChannel(e, tpe, loc)

        case Expression.GetChannel(exp, tpe, loc) =>
          val e = labelExp(exp)
          Expression.GetChannel(e, tpe, loc)

        case Expression.PutChannel(exp1, exp2, tpe, loc) =>
          val e1 = labelExp(exp1)
          val e2 = labelExp(exp2)
          Expression.PutChannel(e1, e2, tpe, loc)

        case Expression.SelectChannel(rules, default, tpe, loc) =>
          val rs = rules map {
            case SelectChannelRule(sym, chan, exp) =>
              val c = labelExp(chan)
              val e = labelExp(exp)
              SelectChannelRule(sym, c, e)
          }

          val d = default.map(labelExp)

          Expression.SelectChannel(rs, d, tpe, loc)

        case Expression.ProcessSpawn(exp, tpe, loc) =>
          val e = labelExp(exp)
          Expression.ProcessSpawn(e, tpe, loc)

        case Expression.ProcessSleep(exp, tpe, loc) =>
          val e = labelExp(exp)
          Expression.ProcessSleep(e, tpe, loc)

        case Expression.ProcessPanic(msg, tpe, loc) =>
          Expression.ProcessPanic(msg, tpe, loc)

        case Expression.FixpointConstraintSet(cs0, tpe, loc) => Expression.FixpointConstraintSet(cs0, tpe, loc)

        case Expression.FixpointCompose(exp1, exp2, tpe, loc) =>
          val e1 = labelExp(exp1)
          val e2 = labelExp(exp2)
          Expression.FixpointCompose(e1, e2, tpe, loc)

        case Expression.FixpointSolve(exp, stf, tpe, loc) =>
          val e = labelExp(exp)
          Expression.FixpointSolve(e, stf, tpe, loc)

        case Expression.FixpointProject(sym, exp, tpe, loc) =>
          val e = labelExp(exp)
          Expression.FixpointProject(sym, e, tpe, loc)

        case Expression.FixpointEntails(exp1, exp2, tpe, loc) =>
          val e1 = labelExp(exp1)
          val e2 = labelExp(exp2)
          Expression.FixpointEntails(e1, e2, tpe, loc)

        case Expression.FixpointFold(sym, exp1, exp2, exp3, tpe, loc) =>
          val e1 = labelExp(exp1)
          val e2 = labelExp(exp2)
          val e3 = labelExp(exp3)
          Expression.FixpointFold(sym, e1, e2, e3, tpe, loc)

        case Expression.HoleError(sym, tpe, loc) => Expression.HoleError(sym, tpe, loc)
        case Expression.MatchError(tpe, loc) => Expression.MatchError(tpe, loc)
        case Expression.SwitchError(tpe, loc) => Expression.SwitchError(tpe, loc)

        case Expression.LambdaClosure(fparams, freeVars, exp, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp.getClass}'.")
        case Expression.Lambda(args, body, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp.getClass}'.")
        case Expression.Apply(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp.getClass}'.")
      }
      LabeledExpression(thisLabel, lExp)
    }

    val labeledDefs = root.defs.map {
      case (sym, defn) => sym -> defn.copy(exp = labelExp(defn.exp))
    }

    def closuresExp(acc: Map[Type, Set[Expression.Closure]], exp: SimplifiedAst.Expression): Map[Type, Set[Expression.Closure]] =
      exp match {
        case LabeledExpression(_, exp) => closuresExp(acc, exp)

        case Expression.Unit => acc
        case Expression.True => acc
        case Expression.False => acc
        case Expression.Char(lit) => acc
        case Expression.Float32(lit) => acc
        case Expression.Float64(lit) => acc
        case Expression.Int8(lit) => acc
        case Expression.Int16(lit) => acc
        case Expression.Int32(lit) => acc
        case Expression.Int64(lit) => acc
        case Expression.BigInt(lit) => acc
        case Expression.Str(lit) => acc

        case Expression.Var(sym, tpe, loc) => acc

        case Expression.Def(sym, tpe, loc) => acc

        case Expression.Eff(sym, tpe, loc) => acc

        case Expression.Closure(sym, freeVars, tpe, loc) =>
          val closures = acc.getOrElse(tpe, Set())
          acc + (tpe -> (closures + Expression.Closure(sym, freeVars, tpe, loc)))

        case Expression.ApplyClo(exp, args, tpe, loc) =>
          val e = closuresExp(acc, exp)
          val as = args.foldLeft(acc)(closuresExp)
          e ++ as

        case Expression.ApplyDef(sym, args, tpe, loc) => args.foldLeft(acc)(closuresExp)

        case Expression.ApplyEff(sym, args, tpe, loc) => args.foldLeft(acc)(closuresExp)

        case Expression.ApplyCloTail(exp, args, tpe, loc) =>
          val e = closuresExp(acc, exp)
          val as = args.foldLeft(acc)(closuresExp)
          e ++ as

        case Expression.ApplyDefTail(sym, args, tpe, loc) => args.foldLeft(acc)(closuresExp)

        case Expression.ApplyEffTail(sym, args, tpe, loc) => args.foldLeft(acc)(closuresExp)

        case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) => actuals.foldLeft(acc)(closuresExp)

        case Expression.Unary(sop, op, exp, tpe, loc) => closuresExp(acc, exp)

        case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
          val e1 = closuresExp(acc, exp1)
          val e2 = closuresExp(acc, exp2)
          e1 ++ e2

        case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
          val cond = closuresExp(acc, exp1)
          val consequent = closuresExp(acc, exp2)
          val alternative = closuresExp(acc, exp3)
          cond ++ consequent ++ alternative

        case Expression.Branch(exp, branches, tpe, loc) =>
          val e = closuresExp(acc, exp)
          val bs = branches.foldLeft(acc) {
            case (clos, (_, br)) => closuresExp(clos, br)
          }
          e ++ bs

        case Expression.JumpTo(sym, tpe, loc) => acc

        case Expression.Let(sym, exp1, exp2, tpe, loc) =>
          // Visit the value expression.
          val e1 = closuresExp(acc, exp1)
          val e2 = closuresExp(acc, exp2)
          e1 ++ e2

        case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
          val e1 = closuresExp(acc, exp1)
          val e2 = closuresExp(acc, exp2)
          e1 ++ e2

        case Expression.Is(sym, tag, exp, loc) => closuresExp(acc, exp)

        case Expression.Tag(sym, tag, exp, tpe, loc) => closuresExp(acc, exp)

        case Expression.Untag(sym, tag, exp, tpe, loc) => closuresExp(acc, exp)

        case Expression.Index(base, offset, tpe, loc) => closuresExp(acc, base)

        case Expression.Tuple(elms, tpe, loc) => elms.foldLeft(acc)(closuresExp)

        case Expression.RecordEmpty(tpe, loc) => acc

        case Expression.RecordSelect(exp, label, tpe, loc) => closuresExp(acc, exp)

        case Expression.RecordExtend(label, value, rest, tpe, loc) =>
          val v = closuresExp(acc, value)
          val r = closuresExp(acc, rest)
          v ++ r

        case Expression.RecordRestrict(label, rest, tpe, loc) => closuresExp(acc, rest)

        case Expression.ArrayLit(elms, tpe, loc) => elms.foldLeft(acc)(closuresExp)

        case Expression.ArrayNew(elm, len, tpe, loc) =>
          val e = closuresExp(acc, elm)
          val ln = closuresExp(acc, len)
          e ++ ln

        case Expression.ArrayLoad(base, index, tpe, loc) =>
          val b = closuresExp(acc, base)
          val i = closuresExp(acc, index)
          b ++ i

        case Expression.ArrayStore(base, index, elm, tpe, loc) =>
          val b = closuresExp(acc, base)
          val i = closuresExp(acc, index)
          val e = closuresExp(acc, elm)
          b ++ i ++ e

        case Expression.ArrayLength(base, tpe, loc) => closuresExp(acc, base)

        case Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) =>
          val b = closuresExp(acc, base)
          val i1 = closuresExp(acc, startIndex)
          val i2 = closuresExp(acc, endIndex)
          b ++ i1 ++ i2

        case Expression.Ref(exp, tpe, loc) => closuresExp(acc, exp)

        case Expression.Deref(exp, tpe, loc) => closuresExp(acc, exp)

        case Expression.Assign(exp1, exp2, tpe, loc) =>
          val e1 = closuresExp(acc, exp1)
          val e2 = closuresExp(acc, exp2)
          e1 ++ e2

        case Expression.HandleWith(exp, bindings, tpe, loc) =>
          val e = closuresExp(acc, exp)
          val bs = bindings.foldLeft(acc) {
            case (clos, SimplifiedAst.HandlerBinding(_, handler)) => closuresExp(clos, handler)
          }
          e ++ bs

        case Expression.Existential(fparam, exp, loc) => closuresExp(acc, exp)

        case Expression.Universal(fparam, exp, loc) => closuresExp(acc, exp)

        case Expression.TryCatch(exp, rules, tpe, loc) =>
          val e = closuresExp(acc, exp)
          val rs = rules.foldLeft(acc) {
            case (clos, SimplifiedAst.CatchRule(_, _, body)) => closuresExp(clos, body)
          }
          e ++ rs

        case Expression.NativeConstructor(constructor, args, tpe, loc) => args.foldLeft(acc)(closuresExp)

        case Expression.NativeField(field, tpe, loc) => acc

        case Expression.NativeMethod(method, args, tpe, loc) => args.foldLeft(acc)(closuresExp)

        case Expression.NewChannel(exp, tpe, loc) => closuresExp(acc, exp)

        case Expression.GetChannel(exp, tpe, loc) => closuresExp(acc, exp)

        case Expression.PutChannel(exp1, exp2, tpe, loc) =>
          val e1 = closuresExp(acc, exp1)
          val e2 = closuresExp(acc, exp2)
          e1 ++ e2

        case Expression.SelectChannel(rules, default, tpe, loc) =>
          val rs = rules.foldLeft(acc) {
            case (clos, SimplifiedAst.SelectChannelRule(_, chan, exp)) =>
              val c = closuresExp(acc, chan)
              val e = closuresExp(acc, exp)
              c ++ e
          }

          val d = default.fold(Map.empty[Type, Set[Expression.Closure]])(closuresExp(acc, _))
          rs ++ d

        case Expression.ProcessSpawn(exp, tpe, loc) => closuresExp(acc, exp)

        case Expression.ProcessSleep(exp, tpe, loc) => closuresExp(acc, exp)

        case Expression.ProcessPanic(msg, tpe, loc) => acc

        case Expression.FixpointConstraintSet(cs0, tpe, loc) => acc

        case Expression.FixpointCompose(exp1, exp2, tpe, loc) =>
          val e1 = closuresExp(acc, exp1)
          val e2 = closuresExp(acc, exp2)
          e1 ++ e2

        case Expression.FixpointSolve(exp, stf, tpe, loc) => closuresExp(acc, exp)

        case Expression.FixpointProject(sym, exp, tpe, loc) => closuresExp(acc, exp)

        case Expression.FixpointEntails(exp1, exp2, tpe, loc) =>
          val e1 = closuresExp(acc, exp1)
          val e2 = closuresExp(acc, exp2)
          e1 ++ e2

        case Expression.FixpointFold(sym, exp1, exp2, exp3, tpe, loc) =>
          val e1 = closuresExp(acc, exp1)
          val e2 = closuresExp(acc, exp2)
          val e3 = closuresExp(acc, exp3)
          e1 ++ e2 ++ e3

        case Expression.HoleError(sym, tpe, loc) => acc
        case Expression.MatchError(tpe, loc) => acc
        case Expression.SwitchError(tpe, loc) => acc

        case Expression.LambdaClosure(fparams, freeVars, exp, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp.getClass}'.")
        case Expression.Lambda(args, body, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp.getClass}'.")
        case Expression.Apply(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp.getClass}'.")
      }

    val closures = labeledDefs.foldLeft(Map.empty[Type, Set[Expression.Closure]]){
      case (c, (_, defn)) => closuresExp(c, defn.exp)
    }

    def visitExp(acc: (AbstractEnvironment, AbstractCache), lExp: LabeledExpression): (AbstractEnvironment, AbstractCache) = {
      val (varFlow, expFlow) = acc
      val result = closures.get(lExp.tpe) match {
        case Some(c) =>
          lExp.exp match {
            case Expression.Var(sym, tpe, loc) =>
              val flow = varFlow.getOrElse(sym, Set())
              val newFlow = varFlow + (sym -> (flow ++ c))
              (newFlow, expFlow)
            case _ =>
              val flow = expFlow.getOrElse(lExp, Set())
              val newFlow = expFlow + (lExp -> (flow ++ c))
              (varFlow, newFlow)
          }
        case None => acc
      }
      lExp.exp match {
        case Expression.Unit => result
        case Expression.True => result
        case Expression.False => result
        case Expression.Char(lit) => result
        case Expression.Float32(lit) => result
        case Expression.Float64(lit) => result
        case Expression.Int8(lit) => result
        case Expression.Int16(lit) => result
        case Expression.Int32(lit) => result
        case Expression.Int64(lit) => result
        case Expression.BigInt(lit) => result
        case Expression.Str(lit) => result

        case Expression.Var(sym, tpe, loc) => result

        case Expression.Def(sym, tpe, loc) => result

        case Expression.Eff(sym, tpe, loc) => result

        case Expression.Closure(sym, freeVars, tpe, loc) => result

        case Expression.ApplyClo(exp, args, tpe, loc) =>
          val e = visitExp(result, exp)
          val as = args.foldLeft(e)(visitExp(_,_))
          as

        case Expression.ApplyDef(sym, args, tpe, loc) => args.foldLeft(result)(visitExp(_,_))

        case Expression.ApplyEff(sym, args, tpe, loc) => args.foldLeft(result)(visitExp(_,_))

        case Expression.ApplyCloTail(exp, args, tpe, loc) =>
          val e = visitExp(result, exp)
          val as = args.foldLeft(e)(visitExp(_,_))
          as

        case Expression.ApplyDefTail(sym, args, tpe, loc) => args.foldLeft(result)(visitExp(_,_))

        case Expression.ApplyEffTail(sym, args, tpe, loc) => args.foldLeft(result)(visitExp(_,_))

        case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) => actuals.foldLeft(result)(visitExp(_,_))

        case Expression.Unary(sop, op, exp, tpe, loc) => visitExp(result, exp)

        case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
          val e1 = visitExp(result, exp1)
          val e2 = visitExp(e1, exp2)
          e2

        case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
          val cond = visitExp(result, exp1)
          val consequent = visitExp(cond, exp2)
          val alternative = visitExp(consequent, exp3)
          alternative

        case Expression.Branch(exp, branches, tpe, loc) =>
          val e = visitExp(result, exp)
          val bs = branches.foldLeft(e) {
            case (res, (_, br)) => visitExp(res, br)
          }
          bs

        case Expression.JumpTo(sym, tpe, loc) => result

        case Expression.Let(sym, exp1, exp2, tpe, loc) =>
          // Visit the value expression.
          val e1 = visitExp(result, exp1)
          val e2 = visitExp(e1, exp2)
          e2

        case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
          val e1 = visitExp(result, exp1)
          val e2 = visitExp(e1, exp2)
          e2

        case Expression.Is(sym, tag, exp, loc) => visitExp(result, exp)

        case Expression.Tag(sym, tag, exp, tpe, loc) => visitExp(result, exp)

        case Expression.Untag(sym, tag, exp, tpe, loc) => visitExp(result, exp)

        case Expression.Index(base, offset, tpe, loc) => visitExp(result, base)

        case Expression.Tuple(elms, tpe, loc) => elms.foldLeft(result)(visitExp(_,_))

        case Expression.RecordEmpty(tpe, loc) => result

        case Expression.RecordSelect(exp, label, tpe, loc) => visitExp(result, exp)

        case Expression.RecordExtend(label, value, rest, tpe, loc) =>
          val v = visitExp(result, value)
          val r = visitExp(v, rest)
          r

        case Expression.RecordRestrict(label, rest, tpe, loc) => visitExp(result, rest)

        case Expression.ArrayLit(elms, tpe, loc) => elms.foldLeft(result)(visitExp(_,_))

        case Expression.ArrayNew(elm, len, tpe, loc) =>
          val e = visitExp(result, elm)
          val ln = visitExp(e, len)
          ln

        case Expression.ArrayLoad(base, index, tpe, loc) =>
          val b = visitExp(result, base)
          val i = visitExp(b, index)
          i

        case Expression.ArrayStore(base, index, elm, tpe, loc) =>
          val b = visitExp(result, base)
          val i = visitExp(b, index)
          val e = visitExp(i, elm)
          e

        case Expression.ArrayLength(base, tpe, loc) => visitExp(result, base)

        case Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) =>
          val b = visitExp(result, base)
          val i1 = visitExp(b, startIndex)
          val i2 = visitExp(i1, endIndex)
          i2

        case Expression.Ref(exp, tpe, loc) => visitExp(result, exp)

        case Expression.Deref(exp, tpe, loc) => visitExp(result, exp)

        case Expression.Assign(exp1, exp2, tpe, loc) =>
          val e1 = visitExp(result, exp1)
          val e2 = visitExp(e1, exp2)
          e2

        case Expression.HandleWith(exp, bindings, tpe, loc) =>
          val e = visitExp(result, exp)
          val bs = bindings.foldLeft(e) {
            case (res, SimplifiedAst.HandlerBinding(_, handler)) => visitExp(res, handler)
          }
          bs

        case Expression.Existential(fparam, exp, loc) => visitExp(result, exp)

        case Expression.Universal(fparam, exp, loc) => visitExp(result, exp)

        case Expression.TryCatch(exp, rules, tpe, loc) =>
          val e = visitExp(result, exp)
          val rs = rules.foldLeft(e) {
            case (res, SimplifiedAst.CatchRule(_, _, body)) => visitExp(res, body)
          }
          rs

        case Expression.NativeConstructor(constructor, args, tpe, loc) => args.foldLeft(result)(visitExp(_,_))

        case Expression.NativeField(field, tpe, loc) => result

        case Expression.NativeMethod(method, args, tpe, loc) => args.foldLeft(result)(visitExp(_,_))

        case Expression.NewChannel(exp, tpe, loc) => visitExp(result, exp)

        case Expression.GetChannel(exp, tpe, loc) => visitExp(result, exp)

        case Expression.PutChannel(exp1, exp2, tpe, loc) =>
          val e1 = visitExp(result, exp1)
          val e2 = visitExp(e1, exp2)
          e2

        case Expression.SelectChannel(rules, default, tpe, loc) =>
          val rs = rules.foldLeft(result) {
            case (res, SimplifiedAst.SelectChannelRule(_, chan, exp)) =>
              val c = visitExp(res, chan)
              val e = visitExp(c, exp)
              e
          }
          val d = default.fold(rs)(visitExp(rs, _))
          d

        case Expression.ProcessSpawn(exp, tpe, loc) => visitExp(result, exp)

        case Expression.ProcessSleep(exp, tpe, loc) => visitExp(result, exp)

        case Expression.ProcessPanic(msg, tpe, loc) => result

        case Expression.FixpointConstraintSet(cs0, tpe, loc) => result

        case Expression.FixpointCompose(exp1, exp2, tpe, loc) =>
          val e1 = visitExp(result, exp1)
          val e2 = visitExp(e1, exp2)
          e2

        case Expression.FixpointSolve(exp, stf, tpe, loc) => visitExp(result, exp)

        case Expression.FixpointProject(sym, exp, tpe, loc) => visitExp(result, exp)

        case Expression.FixpointEntails(exp1, exp2, tpe, loc) =>
          val e1 = visitExp(result, exp1)
          val e2 = visitExp(e1, exp2)
          e2

        case Expression.FixpointFold(sym, exp1, exp2, exp3, tpe, loc) =>
          val e1 = visitExp(result, exp1)
          val e2 = visitExp(e1, exp2)
          val e3 = visitExp(e2, exp3)
          e3

        case Expression.HoleError(sym, tpe, loc) => result
        case Expression.MatchError(tpe, loc) => result
        case Expression.SwitchError(tpe, loc) => result

        case Expression.LambdaClosure(fparams, freeVars, _, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${lExp.exp.getClass}'.")
        case Expression.Lambda(args, body, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${lExp.exp.getClass}'.")
        case Expression.Apply(_, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${lExp.exp.getClass}'.")
      }
    }

    val (varFlow, expFlow) = labeledDefs.foldLeft((Map(), Map()): (AbstractEnvironment, AbstractCache)) {
      case (acc, (_, defn)) => visitExp(acc, defn.exp)
    }

    def inlineClosApp(lExp: LabeledExpression): Option[Symbol.DefnSym] = {
      val closures = lExp.exp match {
        case Expression.Var(sym, tpe, loc) => varFlow.getOrElse(sym, Set())
        case _ => expFlow.getOrElse(lExp, Set())
      }
      if (closures.size == 1) {
        val clos = closures.head
        if (clos.freeVars.isEmpty) {
          val defn = root.defs(clos.sym)
          Some(defn.sym)
        } else {
          None
        }
      } else {
        None
      }
    }

    def inliner(exp0: SimplifiedAst.Expression) : SimplifiedAst.Expression =
      exp0 match {
        case LabeledExpression(label, exp) => inliner(exp)

        case Expression.Unit => exp0
        case Expression.True => exp0
        case Expression.False => exp0
        case Expression.Char(lit) => exp0
        case Expression.Float32(lit) => exp0
        case Expression.Float64(lit) => exp0
        case Expression.Int8(lit) => exp0
        case Expression.Int16(lit) => exp0
        case Expression.Int32(lit) => exp0
        case Expression.Int64(lit) => exp0
        case Expression.BigInt(lit) => exp0
        case Expression.Str(lit) => exp0

        case Expression.Var(sym, tpe, loc) => Expression.Var(sym, tpe, loc)

        case Expression.Def(sym, tpe, loc) => Expression.Def(sym, tpe, loc)

        case Expression.Eff(sym, tpe, loc) => Expression.Eff(sym, tpe, loc)

        case Expression.Closure(sym, freeVars, tpe, loc) => Expression.Closure(sym, freeVars, tpe, loc)

        case Expression.ApplyClo(exp, args, tpe, loc) =>
          val as = args map inliner
          inlineClosApp(exp) match {
            case Some(inlined) => Expression.ApplyDef(inlined, as, tpe, loc)
            case None =>
              val e = inliner(exp)
              Expression.ApplyClo(e, as, tpe, loc)
          }

        case Expression.ApplyDef(sym, args, tpe, loc) =>
          val as = args map inliner
          Expression.ApplyDef(sym, as, tpe, loc)

        case Expression.ApplyEff(sym, args, tpe, loc) =>
          val as = args map inliner
          Expression.ApplyEff(sym, as, tpe, loc)

        case Expression.ApplyCloTail(exp, args, tpe, loc) =>
          val as = args map inliner
          inlineClosApp(exp) match {
            case Some(inlined) => Expression.ApplyDefTail(inlined, as, tpe, loc)
            case None =>
              val e = inliner(exp)
              Expression.ApplyCloTail(e, as, tpe, loc)
          }

        case Expression.ApplyDefTail(sym, args, tpe, loc) =>
          val as = args map inliner
          Expression.ApplyDefTail(sym, as, tpe, loc)

        case Expression.ApplyEffTail(sym, args, tpe, loc) =>
          val as = args map inliner
          Expression.ApplyEffTail(sym, as, tpe, loc)

        case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
          val as = actuals map inliner
          Expression.ApplySelfTail(sym, formals, as, tpe, loc)

        case Expression.Unary(sop, op, exp, tpe, loc) =>
          val e = inliner(exp)
          Expression.Unary(sop, op, e, tpe, loc)

        case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
          val e1 = inliner(exp1)
          val e2 = inliner(exp2)
          Expression.Binary(sop, op, e1, e2, tpe, loc)

        case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
          val cond = inliner(exp1)
          val consequent = inliner(exp2)
          val alternative = inliner(exp3)
          Expression.IfThenElse(cond, consequent, alternative, tpe, loc)

        case Expression.Branch(exp, branches, tpe, loc) =>
          val e = inliner(exp)
          val bs = branches map {
            case (sym, br) => sym -> inliner(br)
          }
          Expression.Branch(e, bs, tpe, loc)

        case Expression.JumpTo(sym, tpe, loc) =>
          Expression.JumpTo(sym, tpe, loc)

        case Expression.Let(sym, exp1, exp2, tpe, loc) =>
          // Visit the value expression.
          val e1 = inliner(exp1)
          val e2 = inliner(exp2)
          Expression.Let(sym, e1, e2, tpe, loc)

        case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
          val e1 = inliner(exp1)
          val e2 = inliner(exp2)
          Expression.LetRec(sym, e1, e2, tpe, loc)

        case Expression.Is(sym, tag, exp, loc) =>
          val e = inliner(exp)
          Expression.Is(sym, tag, e, loc)

        case Expression.Tag(sym, tag, exp, tpe, loc) =>
          val e = inliner(exp)
          Expression.Tag(sym, tag, e, tpe, loc)

        case Expression.Untag(sym, tag, exp, tpe, loc) =>
          val e = inliner(exp)
          Expression.Untag(sym, tag, e, tpe, loc)

        case Expression.Index(base, offset, tpe, loc) =>
          val b = inliner(base)
          Expression.Index(b, offset, tpe, loc)

        case Expression.Tuple(elms, tpe, loc) =>
          val es = elms map inliner
          Expression.Tuple(es, tpe, loc)

        case Expression.RecordEmpty(tpe, loc) =>
          Expression.RecordEmpty(tpe, loc)

        case Expression.RecordSelect(exp, label, tpe, loc) =>
          val e = inliner(exp)
          Expression.RecordSelect(e, label, tpe, loc)

        case Expression.RecordExtend(label, value, rest, tpe, loc) =>
          val v = inliner(value)
          val r = inliner(rest)
          Expression.RecordExtend(label, v, r, tpe, loc)

        case Expression.RecordRestrict(label, rest, tpe, loc) =>
          val r = inliner(rest)
          Expression.RecordRestrict(label, r, tpe, loc)

        case Expression.ArrayLit(elms, tpe, loc) =>
          val es = elms map inliner
          Expression.ArrayLit(es, tpe, loc)

        case Expression.ArrayNew(elm, len, tpe, loc) =>
          val e = inliner(elm)
          val ln = inliner(len)
          Expression.ArrayNew(e, ln, tpe, loc)

        case Expression.ArrayLoad(base, index, tpe, loc) =>
          val b = inliner(base)
          val i = inliner(index)
          Expression.ArrayLoad(b, i, tpe, loc)

        case Expression.ArrayStore(base, index, elm, tpe, loc) =>
          val b = inliner(base)
          val i = inliner(index)
          val e = inliner(elm)
          Expression.ArrayStore(b, i, e, tpe, loc)

        case Expression.ArrayLength(base, tpe, loc) =>
          val b = inliner(base)
          Expression.ArrayLength(b, tpe, loc)

        case Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) =>
          val b = inliner(base)
          val i1 = inliner(startIndex)
          val i2 = inliner(endIndex)
          Expression.ArraySlice(b, i1, i2, tpe, loc)

        case Expression.Ref(exp, tpe, loc) =>
          val e = inliner(exp)
          Expression.Ref(e, tpe, loc)

        case Expression.Deref(exp, tpe, loc) =>
          val e = inliner(exp)
          Expression.Deref(e, tpe, loc)

        case Expression.Assign(exp1, exp2, tpe, loc) =>
          val e1 = inliner(exp1)
          val e2 = inliner(exp2)
          Expression.Assign(e1, e2, tpe, loc)

        case Expression.HandleWith(exp, bindings, tpe, loc) =>
          val e = inliner(exp)
          val bs = bindings map {
            case HandlerBinding(sym, handler) => HandlerBinding(sym, inliner(handler))
          }
          Expression.HandleWith(e, bs, tpe, loc)

        case Expression.Existential(fparam, exp, loc) =>
          val e = inliner(exp)
          Expression.Existential(fparam, e, loc)

        case Expression.Universal(fparam, exp, loc) =>
          val e = inliner(exp)
          Expression.Universal(fparam, e, loc)

        case Expression.TryCatch(exp, rules, tpe, loc) =>
          val e = inliner(exp)
          val rs = rules map {
            case CatchRule(sym, clazz, body) =>
              val b = inliner(body)
              CatchRule(sym, clazz, b)
          }
          Expression.TryCatch(e, rs, tpe, loc)

        case Expression.NativeConstructor(constructor, args, tpe, loc) =>
          val as = args map inliner
          Expression.NativeConstructor(constructor, as, tpe, loc)

        case Expression.NativeField(field, tpe, loc) =>
          Expression.NativeField(field, tpe, loc)

        case Expression.NativeMethod(method, args, tpe, loc) =>
          val as = args map inliner
          Expression.NativeMethod(method, as, tpe, loc)

        case Expression.NewChannel(exp, tpe, loc) =>
          val e = inliner(exp)
          Expression.NewChannel(e, tpe, loc)

        case Expression.GetChannel(exp, tpe, loc) =>
          val e = inliner(exp)
          Expression.GetChannel(e, tpe, loc)

        case Expression.PutChannel(exp1, exp2, tpe, loc) =>
          val e1 = inliner(exp1)
          val e2 = inliner(exp2)
          Expression.PutChannel(e1, e2, tpe, loc)

        case Expression.SelectChannel(rules, default, tpe, loc) =>
          val rs = rules map {
            case SelectChannelRule(sym, chan, exp) =>
              val c = inliner(chan)
              val e = inliner(exp)
              SelectChannelRule(sym, c, e)
          }

          val d = default.map(inliner)

          Expression.SelectChannel(rs, d, tpe, loc)

        case Expression.ProcessSpawn(exp, tpe, loc) =>
          val e = inliner(exp)
          Expression.ProcessSpawn(e, tpe, loc)

        case Expression.ProcessSleep(exp, tpe, loc) =>
          val e = inliner(exp)
          Expression.ProcessSleep(e, tpe, loc)

        case Expression.ProcessPanic(msg, tpe, loc) =>
          Expression.ProcessPanic(msg, tpe, loc)

        case Expression.FixpointConstraintSet(cs0, tpe, loc) => Expression.FixpointConstraintSet(cs0, tpe, loc)

        case Expression.FixpointCompose(exp1, exp2, tpe, loc) =>
          val e1 = inliner(exp1)
          val e2 = inliner(exp2)
          Expression.FixpointCompose(e1, e2, tpe, loc)

        case Expression.FixpointSolve(exp, stf, tpe, loc) =>
          val e = inliner(exp)
          Expression.FixpointSolve(e, stf, tpe, loc)

        case Expression.FixpointProject(sym, exp, tpe, loc) =>
          val e = inliner(exp)
          Expression.FixpointProject(sym, e, tpe, loc)

        case Expression.FixpointEntails(exp1, exp2, tpe, loc) =>
          val e1 = inliner(exp1)
          val e2 = inliner(exp2)
          Expression.FixpointEntails(e1, e2, tpe, loc)

        case Expression.FixpointFold(sym, exp1, exp2, exp3, tpe, loc) =>
          val e1 = inliner(exp1)
          val e2 = inliner(exp2)
          val e3 = inliner(exp3)
          Expression.FixpointFold(sym, e1, e2, e3, tpe, loc)

        case Expression.HoleError(sym, tpe, loc) => Expression.HoleError(sym, tpe, loc)
        case Expression.MatchError(tpe, loc) => Expression.MatchError(tpe, loc)
        case Expression.SwitchError(tpe, loc) => Expression.SwitchError(tpe, loc)

        case Expression.LambdaClosure(fparams, freeVars, exp, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.Lambda(args, body, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.Apply(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
      }

    val defs = labeledDefs.map {
      case (sym, defn) => sym -> defn.copy(exp = inliner(defn.exp))
    }

    val result = root.copy(defs = defs)

    // Print the ast if debugging is enabled.
    if (flix.options.debug) {
      println("--------------CFA RESULT--------------")
      println(PrettyPrinter.Simplified.fmtRoot(result).fmt(TerminalContext.AnsiTerminal))
    }

    result.toSuccess
  }
}
