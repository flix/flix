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
        //
        // Literal Expressions.
        //
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

        //
        // Variable Expressions.
        //
        case Expression.Var(sym, tpe, loc) => Expression.Var(sym, tpe, loc)

        //
        // Def Expressions.
        //
        case Expression.Def(sym, tpe, loc) => Expression.Def(sym, tpe, loc)

        //
        // Eff Expressions.
        //
        case Expression.Eff(sym, tpe, loc) => Expression.Eff(sym, tpe, loc)

        //
        // Closure Expressions.
        //
        case Expression.Closure(sym, freeVars, tpe, loc) => Expression.Closure(sym, freeVars, tpe, loc)

        //
        // ApplyClo Expressions.
        //
        case Expression.ApplyClo(exp, args, tpe, loc) =>
          val e = labelExp(exp)
          val as = args map labelExp
          Expression.ApplyClo(e, as, tpe, loc)

        //
        // ApplyDef Expressions.
        //
        case Expression.ApplyDef(sym, args, tpe, loc) =>
          val as = args map labelExp
          Expression.ApplyDef(sym, as, tpe, loc)

        //
        // ApplyEff Expressions.
        //
        case Expression.ApplyEff(sym, args, tpe, loc) =>
          val as = args map labelExp
          Expression.ApplyEff(sym, as, tpe, loc)

        //
        // ApplyCloTail Expressions.
        //
        case Expression.ApplyCloTail(exp, args, tpe, loc) =>
          val e = labelExp(exp)
          val as = args map labelExp
          Expression.ApplyCloTail(e, as, tpe, loc)

        //
        // ApplyDefTail Expressions.
        //
        case Expression.ApplyDefTail(sym, args, tpe, loc) =>
          val as = args map labelExp
          Expression.ApplyDefTail(sym, as, tpe, loc)

        //
        // ApplyEffTail Expressions.
        //
        case Expression.ApplyEffTail(sym, args, tpe, loc) =>
          val as = args map labelExp
          Expression.ApplyEffTail(sym, as, tpe, loc)

        //
        // ApplySelfTail Expressions.
        //
        case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
          val as = actuals map labelExp
          Expression.ApplySelfTail(sym, formals, as, tpe, loc)

        //
        // Unary Expressions.
        //
        case Expression.Unary(sop, op, exp, tpe, loc) =>
          val e = labelExp(exp)
          Expression.Unary(sop, op, e, tpe, loc)

        //
        // Binary Expressions.
        //
        case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
          val e1 = labelExp(exp1)
          val e2 = labelExp(exp2)
          Expression.Binary(sop, op, e1, e2, tpe, loc)

        //
        // If-then-else Expressions.
        //
        case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
          val cond = labelExp(exp1)
          val consequent = labelExp(exp2)
          val alternative = labelExp(exp3)
          Expression.IfThenElse(cond, consequent, alternative, tpe, loc)

        //
        // Block Expressions.
        //
        case Expression.Branch(exp, branches, tpe, loc) =>
          val e = labelExp(exp)
          val bs = branches map {
            case (sym, br) => sym -> labelExp(br)
          }
          Expression.Branch(e, bs, tpe, loc)

        //
        // Jump Expressions.
        //
        case Expression.JumpTo(sym, tpe, loc) =>
          Expression.JumpTo(sym, tpe, loc)

        //
        // Let Expressions.
        //
        case Expression.Let(sym, exp1, exp2, tpe, loc) =>
          // Visit the value expression.
          val e1 = labelExp(exp1)
          val e2 = labelExp(exp2)
          Expression.Let(sym, e1, e2, tpe, loc)

        //
        // LetRec Expressions.
        //
        case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
          val e1 = labelExp(exp1)
          val e2 = labelExp(exp2)
          Expression.LetRec(sym, e1, e2, tpe, loc)

        //
        // Is Expressions.
        //
        case Expression.Is(sym, tag, exp, loc) =>
          val e = labelExp(exp)
          Expression.Is(sym, tag, e, loc)

        //
        // Tag Expressions.
        //
        case Expression.Tag(sym, tag, exp, tpe, loc) =>
          val e = labelExp(exp)
          Expression.Tag(sym, tag, e, tpe, loc)

        //
        // Untag Expressions.
        //
        case Expression.Untag(sym, tag, exp, tpe, loc) =>
          val e = labelExp(exp)
          Expression.Untag(sym, tag, e, tpe, loc)

        //
        // Index Expressions.
        //
        case Expression.Index(base, offset, tpe, loc) =>
          val b = labelExp(base)
          Expression.Index(b, offset, tpe, loc)

        //
        // Tuple Expressions.
        //
        case Expression.Tuple(elms, tpe, loc) =>
          val es = elms map labelExp
          Expression.Tuple(es, tpe, loc)

        //
        // RecordEmpty Expressions.
        //
        case Expression.RecordEmpty(tpe, loc) =>
          Expression.RecordEmpty(tpe, loc)

        //
        // RecordSelect Expressions.
        //
        case Expression.RecordSelect(exp, label, tpe, loc) =>
          val e = labelExp(exp)
          Expression.RecordSelect(e, label, tpe, loc)

        //
        // RecordExtend Expressions.
        //
        case Expression.RecordExtend(label, value, rest, tpe, loc) =>
          val v = labelExp(value)
          val r = labelExp(rest)
          Expression.RecordExtend(label, v, r, tpe, loc)

        //
        // RecordRestrict Expressions.
        //
        case Expression.RecordRestrict(label, rest, tpe, loc) =>
          val r = labelExp(rest)
          Expression.RecordRestrict(label, r, tpe, loc)

        //
        // ArrayLit Expressions.
        //
        case Expression.ArrayLit(elms, tpe, loc) =>
          val es = elms map labelExp
          Expression.ArrayLit(es, tpe, loc)

        //
        // ArrayNew Expressions.
        //
        case Expression.ArrayNew(elm, len, tpe, loc) =>
          val e = labelExp(elm)
          val ln = labelExp(len)
          Expression.ArrayNew(e, ln, tpe, loc)

        //
        // ArrayLoad Expressions.
        //
        case Expression.ArrayLoad(base, index, tpe, loc) =>
          val b = labelExp(base)
          val i = labelExp(index)
          Expression.ArrayLoad(b, i, tpe, loc)

        //
        // ArrayStore Expressions.
        //
        case Expression.ArrayStore(base, index, elm, tpe, loc) =>
          val b = labelExp(base)
          val i = labelExp(index)
          val e = labelExp(elm)
          Expression.ArrayStore(b, i, e, tpe, loc)

        //
        // ArraySlice Expressions.
        //
        case Expression.ArrayLength(base, tpe, loc) =>
          val b = labelExp(base)
          Expression.ArrayLength(b, tpe, loc)

        //
        // ArraySlice Expressions.
        //
        case Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) =>
          val b = labelExp(base)
          val i1 = labelExp(startIndex)
          val i2 = labelExp(endIndex)
          Expression.ArraySlice(b, i1, i2, tpe, loc)

        //
        // Reference Expressions.
        //
        case Expression.Ref(exp, tpe, loc) =>
          val e = labelExp(exp)
          Expression.Ref(e, tpe, loc)

        //
        // Dereference Expressions.
        //
        case Expression.Deref(exp, tpe, loc) =>
          val e = labelExp(exp)
          Expression.Deref(e, tpe, loc)

        //
        // Assign Expressions.
        //
        case Expression.Assign(exp1, exp2, tpe, loc) =>
          val e1 = labelExp(exp1)
          val e2 = labelExp(exp2)
          Expression.Assign(e1, e2, tpe, loc)

        //
        // HandleWith Expressions.
        //
        case Expression.HandleWith(exp, bindings, tpe, loc) =>
          val e = labelExp(exp)
          val bs = bindings map {
            case HandlerBinding(sym, handler) => HandlerBinding(sym, labelExp(handler))
          }
          Expression.HandleWith(e, bs, tpe, loc)

        //
        // Existential Expressions.
        //
        case Expression.Existential(fparam, exp, loc) =>
          val e = labelExp(exp)
          Expression.Existential(fparam, e, loc)

        //
        // Universal Expressions.
        //
        case Expression.Universal(fparam, exp, loc) =>
          val e = labelExp(exp)
          Expression.Universal(fparam, e, loc)

        //
        // Try Catch Constructor.
        //
        case Expression.TryCatch(exp, rules, tpe, loc) =>
          val e = labelExp(exp)
          val rs = rules map {
            case CatchRule(sym, clazz, body) =>
              val b = labelExp(body)
              CatchRule(sym, clazz, b)
          }
          Expression.TryCatch(e, rs, tpe, loc)

        //
        // Native Constructor.
        //
        case Expression.NativeConstructor(constructor, args, tpe, loc) =>
          val as = args map labelExp
          Expression.NativeConstructor(constructor, as, tpe, loc)

        //
        // Native Field.
        //
        case Expression.NativeField(field, tpe, loc) =>
          Expression.NativeField(field, tpe, loc)

        //
        // Native Method.
        //
        case Expression.NativeMethod(method, args, tpe, loc) =>
          val as = args map labelExp
          Expression.NativeMethod(method, as, tpe, loc)

        //
        // New Channel.
        //
        case Expression.NewChannel(exp, tpe, loc) =>
          val e = labelExp(exp)
          Expression.NewChannel(e, tpe, loc)

        //
        // Get Channel.
        //
        case Expression.GetChannel(exp, tpe, loc) =>
          val e = labelExp(exp)
          Expression.GetChannel(e, tpe, loc)

        //
        // Put Channel.
        //
        case Expression.PutChannel(exp1, exp2, tpe, loc) =>
          val e1 = labelExp(exp1)
          val e2 = labelExp(exp2)
          Expression.PutChannel(e1, e2, tpe, loc)

        //
        // Select Channel.
        //
        case Expression.SelectChannel(rules, default, tpe, loc) =>
          val rs = rules map {
            case SelectChannelRule(sym, chan, exp) =>
              val c = labelExp(chan)
              val e = labelExp(exp)
              SelectChannelRule(sym, c, e)
          }

          val d = default.map(labelExp)

          Expression.SelectChannel(rs, d, tpe, loc)

        //
        // ProcessSpawn.
        //
        case Expression.ProcessSpawn(exp, tpe, loc) =>
          val e = labelExp(exp)
          Expression.ProcessSpawn(e, tpe, loc)

        //
        // ProcessSleep.
        //
        case Expression.ProcessSleep(exp, tpe, loc) =>
          val e = labelExp(exp)
          Expression.ProcessSleep(e, tpe, loc)

        //
        // ProcessPanic.
        //
        case Expression.ProcessPanic(msg, tpe, loc) =>
          Expression.ProcessPanic(msg, tpe, loc)

        //
        // ConstraintSet.
        //
        case Expression.FixpointConstraintSet(cs0, tpe, loc) => Expression.FixpointConstraintSet(cs0, tpe, loc)

        //
        // Constraint Union.
        //
        case Expression.FixpointCompose(exp1, exp2, tpe, loc) =>
          val e1 = labelExp(exp1)
          val e2 = labelExp(exp2)
          Expression.FixpointCompose(e1, e2, tpe, loc)

        //
        // Fixpoint Solve.
        //
        case Expression.FixpointSolve(exp, stf, tpe, loc) =>
          val e = labelExp(exp)
          Expression.FixpointSolve(e, stf, tpe, loc)

        //
        // Fixpoint Project.
        //
        case Expression.FixpointProject(sym, exp, tpe, loc) =>
          val e = labelExp(exp)
          Expression.FixpointProject(sym, e, tpe, loc)

        //
        // Fixpoint Entails.
        //
        case Expression.FixpointEntails(exp1, exp2, tpe, loc) =>
          val e1 = labelExp(exp1)
          val e2 = labelExp(exp2)
          Expression.FixpointEntails(e1, e2, tpe, loc)

        //
        // Fixpoint Fold.
        //
        case Expression.FixpointFold(sym, exp1, exp2, exp3, tpe, loc) =>
          val e1 = labelExp(exp1)
          val e2 = labelExp(exp2)
          val e3 = labelExp(exp3)
          Expression.FixpointFold(sym, e1, e2, e3, tpe, loc)

        //
        // Error Expressions.
        //
        case Expression.HoleError(sym, tpe, loc) => Expression.HoleError(sym, tpe, loc)
        case Expression.MatchError(tpe, loc) => Expression.MatchError(tpe, loc)
        case Expression.SwitchError(tpe, loc) => Expression.SwitchError(tpe, loc)

        //
        // Unexpected Expressions.
        //
        case Expression.LambdaClosure(fparams, freeVars, exp, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp.getClass}'.")
        case Expression.Lambda(args, body, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp.getClass}'.")
        case Expression.Apply(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp.getClass}'.")
      }
      LabeledExpression(thisLabel, lExp)
    }

    val labeledDefs = root.defs.map {
      case (sym, defn) => sym -> defn.copy(exp = labelExp(defn.exp))
    }

    def closuresExp(acc: Set[Expression.Closure], exp: SimplifiedAst.Expression): Set[Expression.Closure] =
      exp match {
        //
        // Labeled Expressions.
        //
        case LabeledExpression(_, exp) => closuresExp(acc, exp)

        //
        // Literal Expressions.
        //
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

        //
        // Variable Expressions.
        //
        case Expression.Var(sym, tpe, loc) => acc

        //
        // Def Expressions.
        //
        case Expression.Def(sym, tpe, loc) => acc

        //
        // Eff Expressions.
        //
        case Expression.Eff(sym, tpe, loc) => acc

        //
        // Closure Expressions.
        //
        case Expression.Closure(sym, freeVars, tpe, loc) => acc + Expression.Closure(sym, freeVars, tpe, loc)

        //
        // ApplyClo Expressions.
        //
        case Expression.ApplyClo(exp, args, tpe, loc) =>
          val e = closuresExp(acc, exp)
          val as = args.foldLeft(acc)(closuresExp)
          e ++ as

        //
        // ApplyDef Expressions.
        //
        case Expression.ApplyDef(sym, args, tpe, loc) => args.foldLeft(acc)(closuresExp)

        //
        // ApplyEff Expressions.
        //
        case Expression.ApplyEff(sym, args, tpe, loc) => args.foldLeft(acc)(closuresExp)

        //
        // ApplyCloTail Expressions.
        //
        case Expression.ApplyCloTail(exp, args, tpe, loc) =>
          val e = closuresExp(acc, exp)
          val as = args.foldLeft(acc)(closuresExp)
          e ++ as

        //
        // ApplyDefTail Expressions.
        //
        case Expression.ApplyDefTail(sym, args, tpe, loc) => args.foldLeft(acc)(closuresExp)

        //
        // ApplyEffTail Expressions.
        //
        case Expression.ApplyEffTail(sym, args, tpe, loc) => args.foldLeft(acc)(closuresExp)

        //
        // ApplySelfTail Expressions.
        //
        case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) => actuals.foldLeft(acc)(closuresExp)

        //
        // Unary Expressions.
        //
        case Expression.Unary(sop, op, exp, tpe, loc) => closuresExp(acc, exp)

        //
        // Binary Expressions.
        //
        case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
          val e1 = closuresExp(acc, exp1)
          val e2 = closuresExp(acc, exp2)
          e1 ++ e2

        //
        // If-then-else Expressions.
        //
        case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
          val cond = closuresExp(acc, exp1)
          val consequent = closuresExp(acc, exp2)
          val alternative = closuresExp(acc, exp3)
          cond ++ consequent ++ alternative

        //
        // Block Expressions.
        //
        case Expression.Branch(exp, branches, tpe, loc) =>
          val e = closuresExp(acc, exp)
          val bs = branches.foldLeft(acc) {
            case (clos, (_, br)) => closuresExp(clos, br)
          }
          e ++ bs

        //
        // Jump Expressions.
        //
        case Expression.JumpTo(sym, tpe, loc) => acc

        //
        // Let Expressions.
        //
        case Expression.Let(sym, exp1, exp2, tpe, loc) =>
          // Visit the value expression.
          val e1 = closuresExp(acc, exp1)
          val e2 = closuresExp(acc, exp2)
          e1 ++ e2

        //
        // LetRec Expressions.
        //
        case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
          val e1 = closuresExp(acc, exp1)
          val e2 = closuresExp(acc, exp2)
          e1 ++ e2

        //
        // Is Expressions.
        //
        case Expression.Is(sym, tag, exp, loc) => closuresExp(acc, exp)

        //
        // Tag Expressions.
        //
        case Expression.Tag(sym, tag, exp, tpe, loc) => closuresExp(acc, exp)

        //
        // Untag Expressions.
        //
        case Expression.Untag(sym, tag, exp, tpe, loc) => closuresExp(acc, exp)

        //
        // Index Expressions.
        //
        case Expression.Index(base, offset, tpe, loc) => closuresExp(acc, base)

        //
        // Tuple Expressions.
        //
        case Expression.Tuple(elms, tpe, loc) => elms.foldLeft(acc)(closuresExp)

        //
        // RecordEmpty Expressions.
        //
        case Expression.RecordEmpty(tpe, loc) => acc

        //
        // RecordSelect Expressions.
        //
        case Expression.RecordSelect(exp, label, tpe, loc) => closuresExp(acc, exp)

        //
        // RecordExtend Expressions.
        //
        case Expression.RecordExtend(label, value, rest, tpe, loc) =>
          val v = closuresExp(acc, value)
          val r = closuresExp(acc, rest)
          v ++ r

        //
        // RecordRestrict Expressions.
        //
        case Expression.RecordRestrict(label, rest, tpe, loc) => closuresExp(acc, rest)

        //
        // ArrayLit Expressions.
        //
        case Expression.ArrayLit(elms, tpe, loc) => elms.foldLeft(acc)(closuresExp)

        //
        // ArrayNew Expressions.
        //
        case Expression.ArrayNew(elm, len, tpe, loc) =>
          val e = closuresExp(acc, elm)
          val ln = closuresExp(acc, len)
          e ++ ln

        //
        // ArrayLoad Expressions.
        //
        case Expression.ArrayLoad(base, index, tpe, loc) =>
          val b = closuresExp(acc, base)
          val i = closuresExp(acc, index)
          b ++ i

        //
        // ArrayStore Expressions.
        //
        case Expression.ArrayStore(base, index, elm, tpe, loc) =>
          val b = closuresExp(acc, base)
          val i = closuresExp(acc, index)
          val e = closuresExp(acc, elm)
          b ++ i ++ e

        //
        // ArraySlice Expressions.
        //
        case Expression.ArrayLength(base, tpe, loc) => closuresExp(acc, base)

        //
        // ArraySlice Expressions.
        //
        case Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) =>
          val b = closuresExp(acc, base)
          val i1 = closuresExp(acc, startIndex)
          val i2 = closuresExp(acc, endIndex)
          b ++ i1 ++ i2

        //
        // Reference Expressions.
        //
        case Expression.Ref(exp, tpe, loc) => closuresExp(acc, exp)

        //
        // Dereference Expressions.
        //
        case Expression.Deref(exp, tpe, loc) => closuresExp(acc, exp)

        //
        // Assign Expressions.
        //
        case Expression.Assign(exp1, exp2, tpe, loc) =>
          val e1 = closuresExp(acc, exp1)
          val e2 = closuresExp(acc, exp2)
          e1 ++ e2

        //
        // HandleWith Expressions.
        //
        case Expression.HandleWith(exp, bindings, tpe, loc) =>
          val e = closuresExp(acc, exp)
          val bs = bindings.foldLeft(acc) {
            case (clos, SimplifiedAst.HandlerBinding(_, handler)) => closuresExp(clos, handler)
          }
          e ++ bs

        //
        // Existential Expressions.
        //
        case Expression.Existential(fparam, exp, loc) => closuresExp(acc, exp)

        //
        // Universal Expressions.
        //
        case Expression.Universal(fparam, exp, loc) => closuresExp(acc, exp)

        //
        // Try Catch Constructor.
        //
        case Expression.TryCatch(exp, rules, tpe, loc) =>
          val e = closuresExp(acc, exp)
          val rs = rules.foldLeft(acc) {
            case (clos, SimplifiedAst.CatchRule(_, _, body)) => closuresExp(clos, body)
          }
          e ++ rs

        //
        // Native Constructor.
        //
        case Expression.NativeConstructor(constructor, args, tpe, loc) => args.foldLeft(acc)(closuresExp)

        //
        // Native Field.
        //
        case Expression.NativeField(field, tpe, loc) => acc

        //
        // Native Method.
        //
        case Expression.NativeMethod(method, args, tpe, loc) => args.foldLeft(acc)(closuresExp)

        //
        // New Channel.
        //
        case Expression.NewChannel(exp, tpe, loc) => closuresExp(acc, exp)

        //
        // Get Channel.
        //
        case Expression.GetChannel(exp, tpe, loc) => closuresExp(acc, exp)

        //
        // Put Channel.
        //
        case Expression.PutChannel(exp1, exp2, tpe, loc) =>
          val e1 = closuresExp(acc, exp1)
          val e2 = closuresExp(acc, exp2)
          e1 ++ e2

        //
        // Select Channel.
        //
        case Expression.SelectChannel(rules, default, tpe, loc) =>
          val rs = rules.foldLeft(acc) {
            case (clos, SimplifiedAst.SelectChannelRule(_, chan, exp)) =>
              val c = closuresExp(acc, chan)
              val e = closuresExp(acc, exp)
              c ++ e
          }

          val d = default.fold(Set.empty[Expression.Closure])(closuresExp(acc, _))
          rs ++ d

        //
        // ProcessSpawn.
        //
        case Expression.ProcessSpawn(exp, tpe, loc) => closuresExp(acc, exp)

        //
        // ProcessSleep.
        //
        case Expression.ProcessSleep(exp, tpe, loc) => closuresExp(acc, exp)

        //
        // ProcessPanic.
        //
        case Expression.ProcessPanic(msg, tpe, loc) => acc

        //
        // ConstraintSet.
        //
        case Expression.FixpointConstraintSet(cs0, tpe, loc) => acc

        //
        // Constraint Union.
        //
        case Expression.FixpointCompose(exp1, exp2, tpe, loc) =>
          val e1 = closuresExp(acc, exp1)
          val e2 = closuresExp(acc, exp2)
          e1 ++ e2

        //
        // Fixpoint Solve.
        //
        case Expression.FixpointSolve(exp, stf, tpe, loc) => closuresExp(acc, exp)

        //
        // Fixpoint Project.
        //
        case Expression.FixpointProject(sym, exp, tpe, loc) => closuresExp(acc, exp)

        //
        // Fixpoint Entails.
        //
        case Expression.FixpointEntails(exp1, exp2, tpe, loc) =>
          val e1 = closuresExp(acc, exp1)
          val e2 = closuresExp(acc, exp2)
          e1 ++ e2

        //
        // Fixpoint Fold.
        //
        case Expression.FixpointFold(sym, exp1, exp2, exp3, tpe, loc) =>
          val e1 = closuresExp(acc, exp1)
          val e2 = closuresExp(acc, exp2)
          val e3 = closuresExp(acc, exp3)
          e1 ++ e2 ++ e3

        //
        // Error Expressions.
        //
        case Expression.HoleError(sym, tpe, loc) => acc
        case Expression.MatchError(tpe, loc) => acc
        case Expression.SwitchError(tpe, loc) => acc

        //
        // Unexpected Expressions.
        //
        case Expression.LambdaClosure(fparams, freeVars, exp, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp.getClass}'.")
        case Expression.Lambda(args, body, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp.getClass}'.")
        case Expression.Apply(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp.getClass}'.")
      }

    val closures = labeledDefs.foldLeft(Set.empty[Expression.Closure]){
      case (c, (_, defn)) => closuresExp(c, defn.exp)
    }

    def visitExp(acc: (AbstractEnvironment, AbstractCache), lExp: LabeledExpression): (AbstractEnvironment, AbstractCache) = {
      val result = lExp.tpe.typeConstructor match {
        case Type.Arrow(_, _) =>
          closures
            .filter(clos => clos.tpe == lExp.tpe)
            .foldLeft(acc) {
              case ((varFlow, expFlow), clos) =>
                lExp.exp match {
                  case Expression.Var(sym, _, _) =>
                    val flow = varFlow.getOrElse(sym, Set())
                    val newFlow = varFlow + (sym -> (flow + clos))
                    (newFlow, expFlow)
                  case _ =>
                    val flow = expFlow.getOrElse(lExp, Set())
                    val newFlow = expFlow + (lExp -> (flow + clos))
                    (varFlow, newFlow)
                }
            }
        case _ => acc
      }
      lExp.exp match {
        //
        // Literal Expressions.
        //
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

        //
        // Variable Expressions.
        //
        case Expression.Var(sym, tpe, loc) => result

        //
        // Def Expressions.
        //
        case Expression.Def(sym, tpe, loc) => result

        //
        // Eff Expressions.
        //
        case Expression.Eff(sym, tpe, loc) => result

        //
        // Closure Expressions.
        //
        case Expression.Closure(sym, freeVars, tpe, loc) => result

        //
        // ApplyClo Expressions.
        //
        case Expression.ApplyClo(exp, args, tpe, loc) =>
          val e = visitExp(result, exp)
          val as = args.foldLeft(e)(visitExp(_,_))
          as

        //
        // ApplyDef Expressions.
        //
        case Expression.ApplyDef(sym, args, tpe, loc) => args.foldLeft(result)(visitExp(_,_))

        //
        // ApplyEff Expressions.
        //
        case Expression.ApplyEff(sym, args, tpe, loc) => args.foldLeft(result)(visitExp(_,_))

        //
        // ApplyCloTail Expressions.
        //
        case Expression.ApplyCloTail(exp, args, tpe, loc) =>
          val e = visitExp(result, exp)
          val as = args.foldLeft(e)(visitExp(_,_))
          as

        //
        // ApplyDefTail Expressions.
        //
        case Expression.ApplyDefTail(sym, args, tpe, loc) => args.foldLeft(result)(visitExp(_,_))

        //
        // ApplyEffTail Expressions.
        //
        case Expression.ApplyEffTail(sym, args, tpe, loc) => args.foldLeft(result)(visitExp(_,_))

        //
        // ApplySelfTail Expressions.
        //
        case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) => actuals.foldLeft(result)(visitExp(_,_))

        //
        // Unary Expressions.
        //
        case Expression.Unary(sop, op, exp, tpe, loc) => visitExp(result, exp)

        //
        // Binary Expressions.
        //
        case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
          val e1 = visitExp(result, exp1)
          val e2 = visitExp(e1, exp2)
          e2

        //
        // If-then-else Expressions.
        //
        case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
          val cond = visitExp(result, exp1)
          val consequent = visitExp(cond, exp2)
          val alternative = visitExp(consequent, exp3)
          alternative

        //
        // Block Expressions.
        //
        case Expression.Branch(exp, branches, tpe, loc) =>
          val e = visitExp(result, exp)
          val bs = branches.foldLeft(e) {
            case (res, (_, br)) => visitExp(res, br)
          }
          bs

        //
        // Jump Expressions.
        //
        case Expression.JumpTo(sym, tpe, loc) => result

        //
        // Let Expressions.
        //
        case Expression.Let(sym, exp1, exp2, tpe, loc) =>
          // Visit the value expression.
          val e1 = visitExp(result, exp1)
          val e2 = visitExp(e1, exp2)
          e2

        //
        // LetRec Expressions.
        //
        case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
          val e1 = visitExp(result, exp1)
          val e2 = visitExp(e1, exp2)
          e2

        //
        // Is Expressions.
        //
        case Expression.Is(sym, tag, exp, loc) => visitExp(result, exp)

        //
        // Tag Expressions.
        //
        case Expression.Tag(sym, tag, exp, tpe, loc) => visitExp(result, exp)

        //
        // Untag Expressions.
        //
        case Expression.Untag(sym, tag, exp, tpe, loc) => visitExp(result, exp)

        //
        // Index Expressions.
        //
        case Expression.Index(base, offset, tpe, loc) => visitExp(result, base)

        //
        // Tuple Expressions.
        //
        case Expression.Tuple(elms, tpe, loc) => elms.foldLeft(result)(visitExp(_,_))

        //
        // RecordEmpty Expressions.
        //
        case Expression.RecordEmpty(tpe, loc) => result

        //
        // RecordSelect Expressions.
        //
        case Expression.RecordSelect(exp, label, tpe, loc) => visitExp(result, exp)

        //
        // RecordExtend Expressions.
        //
        case Expression.RecordExtend(label, value, rest, tpe, loc) =>
          val v = visitExp(result, value)
          val r = visitExp(v, rest)
          r

        //
        // RecordRestrict Expressions.
        //
        case Expression.RecordRestrict(label, rest, tpe, loc) => visitExp(result, rest)

        //
        // ArrayLit Expressions.
        //
        case Expression.ArrayLit(elms, tpe, loc) => elms.foldLeft(result)(visitExp(_,_))

        //
        // ArrayNew Expressions.
        //
        case Expression.ArrayNew(elm, len, tpe, loc) =>
          val e = visitExp(result, elm)
          val ln = visitExp(e, len)
          ln

        //
        // ArrayLoad Expressions.
        //
        case Expression.ArrayLoad(base, index, tpe, loc) =>
          val b = visitExp(result, base)
          val i = visitExp(b, index)
          i

        //
        // ArrayStore Expressions.
        //
        case Expression.ArrayStore(base, index, elm, tpe, loc) =>
          val b = visitExp(result, base)
          val i = visitExp(b, index)
          val e = visitExp(i, elm)
          e

        //
        // ArraySlice Expressions.
        //
        case Expression.ArrayLength(base, tpe, loc) => visitExp(result, base)

        //
        // ArraySlice Expressions.
        //
        case Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) =>
          val b = visitExp(result, base)
          val i1 = visitExp(b, startIndex)
          val i2 = visitExp(i1, endIndex)
          i2

        //
        // Reference Expressions.
        //
        case Expression.Ref(exp, tpe, loc) => visitExp(result, exp)

        //
        // Dereference Expressions.
        //
        case Expression.Deref(exp, tpe, loc) => visitExp(result, exp)

        //
        // Assign Expressions.
        //
        case Expression.Assign(exp1, exp2, tpe, loc) =>
          val e1 = visitExp(result, exp1)
          val e2 = visitExp(e1, exp2)
          e2

        //
        // HandleWith Expressions.
        //
        case Expression.HandleWith(exp, bindings, tpe, loc) =>
          val e = visitExp(result, exp)
          val bs = bindings.foldLeft(e) {
            case (res, SimplifiedAst.HandlerBinding(_, handler)) => visitExp(res, handler)
          }
          bs

        //
        // Existential Expressions.
        //
        case Expression.Existential(fparam, exp, loc) => visitExp(result, exp)

        //
        // Universal Expressions.
        //
        case Expression.Universal(fparam, exp, loc) => visitExp(result, exp)

        //
        // Try Catch Constructor.
        //
        case Expression.TryCatch(exp, rules, tpe, loc) =>
          val e = visitExp(result, exp)
          val rs = rules.foldLeft(e) {
            case (res, SimplifiedAst.CatchRule(_, _, body)) => visitExp(res, body)
          }
          rs

        //
        // Native Constructor.
        //
        case Expression.NativeConstructor(constructor, args, tpe, loc) => args.foldLeft(result)(visitExp(_,_))

        //
        // Native Field.
        //
        case Expression.NativeField(field, tpe, loc) => result

        //
        // Native Method.
        //
        case Expression.NativeMethod(method, args, tpe, loc) => args.foldLeft(result)(visitExp(_,_))

        //
        // New Channel.
        //
        case Expression.NewChannel(exp, tpe, loc) => visitExp(result, exp)

        //
        // Get Channel.
        //
        case Expression.GetChannel(exp, tpe, loc) => visitExp(result, exp)

        //
        // Put Channel.
        //
        case Expression.PutChannel(exp1, exp2, tpe, loc) =>
          val e1 = visitExp(result, exp1)
          val e2 = visitExp(e1, exp2)
          e2

        //
        // Select Channel.
        //
        case Expression.SelectChannel(rules, default, tpe, loc) =>
          val rs = rules.foldLeft(result) {
            case (res, SimplifiedAst.SelectChannelRule(_, chan, exp)) =>
              val c = visitExp(res, chan)
              val e = visitExp(c, exp)
              e
          }
          val d = default.fold(rs)(visitExp(rs, _))
          d

        //
        // ProcessSpawn.
        //
        case Expression.ProcessSpawn(exp, tpe, loc) => visitExp(result, exp)

        //
        // ProcessSleep.
        //
        case Expression.ProcessSleep(exp, tpe, loc) => visitExp(result, exp)

        //
        // ProcessPanic.
        //
        case Expression.ProcessPanic(msg, tpe, loc) => result

        //
        // ConstraintSet.
        //
        case Expression.FixpointConstraintSet(cs0, tpe, loc) => result

        //
        // Constraint Union.
        //
        case Expression.FixpointCompose(exp1, exp2, tpe, loc) =>
          val e1 = visitExp(result, exp1)
          val e2 = visitExp(e1, exp2)
          e2

        //
        // Fixpoint Solve.
        //
        case Expression.FixpointSolve(exp, stf, tpe, loc) => visitExp(result, exp)

        //
        // Fixpoint Project.
        //
        case Expression.FixpointProject(sym, exp, tpe, loc) => visitExp(result, exp)

        //
        // Fixpoint Entails.
        //
        case Expression.FixpointEntails(exp1, exp2, tpe, loc) =>
          val e1 = visitExp(result, exp1)
          val e2 = visitExp(e1, exp2)
          e2

        //
        // Fixpoint Fold.
        //
        case Expression.FixpointFold(sym, exp1, exp2, exp3, tpe, loc) =>
          val e1 = visitExp(result, exp1)
          val e2 = visitExp(e1, exp2)
          val e3 = visitExp(e2, exp3)
          e3

        //
        // Error Expressions.
        //
        case Expression.HoleError(sym, tpe, loc) => result
        case Expression.MatchError(tpe, loc) => result
        case Expression.SwitchError(tpe, loc) => result

        //
        // Unexpected Expressions.
        //
        case Expression.LambdaClosure(fparams, freeVars, _, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${lExp.exp.getClass}'.")
        case Expression.Lambda(args, body, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${lExp.exp.getClass}'.")
        case Expression.Apply(_, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${lExp.exp.getClass}'.")
      }
    }

    val (varFlow, expFlow) = labeledDefs.foldLeft((Map(), Map()): (AbstractEnvironment, AbstractCache)) {
      case (acc, (_, defn)) => visitExp(acc, defn.exp)
    }

    def inlineClosApp(lExp: LabeledExpression, tpe: Type, loc: SourceLocation): Option[Symbol.DefnSym] = {
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
        //
        // Labeled Expressions.
        //
        case LabeledExpression(label, exp) => inliner(exp)

        //
        // Literal Expressions.
        //
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

        //
        // Variable Expressions.
        //
        case Expression.Var(sym, tpe, loc) => Expression.Var(sym, tpe, loc)

        //
        // Def Expressions.
        //
        case Expression.Def(sym, tpe, loc) => Expression.Def(sym, tpe, loc)

        //
        // Eff Expressions.
        //
        case Expression.Eff(sym, tpe, loc) => Expression.Eff(sym, tpe, loc)

        //
        // Closure Expressions.
        //
        case Expression.Closure(sym, freeVars, tpe, loc) => Expression.Closure(sym, freeVars, tpe, loc)

        //
        // ApplyClo Expressions.
        //
        case Expression.ApplyClo(exp, args, tpe, loc) =>
          val as = args map inliner
          inlineClosApp(exp, tpe, loc) match {
            case Some(inlined) => Expression.ApplyDef(inlined, as, tpe, loc)
            case None =>
              val e = inliner(exp)
              Expression.ApplyClo(e, as, tpe, loc)
          }

        //
        // ApplyDef Expressions.
        //
        case Expression.ApplyDef(sym, args, tpe, loc) =>
          val as = args map inliner
          Expression.ApplyDef(sym, as, tpe, loc)

        //
        // ApplyEff Expressions.
        //
        case Expression.ApplyEff(sym, args, tpe, loc) =>
          val as = args map inliner
          Expression.ApplyEff(sym, as, tpe, loc)

        //
        // ApplyCloTail Expressions.
        //
        case Expression.ApplyCloTail(exp, args, tpe, loc) =>
          val as = args map inliner
          inlineClosApp(exp, tpe, loc) match {
            case Some(inlined) => Expression.ApplyDefTail(inlined, as, tpe, loc)
            case None =>
              val e = inliner(exp)
              Expression.ApplyCloTail(e, as, tpe, loc)
          }

        //
        // ApplyDefTail Expressions.
        //
        case Expression.ApplyDefTail(sym, args, tpe, loc) =>
          val as = args map inliner
          Expression.ApplyDefTail(sym, as, tpe, loc)

        //
        // ApplyEffTail Expressions.
        //
        case Expression.ApplyEffTail(sym, args, tpe, loc) =>
          val as = args map inliner
          Expression.ApplyEffTail(sym, as, tpe, loc)

        //
        // ApplySelfTail Expressions.
        //
        case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
          val as = actuals map inliner
          Expression.ApplySelfTail(sym, formals, as, tpe, loc)

        //
        // Unary Expressions.
        //
        case Expression.Unary(sop, op, exp, tpe, loc) =>
          val e = inliner(exp)
          Expression.Unary(sop, op, e, tpe, loc)

        //
        // Binary Expressions.
        //
        case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
          val e1 = inliner(exp1)
          val e2 = inliner(exp2)
          Expression.Binary(sop, op, e1, e2, tpe, loc)

        //
        // If-then-else Expressions.
        //
        case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
          val cond = inliner(exp1)
          val consequent = inliner(exp2)
          val alternative = inliner(exp3)
          Expression.IfThenElse(cond, consequent, alternative, tpe, loc)

        //
        // Block Expressions.
        //
        case Expression.Branch(exp, branches, tpe, loc) =>
          val e = inliner(exp)
          val bs = branches map {
            case (sym, br) => sym -> inliner(br)
          }
          Expression.Branch(e, bs, tpe, loc)

        //
        // Jump Expressions.
        //
        case Expression.JumpTo(sym, tpe, loc) =>
          Expression.JumpTo(sym, tpe, loc)

        //
        // Let Expressions.
        //
        case Expression.Let(sym, exp1, exp2, tpe, loc) =>
          // Visit the value expression.
          val e1 = inliner(exp1)
          val e2 = inliner(exp2)
          Expression.Let(sym, e1, e2, tpe, loc)

        //
        // LetRec Expressions.
        //
        case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
          val e1 = inliner(exp1)
          val e2 = inliner(exp2)
          Expression.LetRec(sym, e1, e2, tpe, loc)

        //
        // Is Expressions.
        //
        case Expression.Is(sym, tag, exp, loc) =>
          val e = inliner(exp)
          Expression.Is(sym, tag, e, loc)

        //
        // Tag Expressions.
        //
        case Expression.Tag(sym, tag, exp, tpe, loc) =>
          val e = inliner(exp)
          Expression.Tag(sym, tag, e, tpe, loc)

        //
        // Untag Expressions.
        //
        case Expression.Untag(sym, tag, exp, tpe, loc) =>
          val e = inliner(exp)
          Expression.Untag(sym, tag, e, tpe, loc)

        //
        // Index Expressions.
        //
        case Expression.Index(base, offset, tpe, loc) =>
          val b = inliner(base)
          Expression.Index(b, offset, tpe, loc)

        //
        // Tuple Expressions.
        //
        case Expression.Tuple(elms, tpe, loc) =>
          val es = elms map inliner
          Expression.Tuple(es, tpe, loc)

        //
        // RecordEmpty Expressions.
        //
        case Expression.RecordEmpty(tpe, loc) =>
          Expression.RecordEmpty(tpe, loc)

        //
        // RecordSelect Expressions.
        //
        case Expression.RecordSelect(exp, label, tpe, loc) =>
          val e = inliner(exp)
          Expression.RecordSelect(e, label, tpe, loc)

        //
        // RecordExtend Expressions.
        //
        case Expression.RecordExtend(label, value, rest, tpe, loc) =>
          val v = inliner(value)
          val r = inliner(rest)
          Expression.RecordExtend(label, v, r, tpe, loc)

        //
        // RecordRestrict Expressions.
        //
        case Expression.RecordRestrict(label, rest, tpe, loc) =>
          val r = inliner(rest)
          Expression.RecordRestrict(label, r, tpe, loc)

        //
        // ArrayLit Expressions.
        //
        case Expression.ArrayLit(elms, tpe, loc) =>
          val es = elms map inliner
          Expression.ArrayLit(es, tpe, loc)

        //
        // ArrayNew Expressions.
        //
        case Expression.ArrayNew(elm, len, tpe, loc) =>
          val e = inliner(elm)
          val ln = inliner(len)
          Expression.ArrayNew(e, ln, tpe, loc)

        //
        // ArrayLoad Expressions.
        //
        case Expression.ArrayLoad(base, index, tpe, loc) =>
          val b = inliner(base)
          val i = inliner(index)
          Expression.ArrayLoad(b, i, tpe, loc)

        //
        // ArrayStore Expressions.
        //
        case Expression.ArrayStore(base, index, elm, tpe, loc) =>
          val b = inliner(base)
          val i = inliner(index)
          val e = inliner(elm)
          Expression.ArrayStore(b, i, e, tpe, loc)

        //
        // ArraySlice Expressions.
        //
        case Expression.ArrayLength(base, tpe, loc) =>
          val b = inliner(base)
          Expression.ArrayLength(b, tpe, loc)

        //
        // ArraySlice Expressions.
        //
        case Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) =>
          val b = inliner(base)
          val i1 = inliner(startIndex)
          val i2 = inliner(endIndex)
          Expression.ArraySlice(b, i1, i2, tpe, loc)

        //
        // Reference Expressions.
        //
        case Expression.Ref(exp, tpe, loc) =>
          val e = inliner(exp)
          Expression.Ref(e, tpe, loc)

        //
        // Dereference Expressions.
        //
        case Expression.Deref(exp, tpe, loc) =>
          val e = inliner(exp)
          Expression.Deref(e, tpe, loc)

        //
        // Assign Expressions.
        //
        case Expression.Assign(exp1, exp2, tpe, loc) =>
          val e1 = inliner(exp1)
          val e2 = inliner(exp2)
          Expression.Assign(e1, e2, tpe, loc)

        //
        // HandleWith Expressions.
        //
        case Expression.HandleWith(exp, bindings, tpe, loc) =>
          val e = inliner(exp)
          val bs = bindings map {
            case HandlerBinding(sym, handler) => HandlerBinding(sym, inliner(handler))
          }
          Expression.HandleWith(e, bs, tpe, loc)

        //
        // Existential Expressions.
        //
        case Expression.Existential(fparam, exp, loc) =>
          val e = inliner(exp)
          Expression.Existential(fparam, e, loc)

        //
        // Universal Expressions.
        //
        case Expression.Universal(fparam, exp, loc) =>
          val e = inliner(exp)
          Expression.Universal(fparam, e, loc)

        //
        // Try Catch Constructor.
        //
        case Expression.TryCatch(exp, rules, tpe, loc) =>
          val e = inliner(exp)
          val rs = rules map {
            case CatchRule(sym, clazz, body) =>
              val b = inliner(body)
              CatchRule(sym, clazz, b)
          }
          Expression.TryCatch(e, rs, tpe, loc)

        //
        // Native Constructor.
        //
        case Expression.NativeConstructor(constructor, args, tpe, loc) =>
          val as = args map inliner
          Expression.NativeConstructor(constructor, as, tpe, loc)

        //
        // Native Field.
        //
        case Expression.NativeField(field, tpe, loc) =>
          Expression.NativeField(field, tpe, loc)

        //
        // Native Method.
        //
        case Expression.NativeMethod(method, args, tpe, loc) =>
          val as = args map inliner
          Expression.NativeMethod(method, as, tpe, loc)

        //
        // New Channel.
        //
        case Expression.NewChannel(exp, tpe, loc) =>
          val e = inliner(exp)
          Expression.NewChannel(e, tpe, loc)

        //
        // Get Channel.
        //
        case Expression.GetChannel(exp, tpe, loc) =>
          val e = inliner(exp)
          Expression.GetChannel(e, tpe, loc)

        //
        // Put Channel.
        //
        case Expression.PutChannel(exp1, exp2, tpe, loc) =>
          val e1 = inliner(exp1)
          val e2 = inliner(exp2)
          Expression.PutChannel(e1, e2, tpe, loc)

        //
        // Select Channel.
        //
        case Expression.SelectChannel(rules, default, tpe, loc) =>
          val rs = rules map {
            case SelectChannelRule(sym, chan, exp) =>
              val c = inliner(chan)
              val e = inliner(exp)
              SelectChannelRule(sym, c, e)
          }

          val d = default.map(inliner)

          Expression.SelectChannel(rs, d, tpe, loc)

        //
        // ProcessSpawn.
        //
        case Expression.ProcessSpawn(exp, tpe, loc) =>
          val e = inliner(exp)
          Expression.ProcessSpawn(e, tpe, loc)

        //
        // ProcessSleep.
        //
        case Expression.ProcessSleep(exp, tpe, loc) =>
          val e = inliner(exp)
          Expression.ProcessSleep(e, tpe, loc)

        //
        // ProcessPanic.
        //
        case Expression.ProcessPanic(msg, tpe, loc) =>
          Expression.ProcessPanic(msg, tpe, loc)

        //
        // ConstraintSet.
        //
        case Expression.FixpointConstraintSet(cs0, tpe, loc) => Expression.FixpointConstraintSet(cs0, tpe, loc)

        //
        // Constraint Union.
        //
        case Expression.FixpointCompose(exp1, exp2, tpe, loc) =>
          val e1 = inliner(exp1)
          val e2 = inliner(exp2)
          Expression.FixpointCompose(e1, e2, tpe, loc)

        //
        // Fixpoint Solve.
        //
        case Expression.FixpointSolve(exp, stf, tpe, loc) =>
          val e = inliner(exp)
          Expression.FixpointSolve(e, stf, tpe, loc)

        //
        // Fixpoint Project.
        //
        case Expression.FixpointProject(sym, exp, tpe, loc) =>
          val e = inliner(exp)
          Expression.FixpointProject(sym, e, tpe, loc)

        //
        // Fixpoint Entails.
        //
        case Expression.FixpointEntails(exp1, exp2, tpe, loc) =>
          val e1 = inliner(exp1)
          val e2 = inliner(exp2)
          Expression.FixpointEntails(e1, e2, tpe, loc)

        //
        // Fixpoint Fold.
        //
        case Expression.FixpointFold(sym, exp1, exp2, exp3, tpe, loc) =>
          val e1 = inliner(exp1)
          val e2 = inliner(exp2)
          val e3 = inliner(exp3)
          Expression.FixpointFold(sym, e1, e2, e3, tpe, loc)

        //
        // Error Expressions.
        //
        case Expression.HoleError(sym, tpe, loc) => Expression.HoleError(sym, tpe, loc)
        case Expression.MatchError(tpe, loc) => Expression.MatchError(tpe, loc)
        case Expression.SwitchError(tpe, loc) => Expression.SwitchError(tpe, loc)

        //
        // Unexpected Expressions.
        //
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
