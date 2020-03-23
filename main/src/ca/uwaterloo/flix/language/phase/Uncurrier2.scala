package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.SimplifiedAst.{CatchRule, Def, Expression, SelectChannelRule}
import ca.uwaterloo.flix.language.ast.{SimplifiedAst, Symbol, Type}
import ca.uwaterloo.flix.language.phase.ControlFlowAnalysis.{LabeledExpression, State, renameBoundVars}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

object Uncurrier2 extends Phase[SimplifiedAst.Root, SimplifiedAst.Root] {

  override def run(root: SimplifiedAst.Root)(implicit flix: Flix): Validation[SimplifiedAst.Root, CompilationError] = flix.phase("Uncurrier2") {
    /* Uncurry an arrow type A -> (B -> C) into an arrow (A, B) -> C
     * where A may be multiple types and A, B, C are all proper types.
    */
    def uncurryArrow(tpe0: Type): Type = tpe0 match {
      case Type.Apply(lhs0, Type.Apply(Type.Apply(Type.Arrow(2, eff1), b), c)) => //TODO: arity could be more than 2
        def visitLhs(lhs0: Type): Type = lhs0 match {
          case Type.Arrow(arity0, eff0) =>
            val eff2 = eff1 match {
              case Type.Impure => Type.Impure
              case Type.Pure => eff0
              case Type.Unit => Type.Unit //TODO: What does this mean?
              case _ => throw InternalCompilerException(s"Unexpected effect: '${eff1.getClass}'.")
            }
            Type.Arrow(arity0 + 1, eff2)

          case Type.Apply(lhs0, rhs0) =>
            val lhs1 = visitLhs(lhs0)
            Type.Apply(lhs1, rhs0)

          case _ => throw InternalCompilerException(s"Unexpected type: '${lhs0.getClass}'.")
        }

        val lhs1 = visitLhs(lhs0)
        val lhs2 = Type.Apply(lhs1, b)
        Type.Apply(lhs2, c)

      case _ => throw InternalCompilerException(s"Unexpected type: '${tpe0.getClass}'.")
    }

    def uncurryDefn(defn: (Symbol.DefnSym, Def), acc0: Map[Symbol.DefnSym, Def]): State[Map[Symbol.DefnSym, Symbol.DefnSym], Map[Symbol.DefnSym, Def]] = {
      val (sym0, defn0) = defn
      defn0.exp match {
        case Expression.Lambda(fparams, body, tpe, loc) => State { ctxt0 =>
          val sym1 = Symbol.freshDefnSym(sym0)
          val params0 = defn0.fparams ++ fparams
          val params1 = params0.map(fparam => fparam.copy(sym = Symbol.freshVarSym(fparam.sym)))
          val vars0 = params0.zip(params1).map {
            case (oldParam, newParam) => oldParam.sym -> newParam.sym
          }
          val exp1 = renameBoundVars(body, vars0.toMap, Map.empty)
          val tpe1 = uncurryArrow(defn0.tpe)
          val uncurriedDefn = defn0.copy(sym = sym1, fparams = params1, exp = exp1, tpe = tpe1)
          val result = sym1 -> uncurriedDefn
          val acc1 = acc0 + result
          val (us, ctxt1) = uncurryDefn(result, acc1)(ctxt0)
          val ctxt2 = ctxt1 + (defn0.sym -> sym1)
          (us, ctxt2)
        }

        case _ => State.ret(acc0)
      }
    }

    val (uncurriedDefs0, uncurryCtxt) = flix.subphase("Uncurrying step 0") {
      root.defs.foldLeft((Map.empty[Symbol.DefnSym, Def], Map.empty[Symbol.DefnSym, Symbol.DefnSym])) {
        case ((defs0, acc0), defn) => uncurryDefn(defn, defs0 + defn)(acc0)
      }
    }

    def uncurryApply(exp0: Expression): Expression =
      exp0 match {
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

        case Expression.Def(sym, tpe, loc) => Expression.Def(sym, tpe, loc)

        case Expression.Var(sym, tpe, loc) => Expression.Var(sym, tpe, loc)

        case Expression.Lambda(args, body, tpe, loc) =>
          val b = uncurryApply(body)
          Expression.Lambda(args, b, tpe, loc)

        case Expression.Apply(Expression.Apply(Expression.Def(sym0, tpe0, loc0), args1, tpe1, loc1), args2, tpe2, loc2) =>
          val sym3 = uncurryCtxt(sym0)
          val defn3 = uncurriedDefs0(sym3)
          val def3 = Expression.Def(sym3, defn3.tpe, loc1)
          val as1 = args1 map uncurryApply
          val as2 = args2 map uncurryApply
          Expression.Apply(def3, as1 ++ as2, tpe2, loc2)

        case Expression.Apply(exp, args, tpe, loc) =>
          val e = uncurryApply(exp)
          val as = args map uncurryApply
          val app = Expression.Apply(e, as, tpe, loc)
          app match {
            // Check to see if we can uncurry further
            case Expression.Apply(Expression.Apply(Expression.Def(sym0, tpe0, loc0), args1, tpe1, loc1), args2, tpe2, loc2) =>
              uncurryApply(app)
            case _ => app
          }

        case Expression.Unary(sop, op, exp, tpe, loc) =>
          val e = uncurryApply(exp)
          Expression.Unary(sop, op, e, tpe, loc)

        case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
          val e1 = uncurryApply(exp1)
          val e2 = uncurryApply(exp2)
          Expression.Binary(sop, op, e1, e2, tpe, loc)

        case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
          val cond = uncurryApply(exp1)
          val consequent = uncurryApply(exp2)
          val alternative = uncurryApply(exp3)
          Expression.IfThenElse(cond, consequent, alternative, tpe, loc)

        case Expression.Branch(exp, branches, tpe, loc) =>
          val e = uncurryApply(exp)
          val bs = branches map {
            case (sym, br) =>
              val b = uncurryApply(br)
              (sym -> b)
          }
          Expression.Branch(e, bs, tpe, loc)

        case Expression.JumpTo(sym, tpe, loc) => Expression.JumpTo(sym, tpe, loc)

        case Expression.Let(sym, exp1, exp2, tpe, loc) =>
          val e1 = uncurryApply(exp1)
          val e2 = uncurryApply(exp2)
          Expression.Let(sym, e1, e2, tpe, loc)

        case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
          val e1 = uncurryApply(exp1)
          val e2 = uncurryApply(exp2)
          Expression.LetRec(sym, e1, e2, tpe, loc)

        case Expression.Is(sym, tag, exp, loc) =>
          val e = uncurryApply(exp)
          Expression.Is(sym, tag, e, loc)

        case Expression.Tag(sym, tag, exp, tpe, loc) =>
          val e = uncurryApply(exp)
          Expression.Tag(sym, tag, e, tpe, loc)

        case Expression.Untag(sym, tag, exp, tpe, loc) =>
          val e = uncurryApply(exp)
          Expression.Untag(sym, tag, e, tpe, loc)

        case Expression.Index(base, offset, tpe, loc) =>
          val b = uncurryApply(base)
          Expression.Index(b, offset, tpe, loc)

        case Expression.Tuple(elms, tpe, loc) =>
          val es = elms map uncurryApply
          Expression.Tuple(es, tpe, loc)

        case Expression.RecordEmpty(tpe, loc) => Expression.RecordEmpty(tpe, loc)

        case Expression.RecordSelect(exp, label, tpe, loc) =>
          val e = uncurryApply(exp)
          Expression.RecordSelect(e, label, tpe, loc)

        case Expression.RecordExtend(label, value, rest, tpe, loc) =>
          val v = uncurryApply(value)
          val r = uncurryApply(rest)
          Expression.RecordExtend(label, v, r, tpe, loc)

        case Expression.RecordRestrict(label, rest, tpe, loc) =>
          val r = uncurryApply(rest)
          Expression.RecordRestrict(label, r, tpe, loc)

        case Expression.ArrayLit(elms, tpe, loc) =>
          val es = elms map uncurryApply
          Expression.ArrayLit(es, tpe, loc)

        case Expression.ArrayNew(elm, len, tpe, loc) =>
          val e = uncurryApply(elm)
          val ln = uncurryApply(len)
          Expression.ArrayNew(e, ln, tpe, loc)

        case Expression.ArrayLoad(base, index, tpe, loc) =>
          val b = uncurryApply(base)
          val i = uncurryApply(index)
          Expression.ArrayLoad(b, i, tpe, loc)

        case Expression.ArrayStore(base, index, elm, tpe, loc) =>
          val b = uncurryApply(base)
          val i = uncurryApply(index)
          val e = uncurryApply(elm)
          Expression.ArrayStore(b, i, e, tpe, loc)

        case Expression.ArrayLength(base, tpe, loc) =>
          val b = uncurryApply(base)
          Expression.ArrayLength(b, tpe, loc)

        case Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) =>
          val b = uncurryApply(base)
          val i1 = uncurryApply(startIndex)
          val i2 = uncurryApply(endIndex)
          Expression.ArraySlice(b, i1, i2, tpe, loc)

        case Expression.Ref(exp, tpe, loc) =>
          val e = uncurryApply(exp)
          Expression.Ref(e, tpe, loc)

        case Expression.Deref(exp, tpe, loc) =>
          val e = uncurryApply(exp)
          Expression.Deref(e, tpe, loc)

        case Expression.Assign(exp1, exp2, tpe, loc) =>
          val e1 = uncurryApply(exp1)
          val e2 = uncurryApply(exp2)
          Expression.Assign(e1, e2, tpe, loc)

        case Expression.Existential(fparam, exp, loc) =>
          val e = uncurryApply(exp)
          Expression.Existential(fparam, e, loc)

        case Expression.Universal(fparam, exp, loc) =>
          val e = uncurryApply(exp)
          Expression.Universal(fparam, e, loc)

        case Expression.Cast(exp, tpe, loc) =>
          val e = uncurryApply(exp)
          Expression.Cast(e, tpe, loc)

        case Expression.TryCatch(exp, rules, tpe, loc) =>
          val e = uncurryApply(exp)
          val rs = rules map {
            case CatchRule(sym, clazz, body) =>
              val b = uncurryApply(body)
              CatchRule(sym, clazz, b)
          }
          Expression.TryCatch(e, rs, tpe, loc)

        case Expression.InvokeConstructor(constructor, args, tpe, loc) =>
          val as = args map uncurryApply
          Expression.InvokeConstructor(constructor, as, tpe, loc)

        case Expression.InvokeMethod(method, exp, args, tpe, loc) =>
          val e = uncurryApply(exp)
          val as = args map uncurryApply
          Expression.InvokeMethod(method, e, as, tpe, loc)

        case Expression.InvokeStaticMethod(method, args, tpe, loc) =>
          val as = args map uncurryApply
          Expression.InvokeStaticMethod(method, as, tpe, loc)

        case Expression.GetField(field, exp, tpe, loc) =>
          val e = uncurryApply(exp)
          Expression.GetField(field, e, tpe, loc)

        case Expression.PutField(field, exp1, exp2, tpe, loc) =>
          val e1 = uncurryApply(exp1)
          val e2 = uncurryApply(exp2)
          Expression.PutField(field, e1, e2, tpe, loc)

        case Expression.GetStaticField(field, tpe, loc) => Expression.GetStaticField(field, tpe, loc)

        case Expression.PutStaticField(field, exp, tpe, loc) =>
          val e = uncurryApply(exp)
          Expression.PutStaticField(field, e, tpe, loc)

        case Expression.NewChannel(exp, tpe, loc) =>
          val e = uncurryApply(exp)
          Expression.NewChannel(e, tpe, loc)

        case Expression.GetChannel(exp, tpe, loc) =>
          val e = uncurryApply(exp)
          Expression.GetChannel(e, tpe, loc)

        case Expression.PutChannel(exp1, exp2, tpe, loc) =>
          val e1 = uncurryApply(exp1)
          val e2 = uncurryApply(exp2)
          Expression.PutChannel(e1, e2, tpe, loc)

        case Expression.SelectChannel(rules, default, tpe, loc) =>
          val rs = rules map {
            case SelectChannelRule(sym, chan, exp) =>
              val c = uncurryApply(chan)
              val e = uncurryApply(exp)
              SelectChannelRule(sym, c, e)
          }
          val d = default.map(uncurryApply)
          Expression.SelectChannel(rs, d, tpe, loc)

        case Expression.ProcessSpawn(exp, tpe, loc) =>
          val e = uncurryApply(exp)
          Expression.ProcessSpawn(e, tpe, loc)

        case Expression.ProcessPanic(msg, tpe, loc) => Expression.ProcessPanic(msg, tpe, loc)

        case Expression.FixpointConstraintSet(cs0, tpe, loc) => Expression.FixpointConstraintSet(cs0, tpe, loc)

        case Expression.FixpointCompose(exp1, exp2, tpe, loc) =>
          val e1 = uncurryApply(exp1)
          val e2 = uncurryApply(exp2)
          Expression.FixpointCompose(e1, e2, tpe, loc)

        case Expression.FixpointSolve(exp, stf, tpe, loc) =>
          val e = uncurryApply(exp)
          Expression.FixpointSolve(e, stf, tpe, loc)

        case Expression.FixpointProject(sym, exp, tpe, loc) =>
          val e = uncurryApply(exp)
          Expression.FixpointProject(sym, e, tpe, loc)

        case Expression.FixpointEntails(exp1, exp2, tpe, loc) =>
          val e1 = uncurryApply(exp1)
          val e2 = uncurryApply(exp2)
          Expression.FixpointEntails(e1, e2, tpe, loc)

        case Expression.FixpointFold(sym, exp1, exp2, exp3, tpe, loc) =>
          val e1 = uncurryApply(exp1)
          val e2 = uncurryApply(exp2)
          val e3 = uncurryApply(exp3)
          Expression.FixpointFold(sym, e1, e2, e3, tpe, loc)

        case Expression.HoleError(sym, tpe, loc) => Expression.HoleError(sym, tpe, loc)
        case Expression.MatchError(tpe, loc) => Expression.MatchError(tpe, loc)

        case Expression.LambdaClosure(fparams, freeVars, exp, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.Closure(sym, freeVars, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplyClo(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplyDef(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplyCloTail(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplyDefTail(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case LabeledExpression(label, exp) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
      }

    val uncurriedDefs1 = flix.subphase("Uncurrying step 1") {
      uncurriedDefs0 map {
        case (sym0, defn0) =>
          val body = uncurryApply(defn0.exp)
          val defn1 = defn0.copy(exp = body)
          sym0 -> defn1
      }
    }

    val result = root.copy(defs = uncurriedDefs1)

    result.toSuccess
  }
}
