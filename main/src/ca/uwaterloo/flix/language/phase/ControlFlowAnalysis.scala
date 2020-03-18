package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.SimplifiedAst._
import ca.uwaterloo.flix.language.ast.ops.SimplifiedAstOps
import ca.uwaterloo.flix.language.ast.{SimplifiedAst, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.debug.PrettyPrinter
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.vt.TerminalContext
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

object ControlFlowAnalysis extends Phase[SimplifiedAst.Root, SimplifiedAst.Root] {

  type AbstractCache = Map[LabeledExpression, Set[Expression.Lambda]]
  type AbstractEnvironment = Map[Symbol.VarSym, Set[Expression.Lambda]]
  type InlineContext = (Set[SourceLocation], Map[Symbol.VarSym, Symbol.VarSym], Map[Symbol.LabelSym, Symbol.LabelSym])

  def run(root: SimplifiedAst.Root)(implicit flix: Flix): Validation[SimplifiedAst.Root, CompilationError] = flix.phase("CFA") {
    def uncurryDefn(defn: (Symbol.DefnSym, Def), acc0: List[(Symbol.DefnSym, Def)]): State[Map[Symbol.DefnSym, Symbol.DefnSym], List[(Symbol.DefnSym, Def)]] = {
      val (sym0, defn0) = defn
      defn0.exp match {
        case Expression.Lambda(fparams, body, tpe, loc) => State { ctxt0 =>
          val sym1 = Symbol.freshDefnSym(sym0)
          val params0 = defn0.fparams ++ fparams
          val params1 = params0.map(fparam => fparam.copy(sym = Symbol.freshVarSym(fparam.sym)))
          val vars0 = params0.zip(params1).map {
            case (oldParam, newParam) => oldParam.sym -> newParam.sym
          }
          val b = renameBoundVars(body, vars0.toMap, Map.empty)
          val uncurriedDefn = defn0.copy(sym = sym1, fparams = params1, exp = b) // TODO: What is its type?
          val result = sym1 -> uncurriedDefn
          val acc1 = result :: acc0
          val (us, ctxt1) = uncurryDefn(result, acc1)(ctxt0)
          val ctxt2 = ctxt1 + (defn0.sym -> sym1)
          (us, ctxt2)
        }

        case _ => State.ret(acc0)
      }
    }

    val (uncurriedDefs, uncurryCtxt) = flix.subphase("Uncurrying") {
      root.defs.foldLeft((List.empty[(Symbol.DefnSym, Def)], Map.empty[Symbol.DefnSym, Symbol.DefnSym])) {
        case ((defs0, acc0), defn) =>
          val (defs1, acc1) = uncurryDefn(defn, List(defn))(acc0)
          (defs0 ++ defs1, acc1)
      }
    }

    def labelExp(exp0: Expression): State[Int, LabeledExpression] = State { label =>
      val labeler: State[Int, Expression] = exp0 match {
        case Expression.Unit => State.ret(exp0)
        case Expression.True => State.ret(exp0)
        case Expression.False => State.ret(exp0)
        case Expression.Char(lit) => State.ret(exp0)
        case Expression.Float32(lit) => State.ret(exp0)
        case Expression.Float64(lit) => State.ret(exp0)
        case Expression.Int8(lit) => State.ret(exp0)
        case Expression.Int16(lit) => State.ret(exp0)
        case Expression.Int32(lit) => State.ret(exp0)
        case Expression.Int64(lit) => State.ret(exp0)
        case Expression.BigInt(lit) => State.ret(exp0)
        case Expression.Str(lit) => State.ret(exp0)

        case Expression.Def(sym, tpe, loc) => State.ret(Expression.Def(sym, tpe, loc))

        case Expression.Var(sym, tpe, loc) => State.ret(Expression.Var(sym, tpe, loc))

        case Expression.Lambda(args, body, tpe, loc) => for {
          b <- labelExp(body)
        } yield Expression.Lambda(args, b, tpe, loc)

        case Expression.Apply(exp, args, tpe, loc) => for {
          e <- labelExp(exp)
          as <- State.sequence(args map labelExp)
        } yield Expression.Apply(e, as, tpe, loc)

        case Expression.Unary(sop, op, exp, tpe, loc) => for {
          e <- labelExp(exp)
        } yield Expression.Unary(sop, op, e, tpe, loc)

        case Expression.Binary(sop, op, exp1, exp2, tpe, loc) => for {
          e1 <- labelExp(exp1)
          e2 <- labelExp(exp2)
        } yield Expression.Binary(sop, op, e1, e2, tpe, loc)

        case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) => for {
          cond <- labelExp(exp1)
          consequent <- labelExp(exp2)
          alternative <- labelExp(exp3)
        } yield Expression.IfThenElse(cond, consequent, alternative, tpe, loc)

        case Expression.Branch(exp, branches, tpe, loc) => for {
          e <- labelExp(exp)
          bs <- State.sequence(branches.toList map {
            case (sym, br) => for {
              b <- labelExp(br)
            } yield (sym, b)
          })
        } yield Expression.Branch(e, bs.toMap, tpe, loc)

        case Expression.JumpTo(sym, tpe, loc) => State.ret(Expression.JumpTo(sym, tpe, loc))

        case Expression.Let(sym, exp1, exp2, tpe, loc) => for {
          e1 <- labelExp(exp1)
          e2 <- labelExp(exp2)
        } yield Expression.Let(sym, e1, e2, tpe, loc)

        case Expression.LetRec(sym, exp1, exp2, tpe, loc) => for {
          e1 <- labelExp(exp1)
          e2 <- labelExp(exp2)
        } yield Expression.LetRec(sym, e1, e2, tpe, loc)

        case Expression.Is(sym, tag, exp, loc) => for {
          e <- labelExp(exp)
        } yield Expression.Is(sym, tag, e, loc)

        case Expression.Tag(sym, tag, exp, tpe, loc) => for {
          e <- labelExp(exp)
        } yield Expression.Tag(sym, tag, e, tpe, loc)

        case Expression.Untag(sym, tag, exp, tpe, loc) => for {
          e <- labelExp(exp)
        } yield Expression.Untag(sym, tag, e, tpe, loc)

        case Expression.Index(base, offset, tpe, loc) => for {
          b <- labelExp(base)
        } yield Expression.Index(b, offset, tpe, loc)

        case Expression.Tuple(elms, tpe, loc) => for {
          es <- State.sequence(elms map labelExp)
        } yield Expression.Tuple(es, tpe, loc)

        case Expression.RecordEmpty(tpe, loc) => State.ret(Expression.RecordEmpty(tpe, loc))

        case Expression.RecordSelect(exp, label, tpe, loc) => for {
          e <- labelExp(exp)
        } yield Expression.RecordSelect(e, label, tpe, loc)

        case Expression.RecordExtend(label, value, rest, tpe, loc) => for {
          v <- labelExp(value)
          r <- labelExp(rest)
        } yield Expression.RecordExtend(label, v, r, tpe, loc)

        case Expression.RecordRestrict(label, rest, tpe, loc) => for {
          r <- labelExp(rest)
        } yield Expression.RecordRestrict(label, r, tpe, loc)

        case Expression.ArrayLit(elms, tpe, loc) => for {
          es <- State.sequence(elms map labelExp)
        } yield Expression.ArrayLit(es, tpe, loc)

        case Expression.ArrayNew(elm, len, tpe, loc) => for {
          e <- labelExp(elm)
          ln <- labelExp(len)
        } yield Expression.ArrayNew(e, ln, tpe, loc)

        case Expression.ArrayLoad(base, index, tpe, loc) => for {
          b <- labelExp(base)
          i <- labelExp(index)
        } yield Expression.ArrayLoad(b, i, tpe, loc)

        case Expression.ArrayStore(base, index, elm, tpe, loc) => for {
          b <- labelExp(base)
          i <- labelExp(index)
          e <- labelExp(elm)
        } yield Expression.ArrayStore(b, i, e, tpe, loc)

        case Expression.ArrayLength(base, tpe, loc) => for {
          b <- labelExp(base)
        } yield Expression.ArrayLength(b, tpe, loc)

        case Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) => for {
          b <- labelExp(base)
          i1 <- labelExp(startIndex)
          i2 <- labelExp(endIndex)
        } yield Expression.ArraySlice(b, i1, i2, tpe, loc)

        case Expression.Ref(exp, tpe, loc) => for {
          e <- labelExp(exp)
        } yield Expression.Ref(e, tpe, loc)

        case Expression.Deref(exp, tpe, loc) => for {
          e <- labelExp(exp)
        } yield Expression.Deref(e, tpe, loc)

        case Expression.Assign(exp1, exp2, tpe, loc) => for {
          e1 <- labelExp(exp1)
          e2 <- labelExp(exp2)
        } yield Expression.Assign(e1, e2, tpe, loc)

        case Expression.Existential(fparam, exp, loc) => for {
          e <- labelExp(exp)
        } yield Expression.Existential(fparam, e, loc)

        case Expression.Universal(fparam, exp, loc) => for {
          e <- labelExp(exp)
        } yield Expression.Universal(fparam, e, loc)

        case Expression.Cast(exp, tpe, loc) => for {
          e <- labelExp(exp)
        } yield Expression.Cast(e, tpe, loc)

        case Expression.TryCatch(exp, rules, tpe, loc) => for {
          e <- labelExp(exp)
          rs <- State.sequence(rules map {
            case CatchRule(sym, clazz, body) => for {
              b <- labelExp(body)
            } yield CatchRule(sym, clazz, b)
          })
        } yield Expression.TryCatch(e, rs, tpe, loc)

        case Expression.InvokeConstructor(constructor, args, tpe, loc) => for {
          as <- State.sequence(args map labelExp)
        } yield Expression.InvokeConstructor(constructor, as, tpe, loc)

        case Expression.InvokeMethod(method, exp, args, tpe, loc) => for {
          e <- labelExp(exp)
          as <- State.sequence(args map labelExp)
        } yield Expression.InvokeMethod(method, e, as, tpe, loc)

        case Expression.InvokeStaticMethod(method, args, tpe, loc) => for {
          as <- State.sequence(args map labelExp)
        } yield Expression.InvokeStaticMethod(method, as, tpe, loc)

        case Expression.GetField(field, exp, tpe, loc) => for {
          e <- labelExp(exp)
        } yield Expression.GetField(field, e, tpe, loc)

        case Expression.PutField(field, exp1, exp2, tpe, loc) => for {
          e1 <- labelExp(exp1)
          e2 <- labelExp(exp2)
        } yield Expression.PutField(field, e1, e2, tpe, loc)

        case Expression.GetStaticField(field, tpe, loc) => State.ret(Expression.GetStaticField(field, tpe, loc))

        case Expression.PutStaticField(field, exp, tpe, loc) => for {
          e <- labelExp(exp)
        } yield Expression.PutStaticField(field, e, tpe, loc)

        case Expression.NewChannel(exp, tpe, loc) => for {
          e <- labelExp(exp)
        } yield Expression.NewChannel(e, tpe, loc)

        case Expression.GetChannel(exp, tpe, loc) => for {
          e <- labelExp(exp)
        } yield Expression.GetChannel(e, tpe, loc)

        case Expression.PutChannel(exp1, exp2, tpe, loc) => for {
          e1 <- labelExp(exp1)
          e2 <- labelExp(exp2)
        } yield Expression.PutChannel(e1, e2, tpe, loc)

        case Expression.SelectChannel(rules, default, tpe, loc) => for {
          rs <- State.sequence(rules map {
            case SelectChannelRule(sym, chan, exp) => for {
              c <- labelExp(chan)
              e <- labelExp(exp)
            } yield SelectChannelRule(sym, c, e)
          })
          d <- State.mapOpt(default.map(labelExp))
        } yield Expression.SelectChannel(rs, d, tpe, loc)

        case Expression.ProcessSpawn(exp, tpe, loc) => for {
          e <- labelExp(exp)
        } yield Expression.ProcessSpawn(e, tpe, loc)

        case Expression.ProcessPanic(msg, tpe, loc) => State.ret(Expression.ProcessPanic(msg, tpe, loc))

        case Expression.FixpointConstraintSet(cs0, tpe, loc) => State.ret(Expression.FixpointConstraintSet(cs0, tpe, loc))

        case Expression.FixpointCompose(exp1, exp2, tpe, loc) => for {
          e1 <- labelExp(exp1)
          e2 <- labelExp(exp2)
        } yield Expression.FixpointCompose(e1, e2, tpe, loc)

        case Expression.FixpointSolve(exp, stf, tpe, loc) => for {
          e <- labelExp(exp)
        } yield Expression.FixpointSolve(e, stf, tpe, loc)

        case Expression.FixpointProject(sym, exp, tpe, loc) => for {
          e <- labelExp(exp)
        } yield Expression.FixpointProject(sym, e, tpe, loc)

        case Expression.FixpointEntails(exp1, exp2, tpe, loc) => for {
          e1 <- labelExp(exp1)
          e2 <- labelExp(exp2)
        } yield Expression.FixpointEntails(e1, e2, tpe, loc)

        case Expression.FixpointFold(sym, exp1, exp2, exp3, tpe, loc) => for {
          e1 <- labelExp(exp1)
          e2 <- labelExp(exp2)
          e3 <- labelExp(exp3)
        } yield Expression.FixpointFold(sym, e1, e2, e3, tpe, loc)

        case Expression.HoleError(sym, tpe, loc) => State.ret(Expression.HoleError(sym, tpe, loc))
        case Expression.MatchError(tpe, loc) => State.ret(Expression.MatchError(tpe, loc))

        case Expression.LambdaClosure(fparams, freeVars, exp, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.Closure(sym, freeVars, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplyClo(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplyDef(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplyCloTail(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplyDefTail(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case LabeledExpression(label, exp) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
      }
      val (exp, newLabel) = labeler(label)
      val lExp = LabeledExpression(newLabel, exp)
      (lExp, newLabel + 1)
    }

    val (labeledDefs, _) = flix.subphase("Labeling") {
      State.sequence(uncurriedDefs map {
        case (sym, defn) => for {
          e <- labelExp(defn.exp)
        } yield (sym -> defn.copy(exp = e))
      })(0)
    }

    def lambdasExp(exp0: Expression): State[Map[Type, Set[Expression.Lambda]], Unit] =
      exp0 match {
        case LabeledExpression(label, exp) => for {
          _ <- lambdasExp(exp)
        } yield ()

        case Expression.Unit => State.ret(())
        case Expression.True => State.ret(())
        case Expression.False => State.ret(())
        case Expression.Char(lit) => State.ret(())
        case Expression.Float32(lit) => State.ret(())
        case Expression.Float64(lit) => State.ret(())
        case Expression.Int8(lit) => State.ret(())
        case Expression.Int16(lit) => State.ret(())
        case Expression.Int32(lit) => State.ret(())
        case Expression.Int64(lit) => State.ret(())
        case Expression.BigInt(lit) => State.ret(())
        case Expression.Str(lit) => State.ret(())

        case Expression.Def(sym, tpe, loc) => State.ret(())

        case Expression.Var(sym, tpe, loc) => State.ret(())

        case Expression.Lambda(args, body, tpe, loc) => State { lam0 =>
          val (_, lam1) = lambdasExp(body).map(_ => ())(lam0)
          val lams = lam1.getOrElse(tpe, Set.empty)
          val result = lam1 + (tpe -> (lams + Expression.Lambda(args, body, tpe, loc)))
          ((), result)
        }

        case Expression.Apply(exp, args, tpe, loc) => for {
          _ <- lambdasExp(exp)
          _ <- State.sequence(args map lambdasExp)
        } yield ()

        case Expression.Unary(sop, op, exp, tpe, loc) => for {
          _ <- lambdasExp(exp)
        } yield ()

        case Expression.Binary(sop, op, exp1, exp2, tpe, loc) => for {
          _ <- lambdasExp(exp1)
          _ <- lambdasExp(exp2)
        } yield ()

        case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) => for {
          _ <- lambdasExp(exp1)
          _ <- lambdasExp(exp2)
          _ <- lambdasExp(exp3)
        } yield ()

        case Expression.Branch(exp, branches, tpe, loc) => for {
          _ <- lambdasExp(exp)
          _ <- State.sequence(branches.toList map {
            case (sym, br) => for {
              _ <- lambdasExp(br)
            } yield ()
          })
        } yield ()

        case Expression.JumpTo(sym, tpe, loc) => State.ret(())

        case Expression.Let(sym, exp1, exp2, tpe, loc) => for {
          _ <- lambdasExp(exp1)
          _ <- lambdasExp(exp2)
        } yield ()

        case Expression.LetRec(sym, exp1, exp2, tpe, loc) => for {
          _ <- lambdasExp(exp1)
          _ <- lambdasExp(exp2)
        } yield ()

        case Expression.Is(sym, tag, exp, loc) => for {
          _ <- lambdasExp(exp)
        } yield ()

        case Expression.Tag(sym, tag, exp, tpe, loc) => for {
          _ <- lambdasExp(exp)
        } yield ()

        case Expression.Untag(sym, tag, exp, tpe, loc) => for {
          _ <- lambdasExp(exp)
        } yield ()

        case Expression.Index(base, offset, tpe, loc) => for {
          _ <- lambdasExp(base)
        } yield ()

        case Expression.Tuple(elms, tpe, loc) => for {
          _ <- State.sequence(elms map lambdasExp)
        } yield ()

        case Expression.RecordEmpty(tpe, loc) => State.ret(())

        case Expression.RecordSelect(exp, label, tpe, loc) => for {
          _ <- lambdasExp(exp)
        } yield ()

        case Expression.RecordExtend(label, value, rest, tpe, loc) => for {
          _ <- lambdasExp(value)
          _ <- lambdasExp(rest)
        } yield ()

        case Expression.RecordRestrict(label, rest, tpe, loc) => for {
          _ <- lambdasExp(rest)
        } yield ()

        case Expression.ArrayLit(elms, tpe, loc) => for {
          _ <- State.sequence(elms map lambdasExp)
        } yield ()

        case Expression.ArrayNew(elm, len, tpe, loc) => for {
          _ <- lambdasExp(elm)
          _ <- lambdasExp(len)
        } yield ()

        case Expression.ArrayLoad(base, index, tpe, loc) => for {
          _ <- lambdasExp(base)
          _ <- lambdasExp(index)
        } yield ()

        case Expression.ArrayStore(base, index, elm, tpe, loc) => for {
          _ <- lambdasExp(base)
          _ <- lambdasExp(index)
          _ <- lambdasExp(elm)
        } yield ()

        case Expression.ArrayLength(base, tpe, loc) => for {
          _ <- lambdasExp(base)
        } yield ()

        case Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) => for {
          _ <- lambdasExp(base)
          _ <- lambdasExp(startIndex)
          _ <- lambdasExp(endIndex)
        } yield ()

        case Expression.Ref(exp, tpe, loc) => for {
          _ <- lambdasExp(exp)
        } yield ()

        case Expression.Deref(exp, tpe, loc) => for {
          _ <- lambdasExp(exp)
        } yield ()

        case Expression.Assign(exp1, exp2, tpe, loc) => for {
          _ <- lambdasExp(exp1)
          _ <- lambdasExp(exp2)
        } yield ()

        case Expression.Existential(fparam, exp, loc) => for {
          _ <- lambdasExp(exp)
        } yield ()

        case Expression.Universal(fparam, exp, loc) => for {
          _ <- lambdasExp(exp)
        } yield ()

        case Expression.Cast(exp, tpe, loc) => for {
          _ <- lambdasExp(exp)
        } yield ()

        case Expression.TryCatch(exp, rules, tpe, loc) => for {
          _ <- lambdasExp(exp)
          _ <- State.sequence(rules map {
            case CatchRule(sym, clazz, body) => for {
              _ <- lambdasExp(body)
            } yield ()
          })
        } yield ()

        case Expression.InvokeConstructor(constructor, args, tpe, loc) => for {
          _ <- State.sequence(args map lambdasExp)
        } yield ()

        case Expression.InvokeMethod(method, exp, args, tpe, loc) => for {
          _ <- lambdasExp(exp)
          _ <- State.sequence(args map lambdasExp)
        } yield ()

        case Expression.InvokeStaticMethod(method, args, tpe, loc) => for {
          _ <- State.sequence(args map lambdasExp)
        } yield ()

        case Expression.GetField(field, exp, tpe, loc) => for {
          _ <- lambdasExp(exp)
        } yield ()

        case Expression.PutField(field, exp1, exp2, tpe, loc) => for {
          _ <- lambdasExp(exp1)
          _ <- lambdasExp(exp2)
        } yield ()

        case Expression.GetStaticField(field, tpe, loc) => State.ret(())

        case Expression.PutStaticField(field, exp, tpe, loc) => for {
          _ <- lambdasExp(exp)
        } yield ()

        case Expression.NewChannel(exp, tpe, loc) => for {
          _ <- lambdasExp(exp)
        } yield ()

        case Expression.GetChannel(exp, tpe, loc) => for {
          _ <- lambdasExp(exp)
        } yield ()

        case Expression.PutChannel(exp1, exp2, tpe, loc) => for {
          _ <- lambdasExp(exp1)
          _ <- lambdasExp(exp2)
        } yield ()

        case Expression.SelectChannel(rules, default, tpe, loc) => for {
          _ <- State.sequence(rules map {
            case SelectChannelRule(sym, chan, exp) => for {
              _ <- lambdasExp(chan)
              _ <- lambdasExp(exp)
            } yield ()
          })
          _ <- State.mapOpt(default.map(lambdasExp))
        } yield ()

        case Expression.ProcessSpawn(exp, tpe, loc) => for {
          _ <- lambdasExp(exp)
        } yield ()

        case Expression.ProcessPanic(msg, tpe, loc) => State.ret(())

        case Expression.FixpointConstraintSet(cs0, tpe, loc) => State.ret(())

        case Expression.FixpointCompose(exp1, exp2, tpe, loc) => for {
          _ <- lambdasExp(exp1)
          _ <- lambdasExp(exp2)
        } yield ()

        case Expression.FixpointSolve(exp, stf, tpe, loc) => for {
          _ <- lambdasExp(exp)
        } yield ()

        case Expression.FixpointProject(sym, exp, tpe, loc) => for {
          _ <- lambdasExp(exp)
        } yield ()

        case Expression.FixpointEntails(exp1, exp2, tpe, loc) => for {
          _ <- lambdasExp(exp1)
          _ <- lambdasExp(exp2)
        } yield ()

        case Expression.FixpointFold(sym, exp1, exp2, exp3, tpe, loc) => for {
          _ <- lambdasExp(exp1)
          _ <- lambdasExp(exp2)
          _ <- lambdasExp(exp3)
        } yield ()

        case Expression.HoleError(sym, tpe, loc) => State.ret(())
        case Expression.MatchError(tpe, loc) => State.ret(())

        case Expression.LambdaClosure(fparams, freeVars, exp, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.Closure(sym, freeVars, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplyClo(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplyDef(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplyCloTail(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplyDefTail(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
      }

    val (_, lambdas) = flix.subphase("Lambda discovery") {
      State.sequence(labeledDefs map {
        case (_, defn) => lambdasExp(defn.exp)
      })(Map.empty)
    }

    def visitExp(lExp: LabeledExpression): State[(AbstractEnvironment, AbstractCache), Unit] = State { acc =>
      val visitor: State[(AbstractEnvironment, AbstractCache), Unit] = lExp.exp match {
        case Expression.Unit => State.ret(())
        case Expression.True => State.ret(())
        case Expression.False => State.ret(())
        case Expression.Char(lit) => State.ret(())
        case Expression.Float32(lit) => State.ret(())
        case Expression.Float64(lit) => State.ret(())
        case Expression.Int8(lit) => State.ret(())
        case Expression.Int16(lit) => State.ret(())
        case Expression.Int32(lit) => State.ret(())
        case Expression.Int64(lit) => State.ret(())
        case Expression.BigInt(lit) => State.ret(())
        case Expression.Str(lit) => State.ret(())

        case Expression.Def(sym, tpe, loc) => State.ret(())

        case Expression.Var(sym, tpe, loc) => State.ret(())

        case Expression.Lambda(args, body, tpe, loc) => for {
          _ <- visitExp(body)
        } yield ()

        case Expression.Apply(exp, args, tpe, loc) => for {
          _ <- visitExp(exp)
          _ <- State.sequence(args map (a => visitExp(a)))
        } yield ()

        case Expression.Unary(sop, op, exp, tpe, loc) => for {
          _ <- visitExp(exp)
        } yield ()

        case Expression.Binary(sop, op, exp1, exp2, tpe, loc) => for {
          _ <- visitExp(exp1)
          _ <- visitExp(exp2)
        } yield ()

        case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) => for {
          _ <- visitExp(exp1)
          _ <- visitExp(exp2)
          _ <- visitExp(exp3)
        } yield ()

        case Expression.Branch(exp, branches, tpe, loc) => for {
          _ <- visitExp(exp)
          _ <- State.sequence(branches.toList map {
            case (sym, br) => for {
              _ <- visitExp(br)
            } yield ()
          })
        } yield ()

        case Expression.JumpTo(sym, tpe, loc) => State.ret(())

        case Expression.Let(sym, exp1, exp2, tpe, loc) => for {
          _ <- visitExp(exp1)
          _ <- visitExp(exp2)
        } yield ()

        case Expression.LetRec(sym, exp1, exp2, tpe, loc) => for {
          _ <- visitExp(exp1)
          _ <- visitExp(exp2)
        } yield ()

        case Expression.Is(sym, tag, exp, loc) => for {
          _ <- visitExp(exp)
        } yield ()

        case Expression.Tag(sym, tag, exp, tpe, loc) => for {
          _ <- visitExp(exp)
        } yield ()

        case Expression.Untag(sym, tag, exp, tpe, loc) => for {
          _ <- visitExp(exp)
        } yield ()

        case Expression.Index(base, offset, tpe, loc) => for {
          _ <- visitExp(base)
        } yield ()

        case Expression.Tuple(elms, tpe, loc) => for {
          _ <- State.sequence(elms map (e => visitExp(e)))
        } yield ()

        case Expression.RecordEmpty(tpe, loc) => State.ret(())

        case Expression.RecordSelect(exp, label, tpe, loc) => for {
          _ <- visitExp(exp)
        } yield ()

        case Expression.RecordExtend(label, value, rest, tpe, loc) => for {
          _ <- visitExp(value)
          _ <- visitExp(rest)
        } yield ()

        case Expression.RecordRestrict(label, rest, tpe, loc) => for {
          _ <- visitExp(rest)
        } yield ()

        case Expression.ArrayLit(elms, tpe, loc) => for {
          _ <- State.sequence(elms map (e => visitExp(e)))
        } yield ()

        case Expression.ArrayNew(elm, len, tpe, loc) => for {
          _ <- visitExp(elm)
          _ <- visitExp(len)
        } yield ()

        case Expression.ArrayLoad(base, index, tpe, loc) => for {
          _ <- visitExp(base)
          _ <- visitExp(index)
        } yield ()

        case Expression.ArrayStore(base, index, elm, tpe, loc) => for {
          _ <- visitExp(base)
          _ <- visitExp(index)
          _ <- visitExp(elm)
        } yield ()

        case Expression.ArrayLength(base, tpe, loc) => for {
          _ <- visitExp(base)
        } yield ()

        case Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) => for {
          _ <- visitExp(base)
          _ <- visitExp(startIndex)
          _ <- visitExp(endIndex)
        } yield ()

        case Expression.Ref(exp, tpe, loc) => for {
          _ <- visitExp(exp)
        } yield ()

        case Expression.Deref(exp, tpe, loc) => for {
          _ <- visitExp(exp)
        } yield ()

        case Expression.Assign(exp1, exp2, tpe, loc) => for {
          _ <- visitExp(exp1)
          _ <- visitExp(exp2)
        } yield ()

        case Expression.Existential(fparam, exp, loc) => for {
          _ <- visitExp(exp)
        } yield ()

        case Expression.Universal(fparam, exp, loc) => for {
          _ <- visitExp(exp)
        } yield ()

        case Expression.Cast(exp, tpe, loc) => for {
          _ <- visitExp(exp)
        } yield ()

        case Expression.TryCatch(exp, rules, tpe, loc) => for {
          _ <- visitExp(exp)
          _ <- State.sequence(rules map {
            case CatchRule(sym, clazz, body) => for {
              _ <- visitExp(body)
            } yield ()
          })
        } yield ()

        case Expression.InvokeConstructor(constructor, args, tpe, loc) => for {
          _ <- State.sequence(args map (e => visitExp(e)))
        } yield ()

        case Expression.InvokeMethod(method, exp, args, tpe, loc) => for {
          _ <- visitExp(exp)
          _ <- State.sequence(args map (e => visitExp(e)))
        } yield ()

        case Expression.InvokeStaticMethod(method, args, tpe, loc) => for {
          _ <- State.sequence(args map (e => visitExp(e)))
        } yield ()

        case Expression.GetField(field, exp, tpe, loc) => for {
          _ <- visitExp(exp)
        } yield ()

        case Expression.PutField(field, exp1, exp2, tpe, loc) => for {
          _ <- visitExp(exp1)
          _ <- visitExp(exp2)
        } yield ()

        case Expression.GetStaticField(field, tpe, loc) => State.ret(())

        case Expression.PutStaticField(field, exp, tpe, loc) => for {
          _ <- visitExp(exp)
        } yield ()

        case Expression.NewChannel(exp, tpe, loc) => for {
          _ <- visitExp(exp)
        } yield ()

        case Expression.GetChannel(exp, tpe, loc) => for {
          _ <- visitExp(exp)
        } yield ()

        case Expression.PutChannel(exp1, exp2, tpe, loc) => for {
          _ <- visitExp(exp1)
          _ <- visitExp(exp2)
        } yield ()

        case Expression.SelectChannel(rules, default, tpe, loc) => for {
          _ <- State.sequence(rules map {
            case SelectChannelRule(sym, chan, exp) => for {
              _ <- visitExp(chan)
              _ <- visitExp(exp)
            } yield ()
          })
          _ <- State.mapOpt(default map (e => visitExp(e)))
        } yield ()

        case Expression.ProcessSpawn(exp, tpe, loc) => for {
          _ <- visitExp(exp)
        } yield ()

        case Expression.ProcessPanic(msg, tpe, loc) => State.ret(())

        case Expression.FixpointConstraintSet(cs0, tpe, loc) => State.ret(())

        case Expression.FixpointCompose(exp1, exp2, tpe, loc) => for {
          _ <- visitExp(exp1)
          _ <- visitExp(exp2)
        } yield ()

        case Expression.FixpointSolve(exp, stf, tpe, loc) => for {
          _ <- visitExp(exp)
        } yield ()

        case Expression.FixpointProject(sym, exp, tpe, loc) => for {
          _ <- visitExp(exp)
        } yield ()

        case Expression.FixpointEntails(exp1, exp2, tpe, loc) => for {
          _ <- visitExp(exp1)
          _ <- visitExp(exp2)
        } yield ()

        case Expression.FixpointFold(sym, exp1, exp2, exp3, tpe, loc) => for {
          _ <- visitExp(exp1)
          _ <- visitExp(exp2)
          _ <- visitExp(exp3)
        } yield ()

        case Expression.HoleError(sym, tpe, loc) => State.ret(())
        case Expression.MatchError(tpe, loc) => State.ret(())

        case Expression.LambdaClosure(fparams, freeVars, exp, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${lExp.exp.getClass}'.")
        case Expression.Closure(sym, freeVars, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${lExp.exp.getClass}'.")
        case Expression.ApplyClo(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${lExp.exp.getClass}'.")
        case Expression.ApplyDef(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${lExp.exp.getClass}'.")
        case Expression.ApplyCloTail(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${lExp.exp.getClass}'.")
        case Expression.ApplyDefTail(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${lExp.exp.getClass}'.")
        case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${lExp.exp.getClass}'.")
        case LabeledExpression(label, exp) => throw InternalCompilerException(s"Unexpected expression: '${lExp.exp.getClass}'.")
      }
      val (_, flows) = visitor(acc)
      val (varFlow, expFlow) = flows
      val result = lambdas.get(lExp.tpe) match {
        case Some(lams) =>
          lExp.exp match {
            case Expression.Var(sym, tpe, loc) => (varFlow + (sym -> lams), expFlow)
            case _ => (varFlow, expFlow + (lExp -> lams))
          }
        case None => flows
      }
      ((), result)
    }

    val (_, (varFlow, expFlow)) = flix.subphase("Points to analysis") {
      State.sequence(labeledDefs map {
        case (_, defn) => visitExp(defn.exp)
      })(Map.empty, Map.empty)
    }

    def inlineExp(exp0: Expression): State[InlineContext, Expression] =
      exp0 match {
        case LabeledExpression(label, exp) => inlineExp(exp)

        case Expression.Unit => State.ret(exp0)
        case Expression.True => State.ret(exp0)
        case Expression.False => State.ret(exp0)
        case Expression.Char(lit) => State.ret(exp0)
        case Expression.Float32(lit) => State.ret(exp0)
        case Expression.Float64(lit) => State.ret(exp0)
        case Expression.Int8(lit) => State.ret(exp0)
        case Expression.Int16(lit) => State.ret(exp0)
        case Expression.Int32(lit) => State.ret(exp0)
        case Expression.Int64(lit) => State.ret(exp0)
        case Expression.BigInt(lit) => State.ret(exp0)
        case Expression.Str(lit) => State.ret(exp0)

        case Expression.Def(sym, tpe, loc) => State.ret(Expression.Def(sym, tpe, loc))

        case Expression.Var(sym, tpe, loc) => State { case (ctxt0, vars, labels) =>
          val s = vars.getOrElse(sym, sym)
          (Expression.Var(s, tpe, loc), (ctxt0, vars, labels))
        }

        case Expression.Lambda(args, body, tpe, loc) => State { case (ctxt0, vars0, labels) =>
          val freshArgs = args.map(fparam => fparam.copy(sym = Symbol.freshVarSym(fparam.sym)))
          val vars1 = args.zip(freshArgs).map {
            case (oldParam, newParam) => oldParam.sym -> newParam.sym
          }
          val (b, _) = inlineExp(body)((ctxt0 + loc, vars0 ++ vars1.toMap, labels))
          (Expression.Lambda(freshArgs, b, tpe, loc), (ctxt0, vars0, labels))
        }

        case Expression.Apply(lExp, args, tpe, loc) => State { st =>
          val lams = lExp.exp match {
            case Expression.Var(sym, tpe, loc) => varFlow.getOrElse(sym, Set.empty)
            case _ => expFlow.getOrElse(lExp, Set.empty)
          }
          val (as, _) = State.sequence(args map inlineExp)(st)
          if (lams.size == 1) {
            val lam = lams.head
            val lamCtxt = st._1
            if (!lamCtxt.contains(lam.loc)) {
              val (i, _) = inlineExp(lam)(st)
              if (SimplifiedAstOps.freeVars(i).isEmpty) {
                (Expression.Apply(i, as, tpe, loc), st)
              } else {
                val (e, _) = inlineExp(lExp)(st)
                (Expression.Apply(e, as, tpe, loc), st)
              }
            } else {
              val (e, _) = inlineExp(lExp)(st)
              (Expression.Apply(e, as, tpe, loc), st)
            }
          } else {
            val (e, _) = inlineExp(lExp)(st)
            (Expression.Apply(e, as, tpe, loc), st)
          }
        }

        case Expression.Unary(sop, op, exp, tpe, loc) => for {
          e <- inlineExp(exp)
        } yield Expression.Unary(sop, op, e, tpe, loc)

        case Expression.Binary(sop, op, exp1, exp2, tpe, loc) => for {
          e1 <- inlineExp(exp1)
          e2 <- inlineExp(exp2)
        } yield Expression.Binary(sop, op, e1, e2, tpe, loc)

        case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) => for {
          cond <- inlineExp(exp1)
          consequent <- inlineExp(exp2)
          alternative <- inlineExp(exp3)
        } yield Expression.IfThenElse(cond, consequent, alternative, tpe, loc)

        case Expression.Branch(exp, branches, tpe, loc) => for {
          // Need to rename all cases before we inline their bodies
          // because they can refer to each other.
          renamedBranches <- State.sequence(branches.toList map {
            case (sym, br) => State { case (ctxt0, vars0, labels0) =>
              val freshLabel = Symbol.freshLabel(sym)
              val labels1 = labels0 + (sym -> freshLabel)
              ((freshLabel -> br), (ctxt0, vars0, labels1))
            }: State[InlineContext, (Symbol.LabelSym, Expression)]
          })
          bs <- State.sequence(renamedBranches map {
            case (sym, br) => for {
              b <- inlineExp(br)
            } yield (sym -> b)
          })
          e <- inlineExp(exp)
        } yield Expression.Branch(e, bs.toMap, tpe, loc)

        case Expression.JumpTo(sym, tpe, loc) => State { case (ctxt0, vars0, labels0) =>
          val s = labels0(sym) // Jumps don't occur outside branch blocks
          (Expression.JumpTo(s, tpe, loc), (ctxt0, vars0, labels0))
        }

        case Expression.Let(sym, exp1, exp2, tpe, loc) => State { case (ctxt0, vars0, labels0) =>
          val freshSym = Symbol.freshVarSym(sym)
          val vars1 = vars0 + (sym -> freshSym)
          val (e1, _) = inlineExp(exp1)((ctxt0, vars0, labels0))
          val (e2, _) = inlineExp(exp2)((ctxt0, vars1, labels0))
          (Expression.Let(freshSym, e1, e2, tpe, loc), (ctxt0, vars0, labels0))
        }

        case Expression.LetRec(sym, exp1, exp2, tpe, loc) => State { case (ctxt0, vars0, labels0) =>
          val freshSym = Symbol.freshVarSym(sym)
          val vars1 = vars0 + (sym -> freshSym)
          val (e1, _) = inlineExp(exp1)((ctxt0, vars1, labels0))
          val (e2, _) = inlineExp(exp2)((ctxt0, vars1, labels0))
          (Expression.LetRec(freshSym, e1, e2, tpe, loc), (ctxt0, vars0, labels0))
        }

        case Expression.Is(sym, tag, exp, loc) => for {
          e <- inlineExp(exp)
        } yield Expression.Is(sym, tag, e, loc)

        case Expression.Tag(sym, tag, exp, tpe, loc) => for {
          e <- inlineExp(exp)
        } yield Expression.Tag(sym, tag, e, tpe, loc)

        case Expression.Untag(sym, tag, exp, tpe, loc) => for {
          e <- inlineExp(exp)
        } yield Expression.Untag(sym, tag, e, tpe, loc)

        case Expression.Index(base, offset, tpe, loc) => for {
          b <- inlineExp(base)
        } yield Expression.Index(b, offset, tpe, loc)

        case Expression.Tuple(elms, tpe, loc) => for {
          es <- State.sequence(elms map inlineExp)
        } yield Expression.Tuple(es, tpe, loc)

        case Expression.RecordEmpty(tpe, loc) => State.ret(Expression.RecordEmpty(tpe, loc))

        case Expression.RecordSelect(exp, label, tpe, loc) => for {
          e <- inlineExp(exp)
        } yield Expression.RecordSelect(e, label, tpe, loc)

        case Expression.RecordExtend(label, value, rest, tpe, loc) => for {
          v <- inlineExp(value)
          r <- inlineExp(rest)
        } yield Expression.RecordExtend(label, v, r, tpe, loc)

        case Expression.RecordRestrict(label, rest, tpe, loc) => for {
          r <- inlineExp(rest)
        } yield Expression.RecordRestrict(label, r, tpe, loc)

        case Expression.ArrayLit(elms, tpe, loc) => for {
          es <- State.sequence(elms map inlineExp)
        } yield Expression.ArrayLit(es, tpe, loc)

        case Expression.ArrayNew(elm, len, tpe, loc) => for {
          e <- inlineExp(elm)
          ln <- inlineExp(len)
        } yield Expression.ArrayNew(e, ln, tpe, loc)

        case Expression.ArrayLoad(base, index, tpe, loc) => for {
          b <- inlineExp(base)
          i <- inlineExp(index)
        } yield Expression.ArrayLoad(b, i, tpe, loc)

        case Expression.ArrayStore(base, index, elm, tpe, loc) => for {
          b <- inlineExp(base)
          i <- inlineExp(index)
          e <- inlineExp(elm)
        } yield Expression.ArrayStore(b, i, e, tpe, loc)

        case Expression.ArrayLength(base, tpe, loc) => for {
          b <- inlineExp(base)
        } yield Expression.ArrayLength(b, tpe, loc)

        case Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) => for {
          b <- inlineExp(base)
          i1 <- inlineExp(startIndex)
          i2 <- inlineExp(endIndex)
        } yield Expression.ArraySlice(b, i1, i2, tpe, loc)

        case Expression.Ref(exp, tpe, loc) => for {
          e <- inlineExp(exp)
        } yield Expression.Ref(e, tpe, loc)

        case Expression.Deref(exp, tpe, loc) => for {
          e <- inlineExp(exp)
        } yield Expression.Deref(e, tpe, loc)

        case Expression.Assign(exp1, exp2, tpe, loc) => for {
          e1 <- inlineExp(exp1)
          e2 <- inlineExp(exp2)
        } yield Expression.Assign(e1, e2, tpe, loc)

        case Expression.Existential(fparam, exp, loc) => State { case (ctxt0, vars0, labels0) =>
          val freshParam = fparam.copy(sym = Symbol.freshVarSym(fparam.sym))
          val vars1 = vars0 + (fparam.sym -> freshParam.sym)
          val (e, _) = inlineExp(exp)((ctxt0, vars1, labels0))
          (Expression.Existential(freshParam, e, loc), (ctxt0, vars0, labels0))
        }

        case Expression.Universal(fparam, exp, loc) => State { case (ctxt0, vars0, labels0) =>
          val freshParam = fparam.copy(sym = Symbol.freshVarSym(fparam.sym))
          val vars1 = vars0 + (fparam.sym -> freshParam.sym)
          val (e, _) = inlineExp(exp)((ctxt0, vars1, labels0))
          (Expression.Universal(freshParam, e, loc), (ctxt0, vars0, labels0))
        }

        case Expression.Cast(exp, tpe, loc) => for {
          e <- inlineExp(exp)
        } yield Expression.Cast(e, tpe, loc)

        case Expression.TryCatch(exp, rules, tpe, loc) => for {
          // TODO: is this right or do we need to handle it like Branch?
          e <- inlineExp(exp)
          rs <- State.sequence(rules map {
            case CatchRule(sym, clazz, body) => State { case (ctxt0, vars0, labels0) =>
              val freshSym = Symbol.freshVarSym(sym)
              val vars1 = vars0 + (sym -> freshSym)
              val (b, _) = inlineExp(body)((ctxt0, vars1, labels0))
              (CatchRule(freshSym, clazz, b), (ctxt0, vars0, labels0))
            }: State[InlineContext, SimplifiedAst.CatchRule]
          })
        } yield Expression.TryCatch(e, rs, tpe, loc)

        case Expression.InvokeConstructor(constructor, args, tpe, loc) => for {
          as <- State.sequence(args map inlineExp)
        } yield Expression.InvokeConstructor(constructor, as, tpe, loc)

        case Expression.InvokeMethod(method, exp, args, tpe, loc) => for {
          e <- inlineExp(exp)
          as <- State.sequence(args map inlineExp)
        } yield Expression.InvokeMethod(method, e, as, tpe, loc)

        case Expression.InvokeStaticMethod(method, args, tpe, loc) => for {
          as <- State.sequence(args map inlineExp)
        } yield Expression.InvokeStaticMethod(method, as, tpe, loc)

        case Expression.GetField(field, exp, tpe, loc) => for {
          e <- inlineExp(exp)
        } yield Expression.GetField(field, e, tpe, loc)

        case Expression.PutField(field, exp1, exp2, tpe, loc) => for {
          e1 <- inlineExp(exp1)
          e2 <- inlineExp(exp2)
        } yield Expression.PutField(field, e1, e2, tpe, loc)

        case Expression.GetStaticField(field, tpe, loc) => State.ret(Expression.GetStaticField(field, tpe, loc))

        case Expression.PutStaticField(field, exp, tpe, loc) => for {
          e <- inlineExp(exp)
        } yield Expression.PutStaticField(field, e, tpe, loc)

        case Expression.NewChannel(exp, tpe, loc) => for {
          e <- inlineExp(exp)
        } yield Expression.NewChannel(e, tpe, loc)

        case Expression.GetChannel(exp, tpe, loc) => for {
          e <- inlineExp(exp)
        } yield Expression.GetChannel(e, tpe, loc)

        case Expression.PutChannel(exp1, exp2, tpe, loc) => for {
          e1 <- inlineExp(exp1)
          e2 <- inlineExp(exp2)
        } yield Expression.PutChannel(e1, e2, tpe, loc)

        case Expression.SelectChannel(rules, default, tpe, loc) => for {
          // TODO: is this right or do we need to handle it like Branch?
          rs <- State.sequence(rules map {
            case SelectChannelRule(sym, chan, exp) => State { case (ctxt0, vars0, labels0) =>
              val freshSym = Symbol.freshVarSym(sym)
              val vars1 = vars0 + (sym -> freshSym)
              val (c, _) = inlineExp(chan)((ctxt0, vars0, labels0)) // TODO: can sym occur in chan?
              val (e, _) = inlineExp(exp)((ctxt0, vars1, labels0))
              (SimplifiedAst.SelectChannelRule(freshSym, c, e), (ctxt0, vars0, labels0))
            }: State[InlineContext, SimplifiedAst.SelectChannelRule]
          })
          d <- State.mapOpt(default.map(inlineExp))
        } yield Expression.SelectChannel(rs, d, tpe, loc)

        case Expression.ProcessSpawn(exp, tpe, loc) => for {
          e <- inlineExp(exp)
        } yield Expression.ProcessSpawn(e, tpe, loc)

        case Expression.ProcessPanic(msg, tpe, loc) => State.ret(Expression.ProcessPanic(msg, tpe, loc))

        case Expression.FixpointConstraintSet(cs0, tpe, loc) => State.ret(Expression.FixpointConstraintSet(cs0, tpe, loc))

        case Expression.FixpointCompose(exp1, exp2, tpe, loc) => for {
          e1 <- inlineExp(exp1)
          e2 <- inlineExp(exp2)
        } yield Expression.FixpointCompose(e1, e2, tpe, loc)

        case Expression.FixpointSolve(exp, stf, tpe, loc) => for {
          e <- inlineExp(exp)
        } yield Expression.FixpointSolve(e, stf, tpe, loc)

        case Expression.FixpointProject(sym, exp, tpe, loc) => for { // TODO: rename?
          e <- inlineExp(exp)
        } yield Expression.FixpointProject(sym, e, tpe, loc)

        case Expression.FixpointEntails(exp1, exp2, tpe, loc) => for {
          e1 <- inlineExp(exp1)
          e2 <- inlineExp(exp2)
        } yield Expression.FixpointEntails(e1, e2, tpe, loc)

        case Expression.FixpointFold(sym, exp1, exp2, exp3, tpe, loc) => for { // TODO: rename?
          e1 <- inlineExp(exp1)
          e2 <- inlineExp(exp2)
          e3 <- inlineExp(exp3)
        } yield Expression.FixpointFold(sym, e1, e2, e3, tpe, loc)

        case Expression.HoleError(sym, tpe, loc) => State.ret(Expression.HoleError(sym, tpe, loc))
        case Expression.MatchError(tpe, loc) => State.ret(Expression.MatchError(tpe, loc))

        case Expression.LambdaClosure(fparams, freeVars, exp, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.Closure(sym, freeVars, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplyClo(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplyDef(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplyCloTail(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplyDefTail(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
      }

    val (unlabeledDefs, _) = flix.subphase("Inlining") {
      State.sequence(labeledDefs.map {
        case (sym, defn) => for {
          i <- inlineExp(defn.exp)
        } yield (sym -> defn.copy(exp = i))
      })((Set.empty, Map.empty, Map.empty))
    }

    def simplifyApply(exp0: Expression): Expression =
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
          val b = simplifyApply(body)
          Expression.Lambda(args, b, tpe, loc)

        case Expression.Apply(lam: Expression.Lambda, args, tpe, loc) =>
          val as = args map simplifyApply
          val let = lam.fparams.zip(as).foldRight(lam.exp) {
            case ((formal, actual), body) => Expression.Let(formal.sym, actual, body, tpe, loc)
          }
          simplifyApply(let)

        case Expression.Apply(exp, args, tpe, loc) =>
          val e = simplifyApply(exp)
          val as = args map simplifyApply
          Expression.Apply(e, as, tpe, loc)

        case Expression.Unary(sop, op, exp, tpe, loc) =>
          val e = simplifyApply(exp)
          Expression.Unary(sop, op, e, tpe, loc)

        case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
          val e1 = simplifyApply(exp1)
          val e2 = simplifyApply(exp2)
          Expression.Binary(sop, op, e1, e2, tpe, loc)

        case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
          val cond = simplifyApply(exp1)
          val consequent = simplifyApply(exp2)
          val alternative = simplifyApply(exp3)
          Expression.IfThenElse(cond, consequent, alternative, tpe, loc)

        case Expression.Branch(exp, branches, tpe, loc) =>
          val e = simplifyApply(exp)
          val bs = branches map {
            case (sym, br) =>
              val b = simplifyApply(br)
              (sym -> b)
          }
          Expression.Branch(e, bs, tpe, loc)

        case Expression.JumpTo(sym, tpe, loc) => Expression.JumpTo(sym, tpe, loc)

        case Expression.Let(sym, exp1, exp2, tpe, loc) =>
          val e1 = simplifyApply(exp1)
          val e2 = simplifyApply(exp2)
          Expression.Let(sym, e1, e2, tpe, loc)

        case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
          val e1 = simplifyApply(exp1)
          val e2 = simplifyApply(exp2)
          Expression.LetRec(sym, e1, e2, tpe, loc)

        case Expression.Is(sym, tag, exp, loc) =>
          val e = simplifyApply(exp)
          Expression.Is(sym, tag, e, loc)

        case Expression.Tag(sym, tag, exp, tpe, loc) =>
          val e = simplifyApply(exp)
          Expression.Tag(sym, tag, e, tpe, loc)

        case Expression.Untag(sym, tag, exp, tpe, loc) =>
          val e = simplifyApply(exp)
          Expression.Untag(sym, tag, e, tpe, loc)

        case Expression.Index(base, offset, tpe, loc) =>
          val b = simplifyApply(base)
          Expression.Index(b, offset, tpe, loc)

        case Expression.Tuple(elms, tpe, loc) =>
          val es = elms map simplifyApply
          Expression.Tuple(es, tpe, loc)

        case Expression.RecordEmpty(tpe, loc) => Expression.RecordEmpty(tpe, loc)

        case Expression.RecordSelect(exp, label, tpe, loc) =>
          val e = simplifyApply(exp)
          Expression.RecordSelect(e, label, tpe, loc)

        case Expression.RecordExtend(label, value, rest, tpe, loc) =>
          val v = simplifyApply(value)
          val r = simplifyApply(rest)
          Expression.RecordExtend(label, v, r, tpe, loc)

        case Expression.RecordRestrict(label, rest, tpe, loc) =>
          val r = simplifyApply(rest)
          Expression.RecordRestrict(label, r, tpe, loc)

        case Expression.ArrayLit(elms, tpe, loc) =>
          val es = elms map simplifyApply
          Expression.ArrayLit(es, tpe, loc)

        case Expression.ArrayNew(elm, len, tpe, loc) =>
          val e = simplifyApply(elm)
          val ln = simplifyApply(len)
          Expression.ArrayNew(e, ln, tpe, loc)

        case Expression.ArrayLoad(base, index, tpe, loc) =>
          val b = simplifyApply(base)
          val i = simplifyApply(index)
          Expression.ArrayLoad(b, i, tpe, loc)

        case Expression.ArrayStore(base, index, elm, tpe, loc) =>
          val b = simplifyApply(base)
          val i = simplifyApply(index)
          val e = simplifyApply(elm)
          Expression.ArrayStore(b, i, e, tpe, loc)

        case Expression.ArrayLength(base, tpe, loc) =>
          val b = simplifyApply(base)
          Expression.ArrayLength(b, tpe, loc)

        case Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) =>
          val b = simplifyApply(base)
          val i1 = simplifyApply(startIndex)
          val i2 = simplifyApply(endIndex)
          Expression.ArraySlice(b, i1, i2, tpe, loc)

        case Expression.Ref(exp, tpe, loc) =>
          val e = simplifyApply(exp)
          Expression.Ref(e, tpe, loc)

        case Expression.Deref(exp, tpe, loc) =>
          val e = simplifyApply(exp)
          Expression.Deref(e, tpe, loc)

        case Expression.Assign(exp1, exp2, tpe, loc) =>
          val e1 = simplifyApply(exp1)
          val e2 = simplifyApply(exp2)
          Expression.Assign(e1, e2, tpe, loc)

        case Expression.Existential(fparam, exp, loc) =>
          val e = simplifyApply(exp)
          Expression.Existential(fparam, e, loc)

        case Expression.Universal(fparam, exp, loc) =>
          val e = simplifyApply(exp)
          Expression.Universal(fparam, e, loc)

        case Expression.Cast(exp, tpe, loc) =>
          val e = simplifyApply(exp)
          Expression.Cast(e, tpe, loc)

        case Expression.TryCatch(exp, rules, tpe, loc) =>
          val e = simplifyApply(exp)
          val rs = rules map {
            case CatchRule(sym, clazz, body) =>
              val b = simplifyApply(body)
              CatchRule(sym, clazz, b)
          }
          Expression.TryCatch(e, rs, tpe, loc)

        case Expression.InvokeConstructor(constructor, args, tpe, loc) =>
          val as = args map simplifyApply
          Expression.InvokeConstructor(constructor, as, tpe, loc)

        case Expression.InvokeMethod(method, exp, args, tpe, loc) =>
          val e = simplifyApply(exp)
          val as = args map simplifyApply
          Expression.InvokeMethod(method, e, as, tpe, loc)

        case Expression.InvokeStaticMethod(method, args, tpe, loc) =>
          val as = args map simplifyApply
          Expression.InvokeStaticMethod(method, as, tpe, loc)

        case Expression.GetField(field, exp, tpe, loc) =>
          val e = simplifyApply(exp)
          Expression.GetField(field, e, tpe, loc)

        case Expression.PutField(field, exp1, exp2, tpe, loc) =>
          val e1 = simplifyApply(exp1)
          val e2 = simplifyApply(exp2)
          Expression.PutField(field, e1, e2, tpe, loc)

        case Expression.GetStaticField(field, tpe, loc) => Expression.GetStaticField(field, tpe, loc)

        case Expression.PutStaticField(field, exp, tpe, loc) =>
          val e = simplifyApply(exp)
          Expression.PutStaticField(field, e, tpe, loc)

        case Expression.NewChannel(exp, tpe, loc) =>
          val e = simplifyApply(exp)
          Expression.NewChannel(e, tpe, loc)

        case Expression.GetChannel(exp, tpe, loc) =>
          val e = simplifyApply(exp)
          Expression.GetChannel(e, tpe, loc)

        case Expression.PutChannel(exp1, exp2, tpe, loc) =>
          val e1 = simplifyApply(exp1)
          val e2 = simplifyApply(exp2)
          Expression.PutChannel(e1, e2, tpe, loc)

        case Expression.SelectChannel(rules, default, tpe, loc) =>
          val rs = rules map {
            case SelectChannelRule(sym, chan, exp) =>
              val c = simplifyApply(chan)
              val e = simplifyApply(exp)
              SelectChannelRule(sym, c, e)
          }
          val d = default.map(simplifyApply)
          Expression.SelectChannel(rs, d, tpe, loc)

        case Expression.ProcessSpawn(exp, tpe, loc) =>
          val e = simplifyApply(exp)
          Expression.ProcessSpawn(e, tpe, loc)

        case Expression.ProcessPanic(msg, tpe, loc) => Expression.ProcessPanic(msg, tpe, loc)

        case Expression.FixpointConstraintSet(cs0, tpe, loc) => Expression.FixpointConstraintSet(cs0, tpe, loc)

        case Expression.FixpointCompose(exp1, exp2, tpe, loc) =>
          val e1 = simplifyApply(exp1)
          val e2 = simplifyApply(exp2)
          Expression.FixpointCompose(e1, e2, tpe, loc)

        case Expression.FixpointSolve(exp, stf, tpe, loc) =>
          val e = simplifyApply(exp)
          Expression.FixpointSolve(e, stf, tpe, loc)

        case Expression.FixpointProject(sym, exp, tpe, loc) =>
          val e = simplifyApply(exp)
          Expression.FixpointProject(sym, e, tpe, loc)

        case Expression.FixpointEntails(exp1, exp2, tpe, loc) =>
          val e1 = simplifyApply(exp1)
          val e2 = simplifyApply(exp2)
          Expression.FixpointEntails(e1, e2, tpe, loc)

        case Expression.FixpointFold(sym, exp1, exp2, exp3, tpe, loc) =>
          val e1 = simplifyApply(exp1)
          val e2 = simplifyApply(exp2)
          val e3 = simplifyApply(exp3)
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

    val simplifiedDefs = flix.subphase("Simplification") {
      unlabeledDefs map {
        case (sym, defn) => sym -> defn.copy(exp = simplifyApply(defn.exp))
      }
    }

    val result = root.copy(defs = simplifiedDefs.toMap)

    // Print the ast if debugging is enabled.
    if (flix.options.debug) {
      println("--------------CFA RESULT BEGIN--------------")
      println(PrettyPrinter.Simplified.fmtRoot(result).fmt(TerminalContext.AnsiTerminal))
      println("--------------CFA RESULT END--------------")
    }

    result.toSuccess
  }

  // TODO: Move to SimplifiedAstOps?
  private def renameBoundVars(exp0: Expression, vars0: Map[Symbol.VarSym, Symbol.VarSym], labels0: Map[Symbol.LabelSym, Symbol.LabelSym])(implicit flix: Flix): Expression =
    exp0 match {
      case LabeledExpression(label, exp) =>
        val e = renameBoundVars(exp, vars0, labels0)
        LabeledExpression(label, e)

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

      case Expression.Var(sym, tpe, loc) =>
        val s = vars0(sym)//vars0.getOrElse(sym, sym)
        Expression.Var(s, tpe, loc)

      case Expression.Lambda(args0, body, tpe, loc) =>
        val args1 = args0.map(fparam => fparam.copy(sym = Symbol.freshVarSym(fparam.sym)))
        val vars1 = args0.zip(args1).map {
          case (oldParam, newParam) => oldParam.sym -> newParam.sym
        }
        val b = renameBoundVars(body, vars1.toMap ++ vars0, labels0)
        Expression.Lambda(args1, b, tpe, loc)

      case Expression.Apply(exp, args, tpe, loc) =>
        val e = renameBoundVars(exp, vars0, labels0)
        val as = args map (renameBoundVars(_, vars0, labels0))
        Expression.Apply(e, as, tpe, loc)

      case Expression.Unary(sop, op, exp, tpe, loc) =>
        val e = renameBoundVars(exp, vars0, labels0)
        Expression.Unary(sop, op, e, tpe, loc)

      case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
        val e1 = renameBoundVars(exp1, vars0, labels0)
        val e2 = renameBoundVars(exp2, vars0, labels0)
        Expression.Binary(sop, op, e1, e2, tpe, loc)

      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        val cond = renameBoundVars(exp1, vars0, labels0)
        val consequent = renameBoundVars(exp2, vars0, labels0)
        val alternative = renameBoundVars(exp3, vars0, labels0)
        Expression.IfThenElse(cond, consequent, alternative, tpe, loc)

      case Expression.Branch(exp, branches, tpe, loc) =>
        // Need to rename all cases before we rename their bodies
        // because they can refer to each other.
        val labels1 = branches.foldLeft(labels0) {
          case (acc, (sym0, br)) =>
            val sym1 = Symbol.freshLabel(sym0)
            acc + (sym0 -> sym1)
        }
        val bs = branches map {
          case (sym, br) =>
            val b = renameBoundVars(br, vars0, labels1)
            labels1(sym) -> b
        }
        val e = renameBoundVars(exp, vars0, labels1)
        Expression.Branch(e, bs, tpe, loc)

      case Expression.JumpTo(sym, tpe, loc) =>
        val s = labels0(sym) // Jumps don't occur outside branch blocks
        Expression.JumpTo(s, tpe, loc)

      case Expression.Let(sym, exp1, exp2, tpe, loc) =>
        val s = Symbol.freshVarSym(sym)
        val vars1 = vars0 + (sym -> s)
        val e1 = renameBoundVars(exp1, vars0, labels0)
        val e2 = renameBoundVars(exp2, vars1, labels0)
        Expression.Let(s, e1, e2, tpe, loc)

      case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
        val s = Symbol.freshVarSym(sym)
        val vars1 = vars0 + (sym -> s)
        val e1 = renameBoundVars(exp1, vars1, labels0)
        val e2 = renameBoundVars(exp2, vars1, labels0)
        Expression.LetRec(s, e1, e2, tpe, loc)

      case Expression.Is(sym, tag, exp, loc) =>
        val e = renameBoundVars(exp, vars0, labels0)
        Expression.Is(sym, tag, e, loc)

      case Expression.Tag(sym, tag, exp, tpe, loc) =>
        val e = renameBoundVars(exp, vars0, labels0)
        Expression.Tag(sym, tag, e, tpe, loc)

      case Expression.Untag(sym, tag, exp, tpe, loc) =>
        val e = renameBoundVars(exp, vars0, labels0)
        Expression.Untag(sym, tag, e, tpe, loc)

      case Expression.Index(base, offset, tpe, loc) =>
        val b = renameBoundVars(base, vars0, labels0)
        Expression.Index(b, offset, tpe, loc)

      case Expression.Tuple(elms, tpe, loc) =>
        val es = elms map (renameBoundVars(_, vars0, labels0))
        Expression.Tuple(es, tpe, loc)

      case Expression.RecordEmpty(tpe, loc) => Expression.RecordEmpty(tpe, loc)

      case Expression.RecordSelect(exp, label, tpe, loc) =>
        val e = renameBoundVars(exp, vars0, labels0)
        Expression.RecordSelect(e, label, tpe, loc)

      case Expression.RecordExtend(label, value, rest, tpe, loc) =>
        val v = renameBoundVars(value, vars0, labels0)
        val r = renameBoundVars(rest, vars0, labels0)
        Expression.RecordExtend(label, v, r, tpe, loc)

      case Expression.RecordRestrict(label, rest, tpe, loc) =>
        val r = renameBoundVars(rest, vars0, labels0)
        Expression.RecordRestrict(label, r, tpe, loc)

      case Expression.ArrayLit(elms, tpe, loc) =>
        val es = elms map (renameBoundVars(_, vars0, labels0))
        Expression.ArrayLit(es, tpe, loc)

      case Expression.ArrayNew(elm, len, tpe, loc) =>
        val e = renameBoundVars(elm, vars0, labels0)
        val ln = renameBoundVars(len, vars0, labels0)
        Expression.ArrayNew(e, ln, tpe, loc)

      case Expression.ArrayLoad(base, index, tpe, loc) =>
        val b = renameBoundVars(base, vars0, labels0)
        val i = renameBoundVars(index, vars0, labels0)
        Expression.ArrayLoad(b, i, tpe, loc)

      case Expression.ArrayStore(base, index, elm, tpe, loc) =>
        val b = renameBoundVars(base, vars0, labels0)
        val i = renameBoundVars(index, vars0, labels0)
        val e = renameBoundVars(elm, vars0, labels0)
        Expression.ArrayStore(b, i, e, tpe, loc)

      case Expression.ArrayLength(base, tpe, loc) =>
        val b = renameBoundVars(base, vars0, labels0)
        Expression.ArrayLength(b, tpe, loc)

      case Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) =>
        val b = renameBoundVars(base, vars0, labels0)
        val i1 = renameBoundVars(startIndex, vars0, labels0)
        val i2 = renameBoundVars(endIndex, vars0, labels0)
        Expression.ArraySlice(b, i1, i2, tpe, loc)

      case Expression.Ref(exp, tpe, loc) =>
        val e = renameBoundVars(exp, vars0, labels0)
        Expression.Ref(e, tpe, loc)

      case Expression.Deref(exp, tpe, loc) =>
        val e = renameBoundVars(exp, vars0, labels0)
        Expression.Deref(e, tpe, loc)

      case Expression.Assign(exp1, exp2, tpe, loc) =>
        val e1 = renameBoundVars(exp1, vars0, labels0)
        val e2 = renameBoundVars(exp2, vars0, labels0)
        Expression.Assign(e1, e2, tpe, loc)

      case Expression.Existential(fparam0, exp, loc) =>
        val fparam1 = fparam0.copy(sym = Symbol.freshVarSym(fparam0.sym))
        val vars1 = vars0 + (fparam0.sym -> fparam1.sym)
        val e = renameBoundVars(exp, vars1, labels0)
        Expression.Existential(fparam1, e, loc)

      case Expression.Universal(fparam0, exp, loc) =>
        val fparam1 = fparam0.copy(sym = Symbol.freshVarSym(fparam0.sym))
        val vars1 = vars0 + (fparam0.sym -> fparam1.sym)
        val e = renameBoundVars(exp, vars1, labels0)
        Expression.Universal(fparam1, e, loc)

      case Expression.Cast(exp, tpe, loc) =>
        val e = renameBoundVars(exp, vars0, labels0)
        Expression.Cast(e, tpe, loc)

      case Expression.TryCatch(exp, rules, tpe, loc) =>
        // TODO: is this right or do we need to handle it like Branch?
        val e = renameBoundVars(exp, vars0, labels0)
        val rs = rules map {
          case CatchRule(sym, clazz, body) =>
            val s = Symbol.freshVarSym(sym)
            val vars1 = vars0 + (sym -> s)
            val b = renameBoundVars(body, vars1, labels0)
            CatchRule(s, clazz, b)
        }
        Expression.TryCatch(e, rs, tpe, loc)

      case Expression.InvokeConstructor(constructor, args, tpe, loc) =>
        val as = args map (renameBoundVars(_, vars0, labels0))
        Expression.InvokeConstructor(constructor, as, tpe, loc)

      case Expression.InvokeMethod(method, exp, args, tpe, loc) =>
        val e = renameBoundVars(exp, vars0, labels0)
        val as = args map (renameBoundVars(_, vars0, labels0))
        Expression.InvokeMethod(method, e, as, tpe, loc)

      case Expression.InvokeStaticMethod(method, args, tpe, loc) =>
        val as = args map (renameBoundVars(_, vars0, labels0))
        Expression.InvokeStaticMethod(method, as, tpe, loc)

      case Expression.GetField(field, exp, tpe, loc) =>
        val e = renameBoundVars(exp, vars0, labels0)
        Expression.GetField(field, e, tpe, loc)

      case Expression.PutField(field, exp1, exp2, tpe, loc) =>
        val e1 = renameBoundVars(exp1, vars0, labels0)
        val e2 = renameBoundVars(exp2, vars0, labels0)
        Expression.PutField(field, e1, e2, tpe, loc)

      case Expression.GetStaticField(field, tpe, loc) => Expression.GetStaticField(field, tpe, loc)

      case Expression.PutStaticField(field, exp, tpe, loc) =>
        val e = renameBoundVars(exp, vars0, labels0)
        Expression.PutStaticField(field, e, tpe, loc)

      case Expression.NewChannel(exp, tpe, loc) =>
        val e = renameBoundVars(exp, vars0, labels0)
        Expression.NewChannel(e, tpe, loc)

      case Expression.GetChannel(exp, tpe, loc) =>
        val e = renameBoundVars(exp, vars0, labels0)
        Expression.GetChannel(e, tpe, loc)

      case Expression.PutChannel(exp1, exp2, tpe, loc) =>
        val e1 = renameBoundVars(exp1, vars0, labels0)
        val e2 = renameBoundVars(exp2, vars0, labels0)
        Expression.PutChannel(e1, e2, tpe, loc)

      case Expression.SelectChannel(rules, default, tpe, loc) =>
        // TODO: is this right or do we need to handle it like Branch?
        val rs = rules map {
          case SelectChannelRule(sym, chan, exp) =>
            val s = Symbol.freshVarSym(sym)
            val vars1 = vars0 + (sym -> s)
            val c = renameBoundVars(chan, vars0, labels0) // TODO: can sym occur in chan?
            val e = renameBoundVars(exp, vars1, labels0)
            SelectChannelRule(s, c, e)
        }
        val d = default.map(renameBoundVars(_, vars0, labels0))
        Expression.SelectChannel(rs, d, tpe, loc)

      case Expression.ProcessSpawn(exp, tpe, loc) =>
        val e = renameBoundVars(exp, vars0, labels0)
        Expression.ProcessSpawn(e, tpe, loc)

      case Expression.ProcessPanic(msg, tpe, loc) => Expression.ProcessPanic(msg, tpe, loc)

      case Expression.FixpointConstraintSet(cs0, tpe, loc) => Expression.FixpointConstraintSet(cs0, tpe, loc)

      case Expression.FixpointCompose(exp1, exp2, tpe, loc) =>
        val e1 = renameBoundVars(exp1, vars0, labels0)
        val e2 = renameBoundVars(exp2, vars0, labels0)
        Expression.FixpointCompose(e1, e2, tpe, loc)

      case Expression.FixpointSolve(exp, stf, tpe, loc) =>
        val e = renameBoundVars(exp, vars0, labels0)
        Expression.FixpointSolve(e, stf, tpe, loc)

      case Expression.FixpointProject(sym, exp, tpe, loc) =>  // TODO: rename?
        val e = renameBoundVars(exp, vars0, labels0)
        Expression.FixpointProject(sym, e, tpe, loc)

      case Expression.FixpointEntails(exp1, exp2, tpe, loc) =>
        val e1 = renameBoundVars(exp1, vars0, labels0)
        val e2 = renameBoundVars(exp2, vars0, labels0)
        Expression.FixpointEntails(e1, e2, tpe, loc)

      case Expression.FixpointFold(sym, exp1, exp2, exp3, tpe, loc) => // TODO: rename?
        val e1 = renameBoundVars(exp1, vars0, labels0)
        val e2 = renameBoundVars(exp2, vars0, labels0)
        val e3 = renameBoundVars(exp3, vars0, labels0)
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
    }

  trait State[S, A] {
    def apply(s: S): (A, S)

    def map[B](f: A => B): State[S, B] = State { s =>
      val (a, newState) = this (s)
      (f(a), newState)
    }

    def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
      val (a, newState) = this (s)
      f(a)(newState)
    }
  }

  object State {
    def apply[S, A](r: S => (A, S)): State[S, A] = new State[S, A] {
      def apply(s: S) = r(s)
    }

    def sequence[S, A](xs: List[State[S, A]]): State[S, List[A]] = State { s0 =>
      xs.foldRight((List.empty[A], s0)) {
        case (st, (acc, s)) =>
          val (a, newState) = st(s)
          (a :: acc, newState)
      }
    }

    def mapOpt[S, A](opt: Option[State[S, A]]): State[S, Option[A]] = opt match {
      case Some(st) => for {
        a <- st
      } yield Some(a)
      case None => ret(None)
    }

    def ret[S, A](a: A): State[S, A] = State { s =>
      (a, s)
    }
  }

  case class LabeledExpression(label: Int, exp: Expression) extends Expression {
    override def tpe: Type = exp.tpe

    override def loc: SourceLocation = exp.loc

    override def equals(obj: Any): Boolean =
      obj match {
        case that: LabeledExpression => this.label == that.label
        case _ => false
      }

    override def hashCode(): Int = label.hashCode()
  }

  implicit def expToLabeled(exp: Expression): LabeledExpression = exp.asInstanceOf[LabeledExpression]
}
