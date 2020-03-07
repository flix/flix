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

  def run(root: SimplifiedAst.Root)(implicit flix: Flix): Validation[SimplifiedAst.Root, CompilationError] = flix.phase("CFA") {
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

        case Expression.TryCatch(exp, rules, tpe, loc) => for {
          e <- labelExp(exp)
          rs <- State.sequence(rules map {
            case CatchRule(sym, clazz, body) => for {
              b <- labelExp(body)
            } yield CatchRule(sym, clazz, b)
          })
        } yield Expression.TryCatch(e, rs, tpe, loc)

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
      val (lExp, newLabel) = labeler(label)
      val result = LabeledExpression(label, lExp)
      (result, newLabel + 1)
    }

    val (labeledDefs, _) = State.sequence(root.defs.toList map {
      case (sym, defn) => for {
        e <- labelExp(defn.exp)
      } yield (sym -> defn.copy(exp = e))
    })(0)

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

        case Expression.Lambda(args, body, tpe, loc) => lam =>
          val (_, lam_) = lambdasExp(body).map(_ => ())(lam)
          val lams = lam_.getOrElse(tpe, Set())
          val result = lam_ + (tpe -> (lams + Expression.Lambda(args, body, tpe, loc)))
          ((), result)

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

        case Expression.TryCatch(exp, rules, tpe, loc) => for {
          _ <- lambdasExp(exp)
          _ <- State.sequence(rules map {
            case CatchRule(sym, clazz, body) => for {
              _ <- lambdasExp(body)
            } yield ()
          })
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

    val (_, lambdas) = State.sequence(labeledDefs map {
      case (_, defn) => lambdasExp(defn.exp)
    })(Map.empty)

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

        case Expression.TryCatch(exp, rules, tpe, loc) => for {
          _ <- visitExp(exp)
          _ <- State.sequence(rules map {
            case CatchRule(sym, clazz, body) => for {
              _ <- visitExp(body)
            } yield ()
          })
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
      val (_, (varFlow, expFlow)) = visitor(acc)
      val result = lambdas.get(lExp.tpe) match {
        case Some(lams) =>
          lExp.exp match {
            case Expression.Var(sym, tpe, loc) =>
              val flow = varFlow.getOrElse(sym, Set())
              val newFlow = varFlow + (sym -> (flow ++ lams))
              (newFlow, expFlow)
            case _ =>
              val flow = expFlow.getOrElse(lExp, Set())
              val newFlow = expFlow + (lExp -> (flow ++ lams))
              (varFlow, newFlow)
          }
        case None => acc
      }
      ((), result)
    }

    val (_, (varFlow, expFlow)) = State.sequence(labeledDefs map {
      case (_, defn) => visitExp(defn.exp)
    })(Map.empty, Map.empty)

    def inliner(exp0: Expression): Expression =
      exp0 match {
        case LabeledExpression(label, exp) =>
          val e = inliner(exp)
          e

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
          val b = inliner(body)
          Expression.Lambda(args, b, tpe, loc)

        case Expression.Apply(lExp: LabeledExpression, args, tpe, loc) =>
          val lams = lExp.exp match {
            case Expression.Var(sym, tpe, loc) => varFlow.getOrElse(sym, Set())
            case _ => expFlow.getOrElse(lExp, Set())
          }
          if (lams.size == 1) {
            val lam = lams.head
            if (SimplifiedAstOps.freeVars(lam).isEmpty) {
              Expression.Apply(inliner(lam), args, tpe, loc)
            } else {
              Expression.Apply(inliner(lExp), args, tpe, loc)
            }
          } else {
            Expression.Apply(inliner(lExp), args, tpe, loc)
          }

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
            case (sym, br) =>
              val b = inliner(br)
              sym -> b
          }
          Expression.Branch(e, bs, tpe, loc)

        case Expression.JumpTo(sym, tpe, loc) => Expression.JumpTo(sym, tpe, loc)

        case Expression.Let(sym, exp1, exp2, tpe, loc) =>
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

        case Expression.RecordEmpty(tpe, loc) => Expression.RecordEmpty(tpe, loc)

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

        case Expression.ProcessPanic(msg, tpe, loc) => Expression.ProcessPanic(msg, tpe, loc)

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

        case Expression.LambdaClosure(fparams, freeVars, exp, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.Closure(sym, freeVars, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplyClo(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplyDef(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplyCloTail(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplyDefTail(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
      }

    val unlabeledDefs = labeledDefs.map {
      case (sym, defn) => sym -> defn.copy(exp = inliner(defn.exp))
    }

    val result = root.copy(defs = unlabeledDefs.toMap)

    // Print the ast if debugging is enabled.
    if (flix.options.debug) {
      println("--------------CFA RESULT--------------")
      println(PrettyPrinter.Simplified.fmtRoot(result).fmt(TerminalContext.AnsiTerminal))
    }

    result.toSuccess
  }

  trait State[S, A] {
    def apply(s: S): (A, S)

    def map[B](f: A => B): State[S, B] = State { s =>
      val (a, newState) = this(s)
      (f(a), newState)
    }

    def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
      val (a, newState) = this(s)
      f(a)(newState)
    }
  }

  object State {
    def apply[S, A](r: S => (A, S)): State[S, A] = new State[S, A] {
      def apply(s: S) = r(s)
    }

    def sequence[S, A](xs: List[State[S, A]]): State[S, List[A]] = xs match {
      case st :: states => for {
        a <- st
        seq <- sequence(states)
      } yield (a :: seq)
      case Nil => s => (List.empty, s)
    }

    def mapOpt[S, A](opt : Option[State[S, A]]): State[S, Option[A]] = opt match {
      case Some(st) => for {
        a <- st
      } yield Some(a)
      case None => ret(None)
    }

    def ret[S, A](a : A): State[S, A] = State { s =>
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
