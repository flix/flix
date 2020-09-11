package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.Ast.Annotation
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.TypedAst.Expression
import ca.uwaterloo.flix.language.errors.NonTailRecursiveCallError
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}
import ca.uwaterloo.flix.util.Validation.{ToFailure, ToSuccess}

object TailrecCheck extends Phase[TypedAst.Root, TypedAst.Root] {

  override def run(input: TypedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, CompilationError] = {
    val annotatedDefs = input.defs.values.filter(_.ann.exists(_.name.isInstanceOf[Annotation.Tailrec])) // MATT don't we have a better annotation checking mechanism?
    Validation.mapN(Validation.sequence(annotatedDefs.map(tailRecCheck))) {
      _ => input
    }
  }


  private def tailRecCheck(defn0: TypedAst.Def): Validation[Unit, CompilationError] = {
    def visit(exp0: TypedAst.Expression, isTail: Boolean): Validation[Unit, CompilationError] = exp0 match {
      case Expression.Unit(loc) => ().toSuccess
      case Expression.Null(tpe, loc) => ().toSuccess
      case Expression.True(loc) => ().toSuccess
      case Expression.False(loc) => ().toSuccess
      case Expression.Char(lit, loc) => ().toSuccess
      case Expression.Float32(lit, loc) => ().toSuccess
      case Expression.Float64(lit, loc) => ().toSuccess
      case Expression.Int8(lit, loc) => ().toSuccess
      case Expression.Int16(lit, loc) => ().toSuccess
      case Expression.Int32(lit, loc) => ().toSuccess
      case Expression.Int64(lit, loc) => ().toSuccess
      case Expression.BigInt(lit, loc) => ().toSuccess
      case Expression.Str(lit, loc) => ().toSuccess
      case Expression.Default(tpe, loc) => ().toSuccess
      case Expression.Wild(tpe, loc) => ().toSuccess
      case Expression.Var(sym, tpe, loc) => ().toSuccess
      case Expression.Def(sym, tpe, loc) => ().toSuccess
      case Expression.Sig(sym, tpe, loc) => ().toSuccess
      case Expression.Hole(sym, tpe, eff, loc) => ().toSuccess
      case Expression.Lambda(fparam, exp, tpe, loc) => ().toSuccess
      case Expression.Apply(Expression.Def(defn, _, _), exps, tpe, eff, loc) if !isTail && defn == defn0.sym && defn0.fparams.length == exps.length =>
        NonTailRecursiveCallError(loc).toFailure
      case Expression.Apply(exp, exps, tpe, eff, loc) =>
        for {
          _ <- visit(exp, false)
          _ <- Validation.sequence(exps.map(visit(_, false)))
        } yield ()
      case Expression.Unary(op, exp, tpe, eff, loc) => visit(exp, false)
      case Expression.Binary(op, exp1, exp2, tpe, eff, loc) =>
        Validation.mapN(visit(exp1, false), visit(exp2, false)) {
          case _ => ()
        }
      case Expression.Let(sym, exp1, exp2, tpe, eff, loc) =>
        Validation.mapN(visit(exp1, false), visit(exp2, isTail)) {
          case _ => ()
        }
      case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
        Validation.mapN(visit(exp1, false), visit(exp2, isTail), visit(exp3, isTail)) {
          case _ => ()
        }
      case Expression.Stm(exp1, exp2, tpe, eff, loc) =>
        Validation.mapN(visit(exp1, false), visit(exp2, isTail)) {
          case _ => ()
        }
      case Expression.Match(exp, rules, tpe, eff, loc) =>
        for {
          _ <- visit(exp, false)
          _ <- Validation.sequence(rules.map { rule => visit(rule.guard, false) })
          _ <- Validation.sequence(rules.map { rule => visit(rule.exp, isTail) })
        } yield ()
      case Expression.NullMatch(exps, rules, tpe, eff, loc) =>
        for {
          _ <- Validation.sequence(exps.map { exp => visit(exp, false) })
          _ <- Validation.sequence(rules.map { rule => visit(rule.exp, isTail) })
        } yield ()
      case Expression.Tag(sym, tag, exp, tpe, eff, loc) => visit(exp, isTail) // MATT ?
      case Expression.Tuple(elms, tpe, eff, loc) =>
        Validation.mapN(Validation.sequence(elms.map(visit(_, false)))) {
          _: List[Unit] => ()
        }
      case Expression.RecordEmpty(tpe, loc) => ().toSuccess
      case Expression.RecordSelect(exp, label, tpe, eff, loc) => visit(exp, false)
      case Expression.RecordExtend(label, value, rest, tpe, eff, loc) =>
        Validation.mapN(visit(value, false), visit(rest, false)) {
          case _ => ()
        }
      case Expression.RecordRestrict(label, rest, tpe, eff, loc) => visit(rest, false)
      case Expression.ArrayLit(elms, tpe, eff, loc) =>
        Validation.mapN(Validation.sequence(elms.map(visit(_, false)))) {
          _: List[Unit] => ()
        }
      case Expression.ArrayNew(elm, len, tpe, eff, loc) =>
        Validation.mapN(visit(elm, false), visit(len, false)) {
          case _ => ()
        }
      case Expression.ArrayLoad(base, index, tpe, eff, loc) =>
        Validation.mapN(visit(base, false), visit(index, false)) {
          case _ => ()
        }
      case Expression.ArrayLength(base, eff, loc) => visit(base, false)
      case Expression.ArrayStore(base, index, elm, loc) =>
        Validation.mapN(visit(base, false), visit(index, false)) {
          case _ => ()
        }
      case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
        Validation.mapN(visit(base, false), visit(beginIndex, false), visit(base, false)) {
          case _ => ()
        }
      case Expression.Ref(exp, tpe, eff, loc) => visit(exp, false)
      case Expression.Deref(exp, tpe, eff, loc) => visit(exp, false)
      case Expression.Assign(exp1, exp2, tpe, eff, loc) =>
        Validation.mapN(visit(exp1, false), visit(exp2, false)) {
          case _ => ()
        }
      case Expression.Existential(fparam, exp, loc) => visit(exp, false)
      case Expression.Universal(fparam, exp, loc) => visit(exp, false)
      case Expression.Ascribe(exp, tpe, eff, loc) => visit(exp, isTail) // MATT ?
      case Expression.Cast(exp, tpe, eff, loc) => visit(exp, isTail) // MATT ?
      case Expression.TryCatch(exp, rules, tpe, eff, loc) =>
        for {
          _ <- visit(exp, false)
          _ <- Validation.sequence(rules.map { rule => visit(rule.exp, isTail) })
        } yield () // MATT idk how to deal with this but it's going away soon anyway I think?
      case Expression.InvokeConstructor(constructor, args, tpe, eff, loc) =>
        Validation.mapN(Validation.sequence(args.map(visit(_, false)))) {
          _: List[Unit] => ()
        }
      case Expression.InvokeMethod(method, exp, args, tpe, eff, loc) =>
        Validation.mapN(Validation.sequence(args.map(visit(_, false)))) {
          _: List[Unit] => ()
        }
      case Expression.InvokeStaticMethod(method, args, tpe, eff, loc) =>
        Validation.mapN(Validation.sequence(args.map(visit(_, false)))) {
          _: List[Unit] => ()
        }
      case Expression.GetField(field, exp, tpe, eff, loc) => visit(exp, false) // MATT
      case Expression.PutField(field, exp1, exp2, tpe, eff, loc) =>
        Validation.mapN(visit(exp1, false), visit(exp2, false)) {
          case _ => ()
        }
      case Expression.GetStaticField(field, tpe, eff, loc) => ().toSuccess
      case Expression.PutStaticField(field, exp, tpe, eff, loc) => visit(exp, false)
      case Expression.NewChannel(exp, tpe, eff, loc) => visit(exp, false)
      case Expression.GetChannel(exp, tpe, eff, loc) => visit(exp, false)
      case Expression.PutChannel(exp1, exp2, tpe, eff, loc) =>
        Validation.mapN(visit(exp1, false), visit(exp2, false)) {
          case _ => ()
        }
      case Expression.SelectChannel(rules, default, tpe, eff, loc) =>
        for {
          _ <- Validation.sequence(rules.map { rule => visit(rule.exp, isTail) }) // MATT ?
          _ <- default.map(visit(_, isTail)).getOrElse(().toSuccess) // MATT ?
        } yield ()
      case Expression.Spawn(exp, tpe, eff, loc) => visit(exp, false)
      case Expression.Lazy(exp, tpe, loc) => visit(exp, false)
      case Expression.Force(exp, tpe, eff, loc) => visit(exp, false)
      case Expression.FixpointConstraintSet(cs, tpe, loc) => throw InternalCompilerException("Unexpected expression.") // MATT
      case Expression.FixpointCompose(exp1, exp2, tpe, eff, loc) => throw InternalCompilerException("Unexpected expression.") // MATT
      case Expression.FixpointSolve(exp, stf, tpe, eff, loc) => throw InternalCompilerException("Unexpected expression.") // MATT
      case Expression.FixpointProject(name, exp, tpe, eff, loc) => throw InternalCompilerException("Unexpected expression.") // MATT
      case Expression.FixpointEntails(exp1, exp2, tpe, eff, loc) => throw InternalCompilerException("Unexpected expression.") // MATT
      case Expression.FixpointFold(name, exp1, exp2, exp3, tpe, eff, loc) => throw InternalCompilerException("Unexpected expression.") // MATT
    }

    visit(defn0.exp, true)
  }
}
