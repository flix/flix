/*
 *  Copyright 2020 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.TypedAst.{ConstraintParam, _}
import ca.uwaterloo.flix.language.ast.{Ast, SourceLocation, Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.errors.LinterError
import ca.uwaterloo.flix.language.phase.unification.Unification
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Validation}
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps._

import scala.annotation.tailrec

/**
  * The Linter phase checks for any applicable lints within the ast.
  *
  * Possible improvements:
  *
  * - Add support for unification of complex binding constructs such as Match and Select.
  * - Add support for unification of constraint sets.
  * - Add support for the `implies` meta-predicate.
  */
object Linter extends Phase[TypedAst.Root, TypedAst.Root] {

  /**
    * Checks the given AST `root` for lints.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, LinterError] = flix.phase("Linter") {
    // Check if the linter is enabled.
    if (!flix.options.xlinter) {
      return root.toSuccess
    }

    // Find all the lints in the ast.
    val lints = lintsOf(root)

    // Find all the targets the lints should be applied to.
    val targets = targetsOf(root)

    println(s"I found: ${lints.length} lints to match against ${targets.length} defs.") // TODO: Debug

    // Searches for applicable lints in the targets.
    val results = ParOps.parMap(targets, visitDef(_, lints))

    // Report linter errors (if any).
    results.flatten match {
      case xs if xs.isEmpty => root.toSuccess
      case xs => Failure(LazyList.from(xs))
    }
  }

  /**
    * A definition is a target for linting if it is:
    *
    * - a non-benchmark.
    * - a non-lint.
    * - a non-test.
    */
  private def isTarget(defn: TypedAst.Def): Boolean = !isBenchmark(defn.ann) && !isLint(defn.ann) && !isTest(defn.ann)

  /**
    * Searches for lint instances in the given definition `defn0`.
    *
    * Returns [[Nil]] if no lint instances are found.
    */
  private def visitDef(defn: Def, lints: List[Lint])(implicit flix: Flix): List[LinterError] =
    lints.flatMap(visitExp(defn.exp, _))

  /**
    * Searches for instances of the given `lint0` in the given expression `exp0`.
    *
    * Returns [[Nil]] if no lint instances are found.
    */
  private def visitExp(exp0: Expression, lint0: Lint)(implicit flix: Flix): List[LinterError] = {
    val recursiveErrors = exp0 match {

      case Expression.Unit(_) => Nil

      case Expression.True(_) => Nil

      case Expression.False(_) => Nil

      case Expression.Char(_, _) => Nil

      case Expression.Float32(_, _) => Nil

      case Expression.Float64(_, _) => Nil

      case Expression.Int8(_, _) => Nil

      case Expression.Int16(_, _) => Nil

      case Expression.Int32(_, _) => Nil

      case Expression.Int64(_, _) => Nil

      case Expression.BigInt(_, _) => Nil

      case Expression.Str(_, _) => Nil

      case Expression.Wild(_, _) => Nil

      case Expression.Var(_, _, _) => Nil

      case Expression.Def(_, _, _) => Nil

      case Expression.Hole(_, _, _, _) => Nil

      case Expression.Lambda(_, exp, _, _) => visitExp(exp, lint0)

      case Expression.Apply(exp, exps, _, _, _) => visitExp(exp, lint0) ::: exps.flatMap(visitExp(_, lint0))

      case Expression.Unary(_, exp, _, _, _) => visitExp(exp, lint0)

      case Expression.Binary(_, exp1, exp2, _, _, _) => visitExp(exp1, lint0) ::: visitExp(exp2, lint0)

      case Expression.Let(_, exp1, exp2, _, _, _) => visitExp(exp1, lint0) ::: visitExp(exp2, lint0)

      case Expression.IfThenElse(exp1, exp2, exp3, _, _, _) => visitExp(exp1, lint0) ::: visitExp(exp2, lint0) ::: visitExp(exp3, lint0)

      case Expression.Stm(exp1, exp2, _, _, _) => visitExp(exp1, lint0) ::: visitExp(exp2, lint0)

      case Expression.Match(exp, rules, _, _, _) =>
        rules.foldLeft(visitExp(exp, lint0)) {
          case (acc, MatchRule(_, guard, body)) => visitExp(guard, lint0) ::: visitExp(body, lint0) ::: acc
        }

      case Expression.MatchNull(exps, rules, _, _, _) =>
        exps.flatMap(visitExp(_, lint0)) ++ rules.flatMap {
          case MatchNullRule(_, exp) => visitExp(exp, lint0)
        }

      case Expression.Tag(_, _, exp, _, _, _) => visitExp(exp, lint0)

      case Expression.Tuple(exps, _, _, _) => exps.flatMap(visitExp(_, lint0))

      case Expression.RecordEmpty(_, _) => Nil

      case Expression.RecordSelect(exp, _, _, _, _) => visitExp(exp, lint0)

      case Expression.RecordExtend(_, exp1, exp2, _, _, _) => visitExp(exp1, lint0) ::: visitExp(exp2, lint0)

      case Expression.RecordRestrict(_, exp, _, _, _) => visitExp(exp, lint0)

      case Expression.ArrayLit(exps, _, _, _) => exps.flatMap(visitExp(_, lint0))

      case Expression.ArrayNew(exp1, exp2, _, _, _) => visitExp(exp1, lint0) ::: visitExp(exp2, lint0)

      case Expression.ArrayLoad(exp1, exp2, _, _, _) => visitExp(exp1, lint0) ::: visitExp(exp2, lint0)

      case Expression.ArrayLength(exp, _, _) => visitExp(exp, lint0)

      case Expression.ArrayStore(exp1, exp2, exp3, _) => visitExp(exp1, lint0) ::: visitExp(exp2, lint0) ::: visitExp(exp3, lint0)

      case Expression.ArraySlice(exp1, exp2, exp3, _, _) => visitExp(exp1, lint0) ::: visitExp(exp2, lint0) ::: visitExp(exp3, lint0)

      case Expression.Ref(exp, _, _, _) => visitExp(exp, lint0)

      case Expression.Deref(exp, _, _, _) => visitExp(exp, lint0)

      case Expression.Assign(exp1, exp2, _, _, _) => visitExp(exp1, lint0) ::: visitExp(exp2, lint0)

      case Expression.Existential(_, exp, _) => visitExp(exp, lint0)

      case Expression.Universal(_, exp, _) => visitExp(exp, lint0)

      case Expression.Ascribe(exp, _, _, _) => visitExp(exp, lint0)

      case Expression.Cast(exp, _, _, _) => visitExp(exp, lint0)

      case Expression.TryCatch(exp, rules, _, _, _) =>
        rules.foldLeft(visitExp(exp, lint0)) {
          case (acc, CatchRule(_, _, body)) => visitExp(body, lint0) ::: acc
        }

      case Expression.InvokeConstructor(_, exps, _, _, _) => exps.flatMap(visitExp(_, lint0))

      case Expression.InvokeMethod(_, exp, exps, _, _, _) => visitExp(exp, lint0) ::: exps.flatMap(visitExp(_, lint0))

      case Expression.InvokeStaticMethod(_, exps, _, _, _) => exps.flatMap(visitExp(_, lint0))

      case Expression.GetField(_, exp, _, _, _) => visitExp(exp, lint0)

      case Expression.PutField(_, exp1, exp2, _, _, _) => visitExp(exp1, lint0) ::: visitExp(exp2, lint0)

      case Expression.GetStaticField(_, _, _, _) => Nil

      case Expression.PutStaticField(_, exp, _, _, _) => visitExp(exp, lint0)

      case Expression.NewChannel(exp, _, _, _) => visitExp(exp, lint0)

      case Expression.GetChannel(exp, _, _, _) => visitExp(exp, lint0)

      case Expression.PutChannel(exp1, exp2, _, _, _) => visitExp(exp1, lint0) ::: visitExp(exp2, lint0)

      case Expression.SelectChannel(rules, default, tpe, eff, loc) =>
        rules.foldLeft(default.map(visitExp(_, lint0)).getOrElse(Nil)) {
          case (acc, SelectChannelRule(_, chan, body)) => visitExp(chan, lint0) ::: visitExp(body, lint0) ::: acc
        }

      case Expression.Spawn(exp, _, _, _) => visitExp(exp, lint0)

      case Expression.FixpointConstraintSet(cs, _, _) => cs.flatMap(visitConstraint(_, lint0))

      case Expression.FixpointCompose(exp1, exp2, _, _, _) => visitExp(exp1, lint0) ::: visitExp(exp2, lint0)

      case Expression.FixpointSolve(exp, _, _, _, _) => visitExp(exp, lint0)

      case Expression.FixpointProject(_, exp, _, _, _) => visitExp(exp, lint0)

      case Expression.FixpointEntails(exp1, exp2, tpe, _, _) => visitExp(exp1, lint0) ::: visitExp(exp2, lint0)

      case Expression.FixpointFold(_, exp1, exp2, exp3, _, _, _) => visitExp(exp1, lint0) ::: visitExp(exp2, lint0) ::: visitExp(exp3, lint0)

      case _ => Nil
    }

    tryLint(exp0, lint0) ::: recursiveErrors
  }

  /**
    * Searches for instances of the given `lint0` in the given constraint `c0`.
    *
    * Returns [[Nil]] if no lint instances are found.
    */
  private def visitConstraint(c0: Constraint, lint0: Lint)(implicit flix: Flix): List[LinterError] = c0 match {
    case Constraint(_, head, body, loc) => body.foldLeft(visitHead(head, lint0)) {
      case (acc, body0) => visitBody(body0, lint0) ::: acc
    }
  }

  /**
    * Searches for instances of the given `lint0` in the given head predicate `head0`.
    *
    * Returns [[Nil]] if no lint instances are found.
    */
  private def visitHead(head0: Predicate.Head, lint0: Lint)(implicit flix: Flix): List[LinterError] = head0 match {
    case Head.Atom(_, _, terms, _, _) => terms.flatMap(visitExp(_, lint0))
    case Head.Union(exp, _, _) => visitExp(exp, lint0)
  }

  /**
    * Searches for instances of the given `lint0` in the given body predicate `body0`.
    *
    * Returns [[Nil]] if no lint instances are found.
    */
  private def visitBody(body0: Predicate.Body, lint0: Lint)(implicit flix: Flix): List[LinterError] = body0 match {
    case Body.Atom(_, _, _, _, _, _) => Nil
    case Body.Guard(exp, _) => visitExp(exp, lint0)
  }

  /**
    * Attempts to apply the given `lint0` to the given expression `exp0`.
    *
    * Returns [[Nil]] if the lint is not applicable.
    */
  private def tryLint(exp0: Expression, lint0: Lint)(implicit flix: Flix): List[LinterError] = {
    val metaVars = lint0.quantifiers.map(_.sym)
    unifyExp(lint0.pattern, exp0, metaVars) match {
      case None => Nil
      case Some(subst) => LinterError.Lint(lint0.sym, subst(lint0.replacement), exp0.loc) :: Nil
    }
  }

  /**
    * Unifies the given symbol `sym1` of type `tpe1` with the symbol `sym2` of type `tpe2` if the `tpe2` is an instance of `tpe1`.
    *
    * NB: Only intended for program variables.
    */
  private def unifyVar(sym1: Symbol.VarSym, tpe1: Type, sym2: Symbol.VarSym, tpe2: Type)(implicit flix: Flix): Option[Substitution] = {
    if (Unification.isInstance(tpe2, tpe1))
      Some(Substitution.singleton(sym1, Expression.Var(sym2, tpe2, SourceLocation.Unknown)))
    else
      None
  }

  /**
    * Optionally returns a substitution that makes `lint0` and `exp0` equal.
    *
    * NB: Unification is left-biased: variables in the expression are not unified against the lint.
    */
  private def unifyExp(lint0: Expression, exp0: Expression, metaVars: List[Symbol.VarSym])(implicit flix: Flix): Option[Substitution] = (lint0, exp0) match {
    case (Expression.Unit(_), Expression.Unit(_)) => Some(Substitution.empty)

    case (Expression.True(_), Expression.True(_)) => Some(Substitution.empty)

    case (Expression.False(_), Expression.False(_)) => Some(Substitution.empty)

    case (Expression.Char(lit1, _), Expression.Char(lit2, _)) if lit1 == lit2 => Some(Substitution.empty)

    case (Expression.Float32(lit1, _), Expression.Float32(lit2, _)) if lit1 == lit2 => Some(Substitution.empty)

    case (Expression.Float64(lit1, _), Expression.Float64(lit2, _)) if lit1 == lit2 => Some(Substitution.empty)

    case (Expression.Int8(lit1, _), Expression.Int8(lit2, _)) if lit1 == lit2 => Some(Substitution.empty)

    case (Expression.Int16(lit1, _), Expression.Int16(lit2, _)) if lit1 == lit2 => Some(Substitution.empty)

    case (Expression.Int32(lit1, _), Expression.Int32(lit2, _)) if lit1 == lit2 => Some(Substitution.empty)

    case (Expression.Int64(lit1, _), Expression.Int64(lit2, _)) if lit1 == lit2 => Some(Substitution.empty)

    case (Expression.BigInt(lit1, _), Expression.BigInt(lit2, _)) if lit1 == lit2 => Some(Substitution.empty)

    case (Expression.Str(lit1, _), Expression.Str(lit2, _)) if lit1 == lit2 => Some(Substitution.empty)

    case (Expression.Wild(_, _), Expression.Wild(_, _)) => Some(Substitution.empty)

    case (Expression.Var(sym, varTyp, _), _) =>
      // The type of the expression.
      val expTyp = exp0.tpe

      // Determine if we are unifying a meta-variable.
      if (metaVars.contains(sym)) {
        // Case 1: We are unifying a meta-variable.
        // Determine if we can unify the type of the meta-variable and the expression.
        if (Unification.isInstance(varTyp, expTyp))
          Some(Substitution.singleton(sym, exp0))
        else
          None
      } else {
        // Case 2: We are unifying a program variable. The variable must be exactly the same.
        exp0 match {
          case Expression.Var(otherSym, _, _) if sym == otherSym =>
            // Case 2.1: The two variables are the same. Unification succeeds.
            Some(Substitution.empty)
          case _ =>
            // Case 2.2: The two variables are different or one is an expression. Unification fails.
            None
        }
      }

    case (_, Expression.Var(sym, varTyp, _)) =>
      // NB: Unification is left-biased so there is no case for a variable on the rhs.
      None

    case (Expression.Def(sym1, _, _), Expression.Def(sym2, _, _)) if sym1 == sym2 => Some(Substitution.empty)

    case (Expression.Hole(sym1, _, _, _), Expression.Hole(sym2, _, _, _)) if sym1 == sym2 => Some(Substitution.empty)

    case (Expression.Lambda(fparam1, exp1, _, _), Expression.Lambda(fparam2, exp2, _, _)) =>
      for {
        s1 <- unifyVar(fparam1.sym, fparam1.tpe, fparam2.sym, fparam2.tpe)
        s2 <- unifyExp(s1(exp1), s1(exp2), metaVars)
      } yield s2 @@ s1

    case (Expression.Apply(exp1, exps1, _, _, _), Expression.Apply(exp2, exps2, _, _, _)) =>
      for {
        s1 <- unifyExp(exp1, exp2, metaVars)
        s2 <- unifyExps(exps1, exps2, metaVars)
      } yield s2 @@ s1

    case (Expression.Unary(op1, exp1, _, _, _), Expression.Unary(op2, exp2, _, _, _)) if op1 == op2 =>
      unifyExp(exp1, exp2, metaVars)

    case (Expression.Binary(op1, exp11, exp12, _, _, _), Expression.Binary(op2, exp21, exp22, _, _, _)) if op1 == op2 =>
      unifyExp(exp11, exp12, exp21, exp22, metaVars)

    case (Expression.Let(sym1, exp11, exp12, _, _, _), Expression.Let(sym2, exp21, exp22, _, _, _)) =>
      for {
        s1 <- unifyVar(sym1, exp11.tpe, sym2, exp21.tpe)
        s2 <- unifyExp(s1(exp12), s1(exp22), metaVars)
      } yield s2 @@ s1

    case (Expression.IfThenElse(exp11, exp12, exp13, _, _, _), Expression.IfThenElse(exp21, exp22, exp23, _, _, _)) =>
      unifyExp(exp11, exp12, exp13, exp21, exp22, exp23, metaVars)

    case (Expression.Stm(exp11, exp12, _, _, _), Expression.Stm(exp21, exp22, _, _, _)) =>
      unifyExp(exp11, exp12, exp21, exp22, metaVars)

    case (Expression.Match(_, _, _, _, _), _) =>
      // NB: We currently do not perform unification of complex binding constructs.
      None

    case (_, Expression.Match(_, _, _, _, _)) =>
      // NB: We currently do not perform unification of complex binding constructs.
      None

    case (Expression.Tag(sym1, tag1, exp1, _, _, _), Expression.Tag(sym2, tag2, exp2, _, _, _)) if sym1 == sym2 && tag1 == tag2 =>
      unifyExp(exp1, exp2, metaVars)

    case (Expression.Tuple(exps1, _, _, _), Expression.Tuple(exps2, _, _, _)) =>
      unifyExps(exps1, exps2, metaVars)

    case (Expression.RecordEmpty(_, _), Expression.RecordEmpty(_, _)) => Some(Substitution.empty)

    case (Expression.RecordSelect(exp1, _, _, _, _), Expression.RecordSelect(exp2, _, _, _, _)) =>
      unifyExp(exp1, exp2, metaVars)

    case (Expression.RecordExtend(_, exp11, exp12, _, _, _), Expression.RecordExtend(_, exp21, exp22, _, _, _)) =>
      unifyExp(exp11, exp12, exp21, exp22, metaVars)

    case (Expression.RecordRestrict(_, exp1, _, _, _), Expression.RecordRestrict(_, exp2, _, _, _)) =>
      unifyExp(exp1, exp2, metaVars)

    case (Expression.ArrayLit(exps1, _, _, _), Expression.ArrayLit(exps2, _, _, _)) =>
      unifyExps(exps1, exps2, metaVars)

    case (Expression.ArrayNew(exp11, exp12, _, _, _), Expression.ArrayNew(exp21, exp22, _, _, _)) =>
      unifyExp(exp11, exp12, exp21, exp22, metaVars)

    case (Expression.ArrayLoad(exp11, exp12, _, _, _), Expression.ArrayLoad(exp21, exp22, _, _, _)) =>
      unifyExp(exp11, exp12, exp21, exp22, metaVars)

    case (Expression.ArrayLength(exp1, _, _), Expression.ArrayLength(exp2, _, _)) =>
      unifyExp(exp1, exp2, metaVars)

    case (Expression.ArrayStore(exp11, exp12, exp13, _), Expression.ArrayStore(exp21, exp22, exp23, _)) =>
      unifyExp(exp11, exp12, exp13, exp21, exp22, exp23, metaVars)

    case (Expression.ArraySlice(exp11, exp12, exp13, _, _), Expression.ArraySlice(exp21, exp22, exp23, _, _)) =>
      unifyExp(exp11, exp12, exp13, exp21, exp22, exp23, metaVars)

    case (Expression.Ref(exp1, _, _, _), Expression.Ref(exp2, _, _, _)) =>
      unifyExp(exp1, exp2, metaVars)

    case (Expression.Deref(exp1, _, _, _), Expression.Deref(exp2, _, _, _)) =>
      unifyExp(exp1, exp2, metaVars)

    case (Expression.Assign(exp11, exp12, _, _, _), Expression.Assign(exp21, exp22, _, _, _)) =>
      unifyExp(exp11, exp12, exp21, exp22, metaVars)

    case (Expression.Existential(_, _, _), _) =>
      // An existentially quantified expression never unifies with anything.
      None

    case (_, Expression.Existential(_, _, _)) =>
      // An existentially quantified expression never unifies with anything.
      None

    case (Expression.Universal(_, _, _), _) =>
      // A universally quantified expression never unifies with anything.
      None

    case (_, Expression.Universal(_, _, _)) =>
      // A universally quantified expression never unifies with anything.
      None

    case (Expression.Ascribe(exp1, _, _, _), Expression.Ascribe(exp2, _, _, _)) =>
      unifyExp(exp1, exp2, metaVars)

    case (Expression.Cast(exp1, _, _, _), Expression.Cast(exp2, _, _, _)) =>
      unifyExp(exp1, exp2, metaVars)

    case (Expression.TryCatch(_, _, _, _, _), _) =>
      // NB: We currently do not perform unification of complex binding constructs.
      None

    case (_, Expression.TryCatch(_, _, _, _, _)) =>
      // NB: We currently do not perform unification of complex binding constructs.
      None

    case (Expression.InvokeConstructor(constructor1, exps1, _, _, _), Expression.InvokeConstructor(constructor2, exps2, _, _, _)) =>
      unifyExps(exps1, exps2, metaVars)

    case (Expression.InvokeMethod(method1, exp1, exps1, _, _, _), Expression.InvokeMethod(method2, exp2, exps2, _, _, _)) if method1 == method2 =>
      for {
        s1 <- unifyExp(exp1, exp2, metaVars)
        s2 <- unifyExps(exps1.map(s1.apply), exps2.map(s1.apply), metaVars)
      } yield s2 @@ s1

    case (Expression.InvokeStaticMethod(method1, exps1, _, _, _), Expression.InvokeStaticMethod(method2, exps2, _, _, _)) if method1 == method2 =>
      unifyExps(exps1, exps2, metaVars)

    case (Expression.GetField(field1, exp1, _, _, _), Expression.GetField(field2, exp2, _, _, _)) if field1 == field2 =>
      unifyExp(exp1, exp2, metaVars)

    case (Expression.PutField(field1, exp11, exp12, _, _, _), Expression.PutField(field2, exp21, exp22, _, _, _)) if field1 == field2 =>
      unifyExp(exp11, exp12, exp21, exp22, metaVars)

    case (Expression.GetStaticField(field1, _, _, _), Expression.GetStaticField(field2, _, _, _)) if field1 == field2 => Some(Substitution.empty)

    case (Expression.PutStaticField(field1, exp1, _, _, _), Expression.PutStaticField(field2, exp2, _, _, _)) if field1 == field2 =>
      unifyExp(exp1, exp2, metaVars)

    case (Expression.NewChannel(exp1, _, _, _), Expression.NewChannel(exp2, _, _, _)) =>
      unifyExp(exp1, exp2, metaVars)

    case (Expression.GetChannel(exp1, _, _, _), Expression.GetChannel(exp2, _, _, _)) =>
      unifyExp(exp1, exp2, metaVars)

    case (Expression.PutChannel(exp11, exp12, _, _, _), Expression.PutChannel(exp21, exp22, _, _, _)) =>
      unifyExp(exp11, exp12, exp21, exp22, metaVars)

    case (Expression.SelectChannel(_, _, _, _, _), _) =>
      // NB: We currently do not perform unification of complex binding constructs.
      None

    case (_, Expression.SelectChannel(_, _, _, _, _)) =>
      // NB: We currently do not perform unification of complex binding constructs.
      None

    case (Expression.Spawn(exp1, _, _, _), Expression.Spawn(exp2, _, _, _)) =>
      unifyExp(exp1, exp2, metaVars)

    case (Expression.FixpointConstraintSet(_, _, _), Expression.FixpointConstraintSet(_, _, _)) =>
      // NB: We currently do not perform unification inside constraint sets.
      None

    case (Expression.FixpointCompose(exp11, exp12, _, _, _), Expression.FixpointCompose(exp21, exp22, _, _, _)) =>
      unifyExp(exp11, exp12, exp21, exp22, metaVars)

    case (Expression.FixpointSolve(exp1, _, _, _, _), Expression.FixpointSolve(exp2, _, _, _, _)) =>
      unifyExp(exp1, exp2, metaVars)

    case (Expression.FixpointProject(name1, exp1, _, _, _), Expression.FixpointProject(name2, exp2, _, _, _)) if name1 == name2 =>
      unifyExp(exp1, exp2, metaVars)

    case (Expression.FixpointEntails(exp11, exp12, _, _, _), Expression.FixpointEntails(exp21, exp22, _, _, _)) =>
      unifyExp(exp11, exp12, exp21, exp22, metaVars)

    case (Expression.FixpointFold(name1, exp11, exp12, exp13, _, _, _), Expression.FixpointFold(name2, exp21, exp22, exp23, _, _, _)) if name1 == name2 =>
      unifyExp(exp11, exp12, exp13, exp21, exp22, exp23, metaVars)

    case _ => None

  }

  /**
    * Optionally returns a substitution that makes `exp11` equal to `exp21` and `exp12` equal to `exp22`.
    */
  private def unifyExp(exp11: Expression, exp12: Expression, exp21: Expression, exp22: Expression, metaVars: List[Symbol.VarSym])(implicit flix: Flix): Option[Substitution] = {
    for {
      s1 <- unifyExp(exp11, exp21, metaVars)
      s2 <- unifyExp(s1(exp12), s1(exp22), metaVars)
    } yield s2 @@ s1
  }

  /**
    * Optionally returns a substitution that makes `exp11` equal to `exp21`, `exp12` equal to `exp22`, and `exp13` equal to `exp23`.
    */
  private def unifyExp(exp11: Expression, exp12: Expression, exp13: Expression, exp21: Expression, exp22: Expression, exp23: Expression, metaVars: List[Symbol.VarSym])(implicit flix: Flix): Option[Substitution] = {
    for {
      s1 <- unifyExp(exp11, exp21, metaVars)
      s2 <- unifyExp(s1(exp12), s1(exp22), metaVars)
      s3 <- unifyExp(s2(exp13), s2(exp23), metaVars) // TODO: What about s32?
    } yield s3 @@ s2 @@ s1
  }

  /**
    * Optionally returns a substitution that makes `l1` and `l2` equal.
    */
  private def unifyExps(l1: List[Expression], l2: List[Expression], metaVars: List[Symbol.VarSym])(implicit flix: Flix): Option[Substitution] = (l1, l2) match {
    case (Nil, Nil) => Some(Substitution.empty)
    case (Nil, _) => None
    case (_, Nil) => None
    case (x :: xs, y :: ys) =>
      for {
        s1 <- unifyExp(x, y, metaVars)
        s2 <- unifyExps(xs.map(s1.apply), ys.map(s1.apply), metaVars)
      } yield s2 @@ s1
  }

  /**
    * Returns all lints in the given AST `root`.
    */
  private def lintsOf(root: Root): List[Lint] = root.defs.foldLeft(Nil: List[Lint]) {
    case (acc, (sym, defn)) if isLint(defn.ann) => lintOf(sym, defn.exp) match {
      case None => acc
      case Some(l) => l :: acc
    }
    case (acc, (sym, defn)) => acc
  }

  /**
    * Returns the quantifiers and inner expression of the given (optionally) quantified expression `exp0`.
    */
  private def splitQuantifiersAndBody(exp0: Expression): (List[FormalParam], Expression) = {
    @tailrec
    def visit(exp0: Expression, acc: List[FormalParam]): (List[FormalParam], Expression) = exp0 match {
      case Expression.Universal(fparam, exp, loc) => visit(exp, fparam :: acc)
      case _ => (acc, exp0)
    }

    visit(exp0, Nil)
  }

  /**
    * Returns all non-lints definitions in the given AST `root`.
    */
  private def targetsOf(root: Root): List[Def] = root.defs.foldLeft(Nil: List[Def]) {
    case (acc, (sym, defn)) => if (isTarget(defn)) defn :: acc else acc
  }

  /**
    * Return the lint from the given symbol `sym` and expression `exp0`.
    */
  private def lintOf(sym: Symbol.DefnSym, exp0: Expression): Option[Lint] = {
    // The contextual equivalence symbol.
    val contextEqSym = Symbol.mkDefnSym("===")

    // The implication symbol.
    val implies = Symbol.mkDefnSym("implies")

    // Split the expression into its parts.
    val (quantifiers, base) = splitQuantifiersAndBody(exp0)

    // Determine the type of lint.
    base match {
      case Expression.Apply(Expression.Apply(Expression.Def(lintSym, _, _), left, _, _, _), right, _, _, _) =>
        if (lintSym == contextEqSym) {
          Some(Lint(sym, quantifiers, left.head, right.head))
        } else if (lintSym == implies) {
          // TODO: Add support for additional lints.
          println(s"Unsupported lint: $sym ")
          None
        } else {
          // TODO: Add support for additional lints.
          println(s"Unsupported lint: $sym ")
          None
        }
      case _ =>
        println(s"Malformed lint:   $sym ")
        None
    }
  }

  /**
    * Represents a lint.
    *
    * @param sym         the symbol of the lint.
    * @param quantifiers the quantifiers of the lint.
    * @param pattern     the pattern of the lint.
    * @param replacement the suggested replacement for the pattern.
    */
  case class Lint(sym: Symbol.DefnSym, quantifiers: List[FormalParam], pattern: Expression, replacement: Expression)

  /**
    * Companion object for the [[Substitution]] class.
    */
  object Substitution {
    /**
      * Represents the empty substitution.
      */
    val empty: Substitution = Substitution(Map.empty)

    /**
      * Returns a singleton substitution with a mapping from `sym` to `exp0`.
      */
    def singleton(sym: Symbol.VarSym, exp0: Expression): Substitution = Substitution(Map(sym -> exp0))
  }

  /**
    * A substitution is a map from variable symbols to expressions.
    */
  case class Substitution(m: Map[Symbol.VarSym, Expression]) {

    /**
      * Returns `true` if `this` is the empty substitution.
      */
    val isEmpty: Boolean = m.isEmpty

    /**
      * Applies the substitution to the variable symbol `sym`.
      */
    def apply(sym: Symbol.VarSym): Symbol.VarSym = m.get(sym) match {
      case None => sym
      case Some(Expression.Var(otherSym, _, _)) => otherSym
      case Some(exp) => throw InternalCompilerException(s"Unexpected non-variable expression: $exp.")
    }

    /**
      * Applies the substitution to the expression `exp0`.
      */
    def apply(exp0: Expression): Expression = exp0 match {
      case Expression.Unit(_) => exp0

      case Expression.Null(_, _) => exp0

      case Expression.True(_) => exp0

      case Expression.False(_) => exp0

      case Expression.Char(_, _) => exp0

      case Expression.Float32(_, _) => exp0

      case Expression.Float64(_, _) => exp0

      case Expression.Int8(_, _) => exp0

      case Expression.Int16(_, _) => exp0

      case Expression.Int32(_, _) => exp0

      case Expression.Int64(_, _) => exp0

      case Expression.BigInt(_, _) => exp0

      case Expression.Str(_, _) => exp0

      case Expression.Wild(_, _) => exp0

      case Expression.Var(sym, _, _) => m.get(sym) match {
        case None => exp0
        case Some(e) => e
      }

      case Expression.Def(_, _, _) => exp0

      case Expression.Hole(_, _, _, _) => exp0

      case Expression.Lambda(fparam, exp, tpe, loc) =>
        val f = apply(fparam)
        val e = apply(exp)
        Expression.Lambda(f, e, tpe, loc)

      case Expression.Apply(exp, exps, tpe, eff, loc) =>
        val e = apply(exp)
        val es = exps.map(apply)
        Expression.Apply(e, es, tpe, eff, loc)

      case Expression.Unary(op, exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.Unary(op, e, tpe, eff, loc)

      case Expression.Binary(op, exp1, exp2, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        Expression.Binary(op, e1, e2, tpe, eff, loc)

      case Expression.Let(sym, exp1, exp2, tpe, eff, loc) =>
        val newSym = apply(sym)
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        Expression.Let(newSym, e1, e2, tpe, eff, loc)

      case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        val e3 = apply(exp3)
        Expression.IfThenElse(e1, e2, e3, tpe, eff, loc)

      case Expression.Stm(exp1, exp2, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        Expression.Stm(e1, e2, tpe, eff, loc)

      case Expression.Match(exp, rules, tpe, eff, loc) =>
        val e = apply(exp)
        val rs = rules.map {
          case MatchRule(pat, guard, exp) => MatchRule(apply(pat), apply(guard), apply(exp))
        }
        Expression.Match(e, rs, tpe, eff, loc)

      case Expression.MatchNull(exps, rules, tpe, eff, loc) =>
        val es = exps.map(apply)
        val rs = rules.map {
          case MatchNullRule(pat, exp) => MatchNullRule(pat, apply(exp))
        }
        Expression.MatchNull(es, rs, tpe, eff, loc)

      case Expression.Tag(sym, tag, exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.Tag(sym, tag, e, tpe, eff, loc)

      case Expression.Tuple(elms, tpe, eff, loc) =>
        val es = elms.map(apply)
        Expression.Tuple(es, tpe, eff, loc)

      case Expression.RecordEmpty(_, _) => exp0

      case Expression.RecordSelect(exp, label, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.RecordSelect(e, label, tpe, eff, loc)

      case Expression.RecordExtend(label, exp1, exp2, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        Expression.RecordExtend(label, e1, e2, tpe, eff, loc)

      case Expression.RecordRestrict(label, exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.RecordRestrict(label, e, tpe, eff, loc)

      case Expression.ArrayLit(elms, tpe, eff, loc) =>
        val es = elms.map(apply)
        Expression.ArrayLit(es, tpe, eff, loc)

      case Expression.ArrayNew(exp1, exp2, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        Expression.ArrayNew(e1, e2, tpe, eff, loc)

      case Expression.ArrayLoad(exp1, exp2, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        Expression.ArrayLoad(e1, e2, tpe, eff, loc)

      case Expression.ArrayLength(exp, eff, loc) =>
        val e = apply(exp)
        Expression.ArrayLength(e, eff, loc)

      case Expression.ArrayStore(exp1, exp2, exp3, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        val e3 = apply(exp3)
        Expression.ArrayStore(e1, e2, e3, loc)

      case Expression.ArraySlice(exp1, exp2, exp3, tpe, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        val e3 = apply(exp3)
        Expression.ArraySlice(e1, e2, e3, tpe, loc)

      case Expression.Ref(exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.Ref(e, tpe, eff, loc)

      case Expression.Deref(exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.Deref(e, tpe, eff, loc)

      case Expression.Assign(exp1, exp2, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        Expression.Assign(e1, e2, tpe, eff, loc)

      case Expression.Ascribe(exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.Ascribe(e, tpe, eff, loc)

      case Expression.Cast(exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.Cast(e, tpe, eff, loc)

      case Expression.TryCatch(exp, rules, tpe, eff, loc) =>
        val e = apply(exp)
        val rs = rules.map {
          case CatchRule(sym, clazz, body) => CatchRule(apply(sym), clazz, apply(body))
        }
        Expression.TryCatch(e, rs, tpe, eff, loc)

      case Expression.InvokeConstructor(constructor, args, tpe, eff, loc) =>
        val as = args.map(apply)
        Expression.InvokeConstructor(constructor, as, tpe, eff, loc)

      case Expression.InvokeMethod(method, exp, args, tpe, eff, loc) =>
        val e = apply(exp)
        val as = args.map(apply)
        Expression.InvokeMethod(method, e, as, tpe, eff, loc)

      case Expression.InvokeStaticMethod(method, args, tpe, eff, loc) =>
        val as = args.map(apply)
        Expression.InvokeStaticMethod(method, as, tpe, eff, loc)

      case Expression.GetField(field, exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.GetField(field, e, tpe, eff, loc)

      case Expression.PutField(field, exp1, exp2, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        Expression.PutField(field, e1, e2, tpe, eff, loc)

      case Expression.GetStaticField(field, tpe, eff, loc) => exp0

      case Expression.PutStaticField(field, exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.PutStaticField(field, e, tpe, eff, loc)

      case Expression.NewChannel(exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.NewChannel(e, tpe, eff, loc)

      case Expression.GetChannel(exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.GetChannel(e, tpe, eff, loc)

      case Expression.PutChannel(exp1, exp2, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        Expression.PutChannel(e1, e2, tpe, eff, loc)

      case Expression.SelectChannel(rules, default, tpe, eff, loc) =>
        val d = default.map(apply)
        val rs = rules.map {
          case SelectChannelRule(sym, chan, body) => SelectChannelRule(apply(sym), apply(chan), apply(body))
        }
        Expression.SelectChannel(rs, d, tpe, eff, loc)

      case Expression.Spawn(exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.Spawn(e, tpe, eff, loc)

      case Expression.FixpointConstraintSet(cs, tpe, loc) =>
        Expression.FixpointConstraintSet(cs.map(apply), tpe, loc)

      case Expression.FixpointCompose(exp1, exp2, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        Expression.FixpointCompose(e1, e2, tpe, eff, loc)

      case Expression.FixpointSolve(exp, stf, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.FixpointSolve(e, stf, tpe, eff, loc)

      case Expression.FixpointProject(name, exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.FixpointProject(name, e, tpe, eff, loc)

      case Expression.FixpointEntails(exp1, exp2, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        Expression.FixpointEntails(e1, e2, tpe, eff, loc)

      case Expression.FixpointFold(name, exp1, exp2, exp3, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        val e3 = apply(exp3)
        Expression.FixpointFold(name, e1, e2, e3, tpe, eff, loc)

      case Expression.Existential(_, _, _) => throw InternalCompilerException(s"Unexpected expression: $exp0.")

      case Expression.Universal(_, _, _) => throw InternalCompilerException(s"Unexpected expression: $exp0.")
    }

    /**
      * Applies the substitution to the pattern `pat0`.
      */
    def apply(pat0: Pattern): Pattern = pat0 match {
      case Pattern.Wild(_, _) => pat0
      case Pattern.Var(sym, _, _) => m.get(sym) match {
        case None => pat0
        case Some(Expression.Var(otherSym, tpe, loc)) => Pattern.Var(otherSym, tpe, loc)
        case Some(exp) => throw InternalCompilerException(s"Unexpected non-variable expression: $exp.")
      }

      case Pattern.Unit(_) => pat0

      case Pattern.True(_) => pat0

      case Pattern.False(_) => pat0

      case Pattern.Char(_, _) => pat0

      case Pattern.Float32(_, _) => pat0

      case Pattern.Float64(_, _) => pat0

      case Pattern.Int8(_, _) => pat0

      case Pattern.Int16(_, _) => pat0

      case Pattern.Int32(_, _) => pat0

      case Pattern.Int64(_, _) => pat0

      case Pattern.BigInt(_, _) => pat0

      case Pattern.Str(_, _) => pat0

      case Pattern.Tag(sym, tag, pat, tpe, loc) =>
        val p = apply(pat)
        Pattern.Tag(sym, tag, p, tpe, loc)

      case Pattern.Tuple(elms, tpe, loc) =>
        val ps = elms.map(apply)
        Pattern.Tuple(ps, tpe, loc)

      case Pattern.Array(elms, tpe, loc) =>
        val ps = elms.map(apply)
        Pattern.Array(ps, tpe, loc)

      case Pattern.ArrayTailSpread(elms, sym, tpe, loc) =>
        val ps = elms.map(apply)
        val newSym = apply(sym)
        Pattern.ArrayTailSpread(ps, newSym, tpe, loc)

      case Pattern.ArrayHeadSpread(sym, elms, tpe, loc) =>
        val ps = elms.map(apply)
        val newSym = apply(sym)
        Pattern.ArrayHeadSpread(newSym, ps, tpe, loc)
    }

    /**
      * Applies the substitution to the constraint `c0`.
      */
    private def apply(c0: Constraint): Constraint = c0 match {
      case Constraint(cparams, head, body, loc) => Constraint(cparams.map(apply), apply(head), body.map(apply), loc)
    }

    /**
      * Applies the substitution to the head predicate `head0`.
      */
    private def apply(head0: Predicate.Head): Predicate.Head = head0 match {
      case Head.Atom(name, den, terms, tpe, loc) => Head.Atom(name, den, terms.map(apply), tpe, loc)
      case Head.Union(exp, tpe, loc) => Head.Union(apply(exp), tpe, loc)
    }

    /**
      * Applies the substitution to the body predicate `body0`.
      */
    private def apply(body0: Predicate.Body): Predicate.Body = body0 match {
      case Body.Atom(name, den, polarity, terms, tpe, loc) => Body.Atom(name, den, polarity, terms.map(apply), tpe, loc)
      case Body.Guard(exp, loc) => Body.Guard(apply(exp), loc)
    }

    /**
      * Applies the substitution to the constraint parameter `cparam0`.
      */
    def apply(cparam0: ConstraintParam): ConstraintParam = cparam0 match {
      case ConstraintParam.HeadParam(sym, tpe, loc) => ConstraintParam.HeadParam(apply(sym), tpe, loc)
      case ConstraintParam.RuleParam(sym, tpe, loc) => ConstraintParam.RuleParam(apply(sym), tpe, loc)
    }

    /**
      * Applies the substitution to the formal parameter `fparam0`.
      */
    def apply(fparam0: FormalParam): FormalParam = fparam0 match {
      case FormalParam(sym, mod, tpe, loc) => FormalParam(apply(sym), mod, tpe, loc)
    }

    /**
      * Returns the left-biased composition of `this` substitution with `that` substitution.
      */
    def ++(that: Substitution): Substitution = {
      if (this.isEmpty) {
        that
      } else if (that.isEmpty) {
        this
      } else {
        Substitution(
          this.m ++ that.m.filter(kv => !this.m.contains(kv._1))
        )
      }
    }

    /**
      * Returns the composition of `this` substitution with `that` substitution.
      */
    def @@(that: Substitution): Substitution = {
      if (this.isEmpty) {
        that
      } else if (that.isEmpty) {
        this
      } else {
        val newMap = that.m.foldLeft(Map.empty[Symbol.VarSym, Expression]) {
          case (macc, (x, t)) => macc.updated(x, this.apply(t))
        }
        Substitution(newMap) ++ this
      }
    }

  }


}
