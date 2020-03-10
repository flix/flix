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
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.errors.LinterError
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{ParOps, Validation}

import scala.annotation.tailrec

/**
  * The Linter phase checks for any applicable lints within the ast.
  */
object Linter extends Phase[TypedAst.Root, TypedAst.Root] {

  /**
    * Checks the given AST `root` for lints.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, LinterError] = flix.phase("Linter") {
    // Find all the lints in the ast.
    val lints = lintsOf(root)

    // Find all the targets the lints should be applied to.
    val targets = targetsOf(root)

    println(s"I found: ${lints.length} lints to match against ${targets.length} defs.") // TODO: Debug

    // Searches for applicable lints in the targets.
    val results = ParOps.parMap(visitDef(_, lints), targets)

    // Report linter errors (if any).
    results.flatten match {
      case Nil => root.toSuccess
      case xs => Failure(LazyList.from(xs))
    }
  }

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

      case Expression.Apply(exp1, exp2, _, _, _) => visitExp(exp1, lint0) ::: visitExp(exp2, lint0)

      case Expression.Unary(_, exp, _, _, _) => visitExp(exp, lint0)

      case Expression.Binary(_, exp1, exp2, _, _, _) => visitExp(exp1, lint0) ::: visitExp(exp2, lint0)

      case Expression.Let(_, exp1, exp2, _, _, _) => visitExp(exp1, lint0) ::: visitExp(exp2, lint0)

      case Expression.LetRec(_, exp1, exp2, _, _, _) => visitExp(exp1, lint0) ::: visitExp(exp2, lint0)

      case Expression.IfThenElse(exp1, exp2, exp3, _, _, _) => visitExp(exp1, lint0) ::: visitExp(exp2, lint0) ::: visitExp(exp3, lint0)

      case Expression.Stm(exp1, exp2, _, _, _) => visitExp(exp1, lint0) ::: visitExp(exp2, lint0)

      case Expression.Match(exp, rules, _, _, _) =>
        rules.foldLeft(visitExp(exp, lint0)) {
          case (acc, MatchRule(_, guard, body)) => visitExp(guard, lint0) ::: visitExp(body, lint0) ::: acc
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

      case Expression.ArrayLength(exp, _, _, _) => visitExp(exp, lint0)

      case Expression.ArrayStore(exp1, exp2, exp3, _, _, _) => visitExp(exp1, lint0) ::: visitExp(exp2, lint0) ::: visitExp(exp3, lint0)

      case Expression.ArraySlice(exp1, exp2, exp3, _, _, _) => visitExp(exp1, lint0) ::: visitExp(exp2, lint0) ::: visitExp(exp3, lint0)

      case Expression.VectorLit(exps, _, _, _) => exps.flatMap(visitExp(_, lint0))

      case Expression.VectorNew(exp, _, _, _, _) => visitExp(exp, lint0)

      case Expression.VectorLoad(exp, _, _, _, _) => visitExp(exp, lint0)

      case Expression.VectorStore(exp1, _, exp2, _, _, _) => visitExp(exp1, lint0) ::: visitExp(exp2, lint0)

      case Expression.VectorLength(exp, _, _, _) => visitExp(exp, lint0)

      case Expression.VectorSlice(exp, _, _, _, _, _) => visitExp(exp, lint0)

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

      case Expression.ProcessSpawn(exp, _, _, _) => visitExp(exp, lint0)

      case Expression.ProcessPanic(_, _, _, _) => Nil

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
  private def tryLint(exp0: Expression, lint0: Lint)(implicit flix: Flix): List[LinterError] =
    unifyExp(lint0.pattern, exp0) match {
      case None => Nil
      case Some(subst) => LinterError.Lint(lint0.sym, subst(lint0.replacement), exp0.loc) :: Nil
    }

  /**
    * Optionally returns a substitution that makes `lint0` and `exp0` equal.
    *
    * NB: Unification is left-biased: variables in the expression are not unified against the lint.
    */
  // TODO: Introduce meta variables argument and also in the lint class.
  private def unifyExp(lint0: Expression, exp0: Expression)(implicit flix: Flix): Option[Substitution] = (lint0, exp0) match {
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
      val expTyp = exp0.tpe

      if (exp0.isInstanceOf[Expression.Var]) {
        return None // TODO
      }

      if (canUnify(varTyp, expTyp))
        Some(Substitution.singleton(sym, exp0))
      else
        None

    // NB: Unification is left-biased so there is no case for a variable on the rhs.

    case (Expression.Def(sym1, _, _), Expression.Def(sym2, _, _)) if sym1 == sym2 => Some(Substitution.empty)

    case (Expression.Hole(sym1, _, _, _), Expression.Hole(sym2, _, _, _)) if sym1 == sym2 => Some(Substitution.empty)

    //      case class Lambda(fparam: TypedAst.FormalParam, exp: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Expression {  // TODO
    //        def eff: Type = Type.Pure
    //      }

    case (Expression.Apply(exp11, exp12, _, _, _), Expression.Apply(exp21, exp22, _, _, _)) =>
      for {
        s1 <- unifyExp(exp11, exp21)
        s2 <- unifyExp(s1(exp12), s1(exp22))
      } yield s2 @@ s1

    case (Expression.Unary(op1, exp1, _, _, _), Expression.Unary(op2, exp2, _, _, _)) if op1 == op2 =>
      unifyExp(exp1, exp2)

    case (Expression.Binary(op1, exp11, exp12, _, _, _), Expression.Binary(op2, exp21, exp22, _, _, _)) if op1 == op2 =>
      for {
        s1 <- unifyExp(exp11, exp21)
        s2 <- unifyExp(s1(exp12), s1(exp22))
      } yield s2 @@ s1

    //      case class Let(sym: Symbol.VarSym, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression  // TODO
    //
    //      case class LetRec(sym: Symbol.VarSym, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //

    case (Expression.IfThenElse(exp11, exp12, exp13, _, _, _), Expression.IfThenElse(exp21, exp22, exp23, _, _, _)) =>
      for {
        s1 <- unifyExp(exp11, exp21)
        s2 <- unifyExp(s1(exp12), s1(exp22))
        s3 <- unifyExp(s2(exp13), s2(exp23)) // TODO: Also apply s1?
      } yield s3 @@ s2 @@ s1

    case (Expression.Stm(exp11, exp12, _, _, _), Expression.Stm(exp21, exp22, _, _, _)) =>
      for {
        s1 <- unifyExp(exp11, exp21)
        s2 <- unifyExp(s1(exp21), s1(exp22))
      } yield s2 @@ s1

    //      case class Match(exp: TypedAst.Expression, rules: List[TypedAst.MatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression  // TODO

    case (Expression.Tag(sym1, tag1, exp1, _, _, _), Expression.Tag(sym2, tag2, exp2, _, _, _)) if sym1 == sym2 && tag1 == tag2 =>
      unifyExp(exp1, exp2)

    //      case class Tuple(elms: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression  // TODO

    case (Expression.RecordEmpty(_, _), Expression.RecordEmpty(_, _)) => Some(Substitution.empty)

    case (Expression.RecordSelect(exp1, _, _, _, _), Expression.RecordSelect(exp2, _, _, _, _)) =>
      unifyExp(exp1, exp2)

    //      case class RecordExtend(label: String, value: TypedAst.Expression, rest: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class RecordRestrict(label: String, rest: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class ArrayLit(elms: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class ArrayNew(elm: TypedAst.Expression, len: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class ArrayLoad(base: TypedAst.Expression, index: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class ArrayLength(base: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class ArrayStore(base: TypedAst.Expression, index: TypedAst.Expression, elm: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class ArraySlice(base: TypedAst.Expression, beginIndex: TypedAst.Expression, endIndex: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class VectorLit(elms: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class VectorNew(elm: TypedAst.Expression, len: Int, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class VectorLoad(base: TypedAst.Expression, index: Int, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class VectorStore(base: TypedAst.Expression, index: Int, elm: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class VectorLength(base: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class VectorSlice(base: TypedAst.Expression, startIndex: Int, endIndex: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class Ref(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class Deref(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class Assign(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class Existential(fparam: TypedAst.FormalParam, exp: TypedAst.Expression, loc: SourceLocation) extends TypedAst.Expression { // TODO
    //        def tpe: Type = Type.Bool
    //
    //        def eff: Type = Type.Pure
    //      }
    //
    //      case class Universal(fparam: TypedAst.FormalParam, exp: TypedAst.Expression, loc: SourceLocation) extends TypedAst.Expression { // TODO
    //        def tpe: Type = Type.Bool
    //
    //        def eff: Type = Type.Pure
    //      }
    //
    //      case class Ascribe(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class Cast(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class TryCatch(exp: TypedAst.Expression, rules: List[TypedAst.CatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    case (Expression.InvokeConstructor(constructor1, exps1, _, _, _), Expression.InvokeConstructor(constructor2, exps2, _, _, _)) =>
      unifyExps(exps1, exps2)

    case (Expression.InvokeMethod(method1, exp1, exps1, _, _, _), Expression.InvokeMethod(method2, exp2, exps2, _, _, _)) if method1 == method2 =>
      for {
        s1 <- unifyExp(exp1, exp2)
        s2 <- unifyExps(exps1.map(s1.apply), exps2.map(s1.apply))
      } yield s2 @@ s1

    case (Expression.InvokeStaticMethod(method1, exps1, _, _, _), Expression.InvokeStaticMethod(method2, exps2, _, _, _)) if method1 == method2 =>
      unifyExps(exps1, exps2)

    case (Expression.GetField(field1, exp1, _, _, _), Expression.GetField(field2, exp2, _, _, _)) if field1 == field2 =>
      unifyExp(exp1, exp2)

    case (Expression.PutField(field1, exp11, exp12, _, _, _), Expression.PutField(field2, exp21, exp22, _, _, _)) if field1 == field2 =>
      for {
        s1 <- unifyExp(exp11, exp21)
        s2 <- unifyExp(s1(exp12), s1(exp22))
      } yield s2 @@ s1

    case (Expression.GetStaticField(field1, _, _, _), Expression.GetStaticField(field2, _, _, _)) if field1 == field2 => Some(Substitution.empty)

    case (Expression.PutStaticField(field1, exp1, _, _, _), Expression.PutStaticField(field2, exp2, _, _, _)) if field1 == field2 =>
      unifyExp(exp1, exp2)

    case (Expression.NewChannel(exp1, _, _, _), Expression.NewChannel(exp2, _, _, _)) =>
      unifyExp(exp1, exp2)

    case (Expression.GetChannel(exp1, _, _, _), Expression.GetChannel(exp2, _, _, _)) =>
      unifyExp(exp1, exp2)

    case (Expression.PutChannel(exp11, exp12, _, _, _), Expression.PutChannel(exp21, exp22, _, _, _)) =>
      for {
        s1 <- unifyExp(exp11, exp21)
        s2 <- unifyExp(s1(exp12), s1(exp22))
      } yield s2 @@ s1

    //      case class SelectChannel(rules: List[TypedAst.SelectChannelRule], default: Option[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //

    case (Expression.ProcessSpawn(exp1, _, _, _), Expression.ProcessSpawn(exp2, _, _, _)) =>
      unifyExp(exp1, exp2)

    case (Expression.ProcessPanic(msg1, _, _, _), Expression.ProcessPanic(msg2, _, _, _)) if msg1 == msg2 => Some(Substitution.empty)

    //      case class FixpointConstraintSet(cs: List[TypedAst.Constraint], tpe: Type, loc: SourceLocation) extends TypedAst.Expression { // TODO
    //        def eff: Type = Type.Pure
    //      }
    //
    case (Expression.FixpointCompose(exp11, exp12, _, _, _), Expression.FixpointCompose(exp21, exp22, _, _, _)) =>
      for {
        s1 <- unifyExp(exp11, exp21)
        s2 <- unifyExp(s1(exp21), s1(exp22))
      } yield s2 @@ s1

    case (Expression.FixpointSolve(exp1, _, _, _, _), Expression.FixpointSolve(exp2, _, _, _, _)) =>
      unifyExp(exp1, exp2)

    case (Expression.FixpointProject(sym1, exp1, _, _, _), Expression.FixpointProject(sym2, exp2, _, _, _)) if sym1 == sym2 =>
      unifyExp(exp1, exp2)



    //      case class FixpointEntails(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class FixpointFold(sym: Symbol.PredSym, exp1: TypedAst.Expression, exp2: TypedAst.Expression, exp3: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO

    case _ => None
  }

  /**
    * Optionally returns a substitution that makes `l1` and `l2` equal.
    */
  private def unifyExps(l1: List[Expression], l2: List[Expression])(implicit flix: Flix): Option[Substitution] = (l1, l2) match {
    case (Nil, Nil) => Some(Substitution.empty)
    case (Nil, _) => None
    case (_, Nil) => None
    case (x :: xs, y :: ys) =>
      for {
        s1 <- unifyExp(x, y)
        s2 <- unifyExps(xs.map(s1.apply), ys.map(s1.apply))
      } yield s2 @@ s1
  }

  // TODO: DOC
  private def unifyConstraint(c1: Constraint, c2: Constraint)(implicit flix: Flix): Option[Substitution] = (c1, c2) match {
    case (Constraint(_, head1, body1, _), Constraint(_, head2, body2, _)) =>
      ??? // TODO
  }



  /**
    * Returns all lints in the given AST `root`.
    */
  private def lintsOf(root: Root): List[Lint] = root.defs.foldLeft(Nil: List[Lint]) {
    case (acc, (sym, defn)) if defn.ann.isLint => lintOf(sym, defn.exp) match {
      case None => acc
      case Some(l) => l :: acc
    }
    case (acc, (sym, defn)) => acc
  }

  /**
    * TODO: DOC
    */
  @tailrec
  private def popForall(exp0: Expression): Expression = exp0 match {
    case Expression.Universal(fparam, exp, loc) => popForall(exp)
    case _ => exp0
  }

  /**
    * Returns all non-lints definitions in the given AST `root`.
    */
  private def targetsOf(root: Root): List[Def] = root.defs.foldLeft(Nil: List[Def]) {
    case (acc, (sym, defn)) => if (!defn.ann.isLint && !defn.ann.isTest) defn :: acc else acc // TODO: Criteria?
  }

  /**
    * TODO: DOC
    */
  private def lintOf(sym: Symbol.DefnSym, exp0: Expression): Option[Lint] = {
    val ctxEquiv = Symbol.mkDefnSym("===")
    val implies = Symbol.mkDefnSym("implies")
    val popped = popForall(exp0)

    popped match {
      case Expression.Apply(Expression.Apply(Expression.Def(someSym, _, _), left, _, _, _), right, _, _, _) =>
        if (someSym == ctxEquiv) {
          Some(Lint(sym, left, right))
        } else if (someSym == implies) {
          None // TODO
        } else {
          println(s"Malformed lint: $sym ") // TODO
          None
        }
      case _ =>
        println(s"Malformed lint: $sym ") // TODO
        None
    }
  }

  /**
    * TODO: DOC
    */
  case class Lint(sym: Symbol.DefnSym, pattern: Expression, replacement: Expression)

  // TODO: DOC
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

  // TODO: DOC
  case class Substitution(m: Map[Symbol.VarSym, Expression]) {

    /**
      * Returns `true` if `this` is the empty substitution.
      */
    val isEmpty: Boolean = m.isEmpty

    /**
      * Applies the substitution to the expression `exp0`.
      */
    def apply(exp0: Expression): Expression = exp0 match {
      case Expression.Unit(_) => exp0

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

      case Expression.Apply(exp1, exp2, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        Expression.Apply(e1, e2, tpe, eff, loc)

      case Expression.Unary(op, exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.Unary(op, e, tpe, eff, loc)

      case Expression.Binary(op, exp1, exp2, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        Expression.Binary(op, e1, e2, tpe, eff, loc)

      //        case class Let(sym: Symbol.VarSym, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression  TODO
      //
      //        case class LetRec(sym: Symbol.VarSym, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression  TODO

      case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        val e3 = apply(exp3)
        Expression.IfThenElse(e1, e2, e3, tpe, eff, loc)

      case Expression.Stm(exp1, exp2, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        Expression.Stm(e1, e2, tpe, eff, loc)

      //        case class Match(exp: TypedAst.Expression, rules: List[TypedAst.MatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression  TODO

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

      case Expression.ArrayLength(exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.ArrayLength(e, tpe, eff, loc)

      case Expression.ArrayStore(exp1, exp2, exp3, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        val e3 = apply(exp3)
        Expression.ArrayStore(e1, e2, e3, tpe, eff, loc)

      case Expression.ArraySlice(exp1, exp2, exp3, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        val e3 = apply(exp3)
        Expression.ArraySlice(e1, e2, e3, tpe, eff, loc)

      case Expression.VectorLit(elms, tpe, eff, loc) =>
        val es = elms.map(apply)
        Expression.VectorLit(es, tpe, eff, loc)

      case Expression.VectorNew(exp, len, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.VectorNew(e, len, tpe, eff, loc)

      case Expression.VectorLoad(exp, index, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.VectorLoad(e, index, tpe, eff, loc)

      case Expression.VectorStore(exp1, index, exp2, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        Expression.VectorStore(e1, index, e2, tpe, eff, loc)

      case Expression.VectorLength(exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.VectorLength(e, tpe, eff, loc)

      case Expression.VectorSlice(exp, begin, end, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.VectorSlice(e, begin, end, tpe, eff, loc)

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

      //        case class Existential(fparam: TypedAst.FormalParam, exp: TypedAst.Expression, loc: SourceLocation) extends TypedAst.Expression {  TODO
      //          def tpe: Type = Type.Bool
      //
      //          def eff: Type = Type.Pure
      //        }
      //
      //        case class Universal(fparam: TypedAst.FormalParam, exp: TypedAst.Expression, loc: SourceLocation) extends TypedAst.Expression {  TODO
      //          def tpe: Type = Type.Bool
      //
      //          def eff: Type = Type.Pure
      //        }

      case Expression.Ascribe(exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.Ascribe(e, tpe, eff, loc)

      case Expression.Cast(exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.Cast(e, tpe, eff, loc)


      //        case class TryCatch(exp: TypedAst.Expression, rules: List[TypedAst.CatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression  TODO
      //

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

      //        case class SelectChannel(rules: List[TypedAst.SelectChannelRule], default: Option[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression  TODO
      //

      case Expression.ProcessSpawn(exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.ProcessSpawn(e, tpe, eff, loc)

      case Expression.ProcessPanic(msg, tpe, eff, loc) => exp0

      //        case class FixpointConstraintSet(cs: List[TypedAst.Constraint], tpe: Type, loc: SourceLocation) extends TypedAst.Expression {  TODO
      //          def eff: Type = Type.Pure
      //        }
      //

      case Expression.FixpointCompose(exp1, exp2, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        Expression.FixpointCompose(e1, e2, tpe, eff, loc)

      case Expression.FixpointSolve(exp, stf, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.FixpointSolve(e, stf, tpe, eff, loc)

      case Expression.FixpointProject(sym, exp, tpe, eff, loc) =>
        val e = apply(exp)
        Expression.FixpointProject(sym, e, tpe, eff, loc)

      case Expression.FixpointEntails(exp1, exp2, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        Expression.FixpointEntails(e1, e2, tpe, eff, loc)

      case Expression.FixpointFold(sym, exp1, exp2, exp3, tpe, eff, loc) =>
        val e1 = apply(exp1)
        val e2 = apply(exp2)
        val e3 = apply(exp3)
        Expression.FixpointFold(sym, e1, e2, e3, tpe, eff, loc)

      case _ => exp0 // TODO
    }

    /**
      * Applies the substitution to the formal parameter `fparam0`.
      */
    def apply(fparam0: FormalParam): FormalParam = fparam0 match {
      case FormalParam(sym, mod, tpe, loc) => m.get(sym) match {
        case None => fparam0
        case Some(otherSym) => FormalParam(???, mod, tpe, loc) // TODO: The subst. need to contain more than just expressions?
      }
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

  /**
    * TODO: DOC
    */
  // TODO: Move somewhere better?
  private def canUnify(tpe1: Type, tpe2: Type)(implicit flix: Flix): Boolean = Unification.unifyTypes(tpe1, tpe2) match {
    case Ok(_) => true
    case Err(_) => false
  }

}
