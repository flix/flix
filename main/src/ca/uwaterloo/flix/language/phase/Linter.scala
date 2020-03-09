/*
 *  Copyright 2019 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.errors.LinterError
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{ParOps, Validation}

import scala.annotation.tailrec

// TODO: DOC
object Linter extends Phase[TypedAst.Root, TypedAst.Root] {

  // TODO: DOC
  def run(root: Root)(implicit flix: Flix): Validation[Root, LinterError] = flix.phase("Linter") {
    // Compute all lints in the AST root.
    val lints = lintsOf(root)

    // Compute a list of all non-lint definitions in the program.
    val defs = nonLintsOf(root)

    // TODO: Debug
    println(s"I found: ${lints.length} lints to match against ${defs.length} defs.")

    // Searches for applicable lints.
    val results = ParOps.parMap(visitDef(_, lints), defs)

    // Check if there were any applicable lints.
    results.flatten match {
      case Nil => root.toSuccess
      case xs => Failure(LazyList.from(xs.take(100))) // TODO: Only returns the first 100 instances.
    }
  }

  /**
    * Searches for applicable lints in the given definition `defn0`.
    */
  private def visitDef(defn: Def, lints: List[Lint])(implicit flix: Flix): List[LinterError] =
    lints.flatMap(visitExp(defn.exp, _))


  // TODO: DOC
  private def visitExp(exp0: Expression, lint: Lint)(implicit flix: Flix): List[LinterError] = {
    tryLint(exp0, lint) ::: (exp0 match {

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

      case Expression.Lambda(_, exp, _, _) => visitExp(exp, lint)

      case Expression.Apply(exp1, exp2, _, _, _) => visitExp(exp1, lint) ::: visitExp(exp2, lint)

      case Expression.Unary(_, exp, _, _, _) => visitExp(exp, lint)

      case Expression.Binary(_, exp1, exp2, _, _, _) => visitExp(exp1, lint) ::: visitExp(exp2, lint)

      //      case class Let(sym: Symbol.VarSym, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
      //
      //      case class LetRec(sym: Symbol.VarSym, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
      //

      case Expression.IfThenElse(exp1, exp2, exp3, _, _, _) => visitExp(exp1, lint) ::: visitExp(exp2, lint) ::: visitExp(exp3, lint)

      case Expression.Stm(exp1, exp2, _, _, _) => visitExp(exp1, lint) ::: visitExp(exp2, lint)

      //      case class Match(exp: TypedAst.Expression, rules: List[TypedAst.MatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
      //

      case Expression.Tag(_, _, exp, _, _, _) => visitExp(exp, lint)

      case Expression.Tuple(exps, _, _, _) => exps.flatMap(visitExp(_, lint))

      case Expression.RecordEmpty(_, _) => Nil

      case Expression.RecordSelect(exp, _, _, _, _) => visitExp(exp, lint)

      case Expression.RecordExtend(_, exp1, exp2, _, _, _) => visitExp(exp1, lint) ::: visitExp(exp2, lint)

      case Expression.RecordRestrict(_, exp, _, _, _) => visitExp(exp, lint)

      case Expression.ArrayLit(exps, _, _, _) => exps.flatMap(visitExp(_, lint))

      case Expression.ArrayNew(exp1, exp2, _, _, _) => visitExp(exp1, lint) ::: visitExp(exp2, lint)

      case Expression.ArrayLoad(exp1, exp2, _, _, _) => visitExp(exp1, lint) ::: visitExp(exp2, lint)

      case Expression.ArrayLength(exp, _, _, _) => visitExp(exp, lint)



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

      case Expression.Ref(exp, _, _, _) => visitExp(exp, lint)

      //      case class Ref(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
      //
      //      case class Deref(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
      //
      //      case class Assign(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
      //
      //      case class Existential(fparam: TypedAst.FormalParam, exp: TypedAst.Expression, loc: SourceLocation) extends TypedAst.Expression { // TODO
      //    def tpe: Type = Type.Bool
      //
      //    def eff: Type = Type.Pure
      //    }
      //
      //      case class Universal(fparam: TypedAst.FormalParam, exp: TypedAst.Expression, loc: SourceLocation) extends TypedAst.Expression { // TODO
      //    def tpe: Type = Type.Bool
      //
      //    def eff: Type = Type.Pure
      //    }
      //
      //      case class Ascribe(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
      //
      //      case class Cast(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
      //
      //      case class TryCatch(exp: TypedAst.Expression, rules: List[TypedAst.CatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
      //
      //      case class InvokeConstructor(constructor: Constructor[_], args: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
      //
      //      case class InvokeMethod(method: Method, exp: TypedAst.Expression, args: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
      //
      //      case class InvokeStaticMethod(method: Method, args: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
      //
      //      case class GetField(field: Field, exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
      //
      //      case class PutField(field: Field, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
      //
      //      case class GetStaticField(field: Field, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
      //
      //      case class PutStaticField(field: Field, exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
      //
      //      case class NewChannel(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
      //
      //      case class GetChannel(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
      //
      //      case class PutChannel(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
      //
      //      case class SelectChannel(rules: List[TypedAst.SelectChannelRule], default: Option[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
      //
      //      case class ProcessSpawn(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
      //
      //      case class ProcessPanic(msg: String, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
      //
      //      case class FixpointConstraintSet(cs: List[TypedAst.Constraint], tpe: Type, loc: SourceLocation) extends TypedAst.Expression { // TODO
      //    def eff: Type = Type.Pure
      //    }
      //
      //      case class FixpointCompose(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
      //
      //      case class FixpointSolve(exp: TypedAst.Expression, stf: Ast.Stratification, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
      //
      //      case class FixpointProject(sym: Symbol.PredSym, exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
      //
      //      case class FixpointEntails(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
      //
      //      case class FixpointFold(sym: Symbol.PredSym, exp1: TypedAst.Expression, exp2: TypedAst.Expression, exp3: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO


      case _ => Nil
    })
  }

  // TODO: DOC
  private def tryLint(exp0: Expression, lint: Lint)(implicit flix: Flix): List[LinterError] = {
    unify(lint.leftExp, exp0) match {
      case None => Nil
      case Some(subst) => LinterError.Simplify(lint.sym, subst(lint.replacement), exp0.loc) :: Nil
    }
  }

  /**
    * Optionally return a substitution that makes `lint0` and `exp0` equal.
    *
    * NB: Unification is left-biased: variables in the expression are not unified against the lint.
    */
  private def unify(lint0: Expression, exp0: Expression)(implicit flix: Flix): Option[Substitution] = (lint0, exp0) match {
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

    //
    //      case class Int64(lit: scala.Long, loc: SourceLocation) extends TypedAst.Expression { // TODO
    //        def tpe: Type = Type.Int64
    //
    //        def eff: Type = Type.Pure
    //      }
    //
    //      case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends TypedAst.Expression {  // TODO
    //        def tpe: Type = Type.BigInt
    //
    //        def eff: Type = Type.Pure
    //      }
    //
    //      case class Str(lit: java.lang.String, loc: SourceLocation) extends TypedAst.Expression {  // TODO
    //        def tpe: Type = Type.Str
    //
    //        def eff: Type = Type.Pure
    //      }
    //
    //      case class Wild(tpe: Type, loc: SourceLocation) extends TypedAst.Expression {  // TODO
    //        def eff: Type = Type.Pure
    //      }

    case (Expression.Var(sym, varTyp, _), _) =>
      if (exp0.isInstanceOf[Expression.Var]) {
        return None // TODO
      }

      val expTyp = exp0.tpe
      //println(s"canUnify($expTyp, $varTyp)")
      if (canUnify(varTyp, expTyp))
        Some(Substitution.singleton(sym, exp0))
      else None

    // NB: Unification is left-biased so there is no case for a variable on the rhs.

    case (Expression.Def(sym1, _, _), Expression.Def(sym2, _, _)) if sym1 == sym2 => Some(Substitution.empty)

    //      case class Hole(sym: Symbol.HoleSym, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression  // TODO
    //
    //      case class Lambda(fparam: TypedAst.FormalParam, exp: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Expression {  // TODO
    //        def eff: Type = Type.Pure
    //      }

    case (Expression.Apply(exp11, exp12, _, _, _), Expression.Apply(exp21, exp22, _, _, _)) =>
      for {
        s1 <- unify(exp11, exp21)
        s2 <- unify(s1(exp12), s1(exp22))
      } yield s2 @@ s1

    case (Expression.Unary(op1, exp1, _, _, _), Expression.Unary(op2, exp2, _, _, _)) if op1 == op2 =>
      unify(exp1, exp2)

    case (Expression.Binary(op1, exp11, exp12, _, _, _), Expression.Binary(op2, exp21, exp22, _, _, _)) if op1 == op2 =>
      for {
        s1 <- unify(exp11, exp21)
        s2 <- unify(s1(exp12), s1(exp22))
      } yield s2 @@ s1

    //      case class Let(sym: Symbol.VarSym, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression  // TODO
    //
    //      case class LetRec(sym: Symbol.VarSym, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class IfThenElse(exp1: TypedAst.Expression, exp2: TypedAst.Expression, exp3: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression  // TODO
    //

    case (Expression.Stm(exp11, exp12, _, _, _), Expression.Stm(exp21, exp22, _, _, _)) =>
      for {
        s1 <- unify(exp11, exp21)
        s2 <- unify(s1(exp21), s1(exp22))
      } yield s2 @@ s1

    //      case class Stm(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression  // TODO
    //
    //      case class Match(exp: TypedAst.Expression, rules: List[TypedAst.MatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression  // TODO
    //
    //      case class Tag(sym: Symbol.EnumSym, tag: String, exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression  // TODO
    //
    //      case class Tuple(elms: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression  // TODO
    //
    //      case class RecordEmpty(tpe: Type, loc: SourceLocation) extends TypedAst.Expression {  // TODO
    //        def eff: Type = Type.Pure
    //      }
    //
    //      case class RecordSelect(exp: TypedAst.Expression, label: String, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
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
    //      case class InvokeConstructor(constructor: Constructor[_], args: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class InvokeMethod(method: Method, exp: TypedAst.Expression, args: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class InvokeStaticMethod(method: Method, args: List[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class GetField(field: Field, exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class PutField(field: Field, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class GetStaticField(field: Field, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class PutStaticField(field: Field, exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class NewChannel(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class GetChannel(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class PutChannel(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class SelectChannel(rules: List[TypedAst.SelectChannelRule], default: Option[TypedAst.Expression], tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class ProcessSpawn(exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class ProcessPanic(msg: String, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class FixpointConstraintSet(cs: List[TypedAst.Constraint], tpe: Type, loc: SourceLocation) extends TypedAst.Expression { // TODO
    //        def eff: Type = Type.Pure
    //      }
    //
    //      case class FixpointCompose(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class FixpointSolve(exp: TypedAst.Expression, stf: Ast.Stratification, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class FixpointProject(sym: Symbol.PredSym, exp: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class FixpointEntails(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO
    //
    //      case class FixpointFold(sym: Symbol.PredSym, exp1: TypedAst.Expression, exp2: TypedAst.Expression, exp3: TypedAst.Expression, tpe: Type, eff: Type, loc: SourceLocation) extends TypedAst.Expression // TODO

    case _ => None
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
  private def nonLintsOf(root: Root): List[Def] = root.defs.foldLeft(Nil: List[Def]) {
    case (acc, (sym, defn)) => if (!defn.ann.isLint) defn :: acc else acc
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
  case class Lint(sym: Symbol.DefnSym, leftExp: Expression, replacement: Expression)

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

      case _ => ???

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
