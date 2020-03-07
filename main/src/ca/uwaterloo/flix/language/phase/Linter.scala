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
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{SourceLocation, TypedAst}
import ca.uwaterloo.flix.language.errors.LinterError
import ca.uwaterloo.flix.util.{ParOps, Validation}
import ca.uwaterloo.flix.util.Validation._

object Linter extends Phase[TypedAst.Root, TypedAst.Root] {

  def run(root: Root)(implicit flix: Flix): Validation[Root, LinterError] = flix.phase("Linter") {
    // Compute all lints in the AST root.
    val lints = lintsOf(root)

    // Compute a list of all non-lint definitions in the program.
    val defs = nonLintsOf(root)

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
  private def visitDef(defn: Def, lints: List[Lint]): List[LinterError] =
    lints.flatMap(visitExp(defn.exp, _))

  /**
    * Computes whether the given lint `l0` is applicable to the given expression `exp0`.
    */
  private def visitExp(exp0: Expression, lint: Lint): Option[LinterError] = {
    if (lint.sym.name == "leftAdditionByZero") // TODO
      unify(exp0, lint.exp) match {
        case None => None
        case Some(_) => Some(LinterError.Simplify("hello", SourceLocation.Unknown))
      }
    else
      None
  }

  private def unify(exp1: Expression, exp2: Expression): Option[Subst] = (exp1, exp2) match {
    case (Expression.Unit(_), Expression.Unit(_)) => Some(Subst.empty)

    case (Expression.True(_), Expression.True(_)) => Some(Subst.empty)

    case (Expression.False(_), Expression.False(_)) => Some(Subst.empty)

    case (Expression.Char(lit1, _), Expression.Char(lit2, _)) if lit1 == lit2 => Some(Subst.empty)

    case (Expression.Float32(lit1, _), Expression.Float32(lit2, _)) if lit1 == lit2 => Some(Subst.empty)

    case (Expression.Float64(lit1, _), Expression.Float64(lit2, _)) if lit1 == lit2 => Some(Subst.empty)

    case (Expression.Int8(lit1, _), Expression.Int8(lit2, _)) if lit1 == lit2 => Some(Subst.empty)

    case (Expression.Int16(lit1, _), Expression.Int16(lit2, _)) if lit1 == lit2 => Some(Subst.empty)

    case (Expression.Int32(lit1, _), Expression.Int32(lit2, _)) if lit1 == lit2 => Some(Subst.empty)

    case (Expression.Unary(op1, exp1, _, _, _), Expression.Unary(op2, exp2, _, _, _)) if op1 == op2 =>
      unify(exp1, exp2)

    case (Expression.Binary(op1, exp11, exp12, _, _, _), Expression.Binary(op2, exp21, exp22, _, _, _)) if op1 == op2 =>
      for {
        s1 <- unify(exp11, exp21)
        s2 <- unify(s1(exp12), s1(exp22))
      } yield s2 @@ s1

    case _ => None
  }

  /**
    * Returns all lints in the given AST `root`.
    */
  private def lintsOf(root: Root): List[Lint] = root.defs.foldLeft(Nil: List[Lint]) {
    case (acc, (sym, defn)) if (defn.ann.isLint) => defn.exp match {
      case Expression.Universal(_, exp, _) => Lint(defn.sym, exp) :: acc // TODO
      case _ => Lint(defn.sym, defn.exp) :: acc
    }
    case (acc, (sym, defn)) => acc
  }

  /**
    * Returns all non-lints definitions in the given AST `root`.
    */
  private def nonLintsOf(root: Root): List[Def] = root.defs.foldLeft(Nil: List[Def]) {
    case (acc, (sym, defn)) => if (!defn.ann.isLint) defn :: acc else acc
  }

  case class Lint(sym: Symbol.DefnSym, exp: Expression)

  object Subst {
    val empty: Subst = Subst()
  }

  case class Subst() {

    def apply(exp0: Expression): Expression = exp0 match {
      case Expression.Unit(_) => exp0

      case Expression.True(_) => exp0

    }

    def @@(that: Subst): Subst = ??? // TODO

  }

}
