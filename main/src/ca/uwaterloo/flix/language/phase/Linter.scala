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
      Some(LinterError.Simplify("hello", SourceLocation.Unknown))
    else
      None
  }

  private def unify(exp1: Expression, exp2: Expression): Option[Subst] = ???

  /**
    * Returns all lints in the given AST `root`.
    */
  private def lintsOf(root: Root): List[Lint] = root.defs.foldLeft(Nil: List[Lint]) {
    case (acc, (sym, defn)) => if (defn.ann.isLint) Lint(defn.sym, defn.exp) :: acc else acc
  }

  /**
    * Returns all non-lints definitions in the given AST `root`.
    */
  private def nonLintsOf(root: Root): List[Def] = root.defs.foldLeft(Nil: List[Def]) {
    case (acc, (sym, defn)) => if (!defn.ann.isLint) defn :: acc else acc
  }

  case class Lint(sym: Symbol.DefnSym, exp: Expression)

  case class Subst()

}
