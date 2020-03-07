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
import ca.uwaterloo.flix.language.ast.{BinaryOperator, SourceLocation, Symbol, TypedAst}
import ca.uwaterloo.flix.language.debug.FormatExpression
import ca.uwaterloo.flix.language.errors.LinterError
import ca.uwaterloo.flix.util.{ParOps, Result, Validation}
import ca.uwaterloo.flix.util.Validation._

object Linter extends Phase[TypedAst.Root, TypedAst.Root] {

  def run(root: Root)(implicit flix: Flix): Validation[Root, LinterError] = flix.phase("Linter") {
    // Finds all lints the in the ast.
    val lints = lintsOf(root)

    // Every definition in the program.
    val defs = root.defs.values.toList

    // Visit every definition in parallel.
    val results = ParOps.parMap(visitDef(_, lints), defs)

    // Collect all the results.
    results.flatten match {
      case Nil => root.toSuccess
      case xs => Failure(LazyList.from(xs))
    }
  }

  private def visitDef(defn: Def, lints: List[Lint]): List[LinterError] =
    LinterError.TrivialExpression(SourceLocation.Unknown) :: Nil

  private def lintsOf(root: Root): List[Lint] = root.defs.foldLeft(Nil: List[Lint]) {
    case (acc, (sym, defn)) => if (defn.ann.isLint) Lint(defn.exp) :: acc else acc
  }

  case class Lint(exp: Expression)

}
