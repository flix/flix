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
import ca.uwaterloo.flix.language.ast.{BinaryOperator, Symbol, TypedAst}
import ca.uwaterloo.flix.language.debug.FormatExpression
import ca.uwaterloo.flix.language.errors.LinterError
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

object Linter extends Phase[TypedAst.Root, TypedAst.Root] {

  def run(root: TypedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, LinterError] = flix.phase("Linter") {
    // TODO: Implement the linter.
    root.toSuccess
  }
}
