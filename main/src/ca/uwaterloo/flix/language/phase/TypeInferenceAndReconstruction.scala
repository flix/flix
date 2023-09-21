/*
 * Copyright 2023 Matthew Lutze
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.util.Validation.flatMapN
import ca.uwaterloo.flix.util._

object TypeInferenceAndReconstruction {

  /**
    * Type checks the given AST root.
    */
  def run(root: KindedAst.Root, oldRoot: TypedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): Validation[TypedAst.Root, TypeError] = flix.phase("Typer") {
    flatMapN(Typer.run(root, oldRoot, changeSet)) {
      case (defSubsts, sigSubsts) =>
        TypeReconstruction.run(root, defSubsts, sigSubsts, oldRoot, changeSet)
    }
  }
}
