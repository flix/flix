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
import ca.uwaterloo.flix.language.ast.Ast.{CheckedCastType, Denotation}
import ca.uwaterloo.flix.language.ast.Type.getFlixType
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.phase.inference.RestrictableChooseInference
import ca.uwaterloo.flix.language.phase.unification.InferMonad.{seqM, traverseM}
import ca.uwaterloo.flix.language.phase.unification.TypeMinimization.minimizeScheme
import ca.uwaterloo.flix.language.phase.unification.Unification._
import ca.uwaterloo.flix.language.phase.unification._
import ca.uwaterloo.flix.language.phase.util.PredefinedClasses
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.Validation.{ToFailure, ToSuccess, flatMapN, mapN, traverse, traverseValues}
import ca.uwaterloo.flix.util._
import ca.uwaterloo.flix.util.collection.ListMap

import java.io.PrintWriter
import scala.annotation.tailrec

object Typer {

  /**
    * Type checks the given AST root.
    */
  def run(root: KindedAst.Root, oldRoot: TypedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): Validation[TypedAst.Root, TypeError] = flix.phase("Typer") {
    flatMapN(TypeInference.run(root, oldRoot, changeSet)) {
      case (defSubsts, sigSubsts) =>
        TypeReconstruction.run(root, defSubsts, sigSubsts, oldRoot, changeSet)
    }
  }
}
