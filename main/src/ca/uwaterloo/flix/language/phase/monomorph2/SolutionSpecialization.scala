/*
 * Copyright 2026 Simon Lykke Andersen
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

package ca.uwaterloo.flix.language.phase.monomorph2

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{MonoAst, TypedAst}

/**
  * Solution-driven specialization: uses the solver's solution to specialize all defs, enums, and
  * structs in a single parallel pass.
  *
  * `run` and [[SolutionLowering]] have different responsibilities: `run` builds the tables mapping
  * each original polymorphic symbol/instantiation to its fresh specialized symbol, and
  * [[SolutionLowering]] does the actual `TypedAst`-to-`MonoAst` transformation, using those tables
  * to resolve which fresh symbol each call/tag/struct-access site should point to.
  */
object SolutionSpecialization {

  /** Specializes `root` per `solution`, the constraint solver's output. */
  def run(root: TypedAst.Root, solution: Solution)(implicit flix: Flix): MonoAst.Root = ???
}
