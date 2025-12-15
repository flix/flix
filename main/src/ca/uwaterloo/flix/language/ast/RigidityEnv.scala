/*
 * Copyright 2022 Matthew Lutze
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
package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.shared.Scope

import scala.collection.immutable.SortedSet


object RigidityEnv {
  /**
    * The empty rigidity environment.
    */
  val empty: RigidityEnv = RigidityEnv(SortedSet.empty)

  /**
    * Returns the rigidity environment where only the given variables are marked rigid.
    */
  def ofRigidVars(tvars: Iterable[Symbol.KindedTypeVarSym]): RigidityEnv = RigidityEnv(tvars.to(SortedSet))
}

