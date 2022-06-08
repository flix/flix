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

import scala.collection.immutable.SortedSet

/**
  * Environment tracking the rigidity of type variables.
  *
  * `s` holds the set of rigid variable symbols.
  * All variables not in `s` are considered flexible.
  */
case class RigidityEnv(s: SortedSet[Symbol.KindedTypeVarSym]) {

  /**
    * Returns the rigidity of the given `sym` according to this environment.
    */
  def get(sym: Symbol.KindedTypeVarSym): Rigidity = {
    if (s.contains(sym)) {
      Rigidity.Rigid
    } else {
      Rigidity.Flexible
    }
  }

  /**
    * Marks the given `sym` as rigid in this environment.
    */
  def withRigid(sym: Symbol.KindedTypeVarSym): RigidityEnv = RigidityEnv(s + sym)

  /**
    * Returns true iff the given `sym` is rigid according to this environment.
    */
  def isRigid(sym: Symbol.KindedTypeVarSym): Boolean = get(sym) == Rigidity.Rigid

  /**
    * Returns true iff the given `sym` is flexible according to this environment.
    */
  def isFlexible(sym: Symbol.KindedTypeVarSym): Boolean = get(sym) == Rigidity.Flexible
}

object RigidityEnv {
  /**
    * The empty rigidity environment.
    */
  val empty: RigidityEnv = RigidityEnv(SortedSet.empty)
}
