/*
 * Copyright 2018 Magnus Madsen
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

/**
  * Represents the computational effect of an expression.
  */
sealed trait Eff {

  def isPure: Boolean = this match {
    case Eff.Pure => true
    case Eff.Impure => false
  }

}

object Eff {

  /**
    * Represents the empty effect.
    */
  val Empty: Eff = Impure

  case object Pure extends Eff

  case object Impure extends Eff

}
