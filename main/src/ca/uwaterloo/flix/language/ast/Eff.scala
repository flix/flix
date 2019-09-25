/*
 * Copyright 2019 Magnus Madsen
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

import ca.uwaterloo.flix.api.Flix

/**
  * Represents the computational effect of an expression.
  */
sealed trait Eff

object Eff {

  /**
    * Returns a fresh effect variable.
    */
  def freshEffVar()(implicit flix: Flix): Eff.Var = Eff.Var(flix.genSym.freshId())

  /**
    * Represents an effect variable.
    */
  case class Var(id: Int) extends Eff {
    /**
      * Returns `true` if `this` type variable is equal to `o`.
      */
    override def equals(o: scala.Any): Boolean = o match {
      case that: Var => this.id == that.id
      case _ => false
    }

    /**
      * Returns the hash code of `this` type variable.
      */
    override def hashCode(): Int = id
  }

  /**
    * Represents a pure effect.
    */
  case object Pure extends Eff

  /**
    * Represents an impure effect.
    */
  case object Impure extends Eff

}
