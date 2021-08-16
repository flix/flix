/*
 * Copyright 2021 Matthew Lutze
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
  * Indicates whether a variable is scoped.
  * A scoped value must not escape its scope.
  */
sealed trait Scopedness { // MATT remove later probably
  def toType: Type = this match {
    case Scopedness.Scoped => Type.Scoped
    case Scopedness.Unscoped => Type.Unscoped
    case Scopedness.Var(id) => Type.Var(id, Kind.Bool)
  }
}

object Scopedness {
  case object Scoped extends Scopedness

  case object Unscoped extends Scopedness

  case class Var(id: Int) extends Scopedness

  def freshVar()(implicit flix: Flix): Var = Var(flix.genSym.freshId())
}
