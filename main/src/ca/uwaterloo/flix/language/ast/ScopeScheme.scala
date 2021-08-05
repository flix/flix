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

import ca.uwaterloo.flix.util.InternalCompilerException

// MATT docs
sealed trait ScopeScheme {
  def <=(other: ScopeScheme): Boolean = (this, other) match {
    case (ScopeScheme.Unit, ScopeScheme.Unit) => true
    case (ScopeScheme.Arrow(paramSco1, paramSch1, retSch1), ScopeScheme.Arrow(paramSco2, paramSch2, retSch2)) =>
      // NB: Contravariant on the left
      paramSco2 <= paramSco1 && paramSch2 <= paramSch1 && retSch1 <= retSch2
    case _ => throw InternalCompilerException("Incompatible ScopeSchemes")
  }

  def max(other: ScopeScheme): ScopeScheme = (this, other) match {
    case (ScopeScheme.Unit, ScopeScheme.Unit) => ScopeScheme.Unit
    case (ScopeScheme.Arrow(paramSco1, paramSch1, retSch1), ScopeScheme.Arrow(paramSco2, paramSch2, retSch2)) =>
      // NB: Contravariant on the left
      ScopeScheme.Arrow(paramSco1 min paramSco2, paramSch1 min paramSch2, retSch1 max retSch2)
    case _ => throw InternalCompilerException("Incompatible ScopeSchemes")
  }

  def min(other: ScopeScheme): ScopeScheme = (this, other) match {
    case (ScopeScheme.Unit, ScopeScheme.Unit) => ScopeScheme.Unit
    case (ScopeScheme.Arrow(paramSco1, paramSch1, retSch1), ScopeScheme.Arrow(paramSco2, paramSch2, retSch2)) =>
      // NB: Contravariant on the left
      ScopeScheme.Arrow(paramSco1 max paramSco2, paramSch1 max paramSch2, retSch1 min retSch2)
    case _ => throw InternalCompilerException("Incompatible ScopeSchemes")
  }

}

object ScopeScheme {
  case object Unit extends ScopeScheme

  case class Arrow(paramSco: Scopedness, paramSch: ScopeScheme, retSch: ScopeScheme) extends ScopeScheme
}
