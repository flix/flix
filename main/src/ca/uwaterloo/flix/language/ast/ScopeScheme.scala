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
    case (ScopeScheme.Implicit, ScopeScheme.Implicit) => true
    case (expl: ScopeScheme.Explicit, ScopeScheme.Implicit) => ScopeScheme.Explicit.lt(expl, ScopeScheme.mkUnscopedExplicit(expl))
    case (ScopeScheme.Implicit, expl: ScopeScheme.Explicit) => ScopeScheme.Explicit.lt(ScopeScheme.mkUnscopedExplicit(expl), expl)
    case (expl1: ScopeScheme.Explicit, expl2: ScopeScheme.Explicit) => ScopeScheme.Explicit.lt(expl1, expl2)
  }

  def min(other: ScopeScheme): ScopeScheme = (this, other) match {
    case (ScopeScheme.Implicit, ScopeScheme.Implicit) => ScopeScheme.Implicit
    case (expl: ScopeScheme.Explicit, ScopeScheme.Implicit) => ScopeScheme.Explicit.min(expl, ScopeScheme.mkUnscopedExplicit(expl))
    case (ScopeScheme.Implicit, expl: ScopeScheme.Explicit) => ScopeScheme.Explicit.min(ScopeScheme.mkUnscopedExplicit(expl), expl)
    case (expl1: ScopeScheme.Explicit, expl2: ScopeScheme.Explicit) => ScopeScheme.Explicit.min(expl1, expl2)
  }

  def max(other: ScopeScheme): ScopeScheme = (this, other) match {
    case (ScopeScheme.Implicit, ScopeScheme.Implicit) => ScopeScheme.Implicit
    case (expl: ScopeScheme.Explicit, ScopeScheme.Implicit) => ScopeScheme.Explicit.max(expl, ScopeScheme.mkUnscopedExplicit(expl))
    case (ScopeScheme.Implicit, expl: ScopeScheme.Explicit) => ScopeScheme.Explicit.max(ScopeScheme.mkUnscopedExplicit(expl), expl)
    case (expl1: ScopeScheme.Explicit, expl2: ScopeScheme.Explicit) => ScopeScheme.Explicit.max(expl1, expl2)
  }

  // MATT docs
  override def toString: String = this match {
    case ScopeScheme.Implicit => "???"
    case ScopeScheme.Unit => "."
    case ScopeScheme.Arrow(paramSco, paramSch, retSch) =>
      val scoPart = paramSco match {
        case Scopedness.Scoped => "scoped "
        case Scopedness.Unscoped => ""
      }
      val scScPart = paramSch match {
        case _: ScopeScheme.Arrow => s"($paramSch)"
        case ScopeScheme.Unit => s"$paramSch"
      }
      s"$scoPart$scScPart -> $retSch"
  }

}

object ScopeScheme {
  sealed trait Explicit extends ScopeScheme

  object Explicit {
    def lt(scSc1: Explicit, scSc2: Explicit): Boolean = (scSc1, scSc2) match {
      case (ScopeScheme.Unit, ScopeScheme.Unit) => true
      case (ScopeScheme.Arrow(paramSco1, paramSch1, retSch1), ScopeScheme.Arrow(paramSco2, paramSch2, retSch2)) =>
        // NB: Contravariant on the left
        paramSco2 <= paramSco1 && paramSch2 <= paramSch1 && retSch1 <= retSch2
      case _ => throw InternalCompilerException("Incompatible ScopeSchemes")
    }

    def max(scSc1: Explicit, scSc2: Explicit): ScopeScheme.Explicit = (scSc1, scSc2) match {
      case (ScopeScheme.Unit, ScopeScheme.Unit) => ScopeScheme.Unit
      case (ScopeScheme.Arrow(paramSco1, paramSch1, retSch1), ScopeScheme.Arrow(paramSco2, paramSch2, retSch2)) =>
        // NB: Contravariant on the left
        ScopeScheme.Arrow(paramSco1 min paramSco2, paramSch1 min paramSch2, retSch1 max retSch2)
      case _ => throw InternalCompilerException("Incompatible ScopeSchemes")
    }

    def min(scSc1: Explicit, scSc2: Explicit): ScopeScheme.Explicit = (scSc1, scSc2) match {
      case (ScopeScheme.Unit, ScopeScheme.Unit) => ScopeScheme.Unit
      case (ScopeScheme.Arrow(paramSco1, paramSch1, retSch1), ScopeScheme.Arrow(paramSco2, paramSch2, retSch2)) =>
        // NB: Contravariant on the left
        ScopeScheme.Arrow(paramSco1 max paramSco2, paramSch1 max paramSch2, retSch1 min retSch2)
      case _ => throw InternalCompilerException("Incompatible ScopeSchemes")
    }
  }

  case object Implicit extends ScopeScheme

  case object Unit extends Explicit

  case class Arrow(paramSco: Scopedness, paramSch: ScopeScheme, retSch: ScopeScheme) extends Explicit

  private def mkUnscoped(scSc: ScopeScheme): ScopeScheme = scSc match {
    case ScopeScheme.Unit => ScopeScheme.Unit
    case ScopeScheme.Arrow(_, paramSch, retSch) => Arrow(Scopedness.Unscoped, mkUnscoped(paramSch), mkUnscoped(retSch))
    case ScopeScheme.Implicit => ScopeScheme.Implicit
  }

  private def mkUnscopedExplicit(scSc: Explicit): Explicit = scSc match {
    case ScopeScheme.Unit => ScopeScheme.Unit
    case ScopeScheme.Arrow(_, paramSch, retSch) => Arrow(Scopedness.Unscoped, mkUnscoped(paramSch), mkUnscoped(retSch))
  }
}
