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
package ca.uwaterloo.flix.language.phase.typer

import ca.uwaterloo.flix.language.ast.{Kind, SourceLocation, Symbol, Type}


/**
  * A constraint generated via type inference.
  */
sealed trait TypeConstraint {

  /**
    * The index indicates the order in which constraints will be evaluated.
    * A constraint with a lower index is reduced first if possible.
    */
  lazy val index: (Int, Int, Int) = this match {
    case TypeConstraint.Equality(_: Type.Var, Type.Pure, _) => (0, 0, 0)
    case TypeConstraint.Equality(Type.Pure, _: Type.Var, _) => (0, 0, 0)
    case TypeConstraint.Equality(tvar1: Type.Var, tvar2: Type.Var, _) if tvar1 != tvar2 => (0, 0, 0)
    case TypeConstraint.Purification(_, _, _, _, _) => (0, 0, 0)
    case TypeConstraint.Equality(tpe1, tpe2, _) =>
      val tvars = tpe1.typeVars ++ tpe2.typeVars
      val effTvars = tvars.filter(_.kind == Kind.Eff)
      (1, effTvars.size, tvars.size)
    case TypeConstraint.Trait(_, _, _) => (2, 0, 0)
  }

  override def toString: String = this match {
    case TypeConstraint.Equality(tpe1, tpe2, _) => s"$tpe1 ~ $tpe2"
    case TypeConstraint.Trait(sym, tpe, _) => s"$sym[$tpe]"
    case TypeConstraint.Purification(sym, eff1, eff2, _, nested) => s"$eff1 ~ ($eff2)[$sym ↦ Pure] ∧ $nested"
  }

  /**
    * Returns the number of type variables in the constraint.
    */
  def numVars: Int = this match {
    case TypeConstraint.Equality(tpe1, tpe2, _) => tpe1.typeVars.size + tpe2.typeVars.size
    case TypeConstraint.Trait(_, tpe, _) => tpe.typeVars.size
    case TypeConstraint.Purification(_, eff1, eff2, _, _) => eff1.typeVars.size + eff2.typeVars.size
  }

  def loc: SourceLocation
}

object TypeConstraint {

  /**
    * A constraint indicating the equivalence of two types.
    * {{{
    *   tpe1 ~ tpe2
    * }}}
    */
  case class Equality(tpe1: Type, tpe2: Type, prov: Provenance) extends TypeConstraint {
    def loc: SourceLocation = prov.loc
  }

  /**
    * A constraint indicating that the given type is a member of the given trait.
    * {{{
    *   sym[tpe]
    * }}}
    */
  case class Trait(sym: Symbol.TraitSym, tpe: Type, loc: SourceLocation) extends TypeConstraint

  /**
    * A constraint indicating that:
    *   - `eff1` is equivalent to `eff2` when the region `sym` is purified in `eff2`, and
    *   - the nested constraints all hold
    *
    * This constraint arises when exiting a region.
    * All nested constraints must be resolved before determining the equality of `eff1` and `eff2`,
    * because the nested constraints influence `eff2`.
    *
    * {{{
    *   eff1 ~ eff2[sym ↦ Pure] ∧ nested
    * }}}
    */
  case class Purification(sym: Symbol.KindedTypeVarSym, eff1: Type, eff2: Type, prov: Provenance, nested: List[TypeConstraint]) extends TypeConstraint {
    def loc: SourceLocation = prov.loc
  }

  sealed trait Provenance {
    def loc: SourceLocation
  }

  object Provenance {

    /**
      * The constraint indicates that the left type is the expected type, while the right type is the actual type.
      */
    case class ExpectType(expected: Type, actual: Type, loc: SourceLocation) extends Provenance

    /**
      * The constraint indicates that the left effect is the expected effect, while the right effect is the actual effect.
      */
    case class ExpectEffect(expected: Type, actual: Type, loc: SourceLocation) extends Provenance

    /**
      * The constraint indicates that the left type is the expected type of the `n`th argument to a function.
      */
    case class ExpectArgument(expected: Type, actual: Type, sym: Symbol, num: Int, loc: SourceLocation) extends Provenance

    /**
      * The constraint indicates that the types must match.
      */
    case class Match(tpe1: Type, tpe2: Type, loc: SourceLocation) extends Provenance
  }
}
