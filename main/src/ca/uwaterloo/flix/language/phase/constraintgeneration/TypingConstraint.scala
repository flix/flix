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
package ca.uwaterloo.flix.language.phase.constraintgeneration

import ca.uwaterloo.flix.language.ast.{LevelEnv, SourceLocation, Symbol, Type}


sealed trait TypingConstraint {
  override def toString: String = this match {
    case TypingConstraint.Equality(tpe1, tpe2, lenv, prov, loc) => s"$tpe1 ~ $tpe2"
    case TypingConstraint.Class(sym, tpe, lenv, loc) => s"$sym[$tpe]"
  }
}

object TypingConstraint {
  case class Equality(tpe1: Type, tpe2: Type, lenv: LevelEnv, prov: Provenance, loc: SourceLocation) extends TypingConstraint

  case class Class(sym: Symbol.ClassSym, tpe: Type, lenv: LevelEnv, loc: SourceLocation) extends TypingConstraint

  sealed trait Provenance

  object Provenance {

    /**
      * The constraint indicates that the left type is the expected type, while the right type is the actual type.
      */
    object ExpectLeft extends Provenance

    /**
      * The constraint indicates that the left type is the expected type of the `n`th argument to a function.
      */
    case class ExpectLeftArgument(sym: Symbol, num: Int) extends Provenance

    /**
      * The constraint indicates that the types must match.
      */
    object Match extends Provenance
  }
}
