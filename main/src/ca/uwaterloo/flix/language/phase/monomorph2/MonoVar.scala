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

import ca.uwaterloo.flix.language.ast.Symbol

/**
  * A monomorphization target-variable (def/enum/sig/struct/restrictable-enum) whose concrete
  * instantiation the solver determines, then substitutes back wherever `MonoArg.Param`
  * references it.
  */
sealed trait MonoVar

object MonoVar {
  case class Def(sym: Symbol.DefnSym) extends MonoVar

  case class Enum(sym: Symbol.EnumSym) extends MonoVar

  case class Sig(sym: Symbol.SigSym) extends MonoVar

  case class RestrictableEnum(sym: Symbol.RestrictableEnumSym) extends MonoVar

  case class Struct(sym: Symbol.StructSym) extends MonoVar
}
