/*
 * Copyright 2025 Jakob Schneider Villumsen
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
package ca.uwaterloo.flix.api.effectlock.serialization

sealed trait SType

object SType {

  case class Var(sym: SSymbol.VarSym) extends SType

  case class Cst(tc: STC) extends SType

  case class Apply(tpe1: SType, tpe2: SType) extends SType

  case class Alias(symUse: SSymbol.TypeAliasSym, args: List[SType], tpe: SType) extends SType

  case class AssocType(symUse: SSymbol.AssocTypeSym, arg: SType, kind: SKind) extends SType

}
