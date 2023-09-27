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


sealed class TypingConstraint

object TypingConstraint {
  case class Equality(tpe1: Type, tpe2: Type, lenv: LevelEnv, loc: SourceLocation) extends TypingConstraint

  case class Class(sym: Symbol.ClassSym, tpe: Type, lenv: LevelEnv, loc: SourceLocation) extends TypingConstraint
}
