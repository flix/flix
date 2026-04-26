/*
 * Copyright 2026 Alexander Sommer, Samuel Skovbakke
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
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.Symbol

sealed trait EffSymOrRigidVar {
  def name: String
}

object EffSymOrRigidVar {

  case class Eff(symbol: Symbol.EffSym) extends EffSymOrRigidVar {
    def name: String = symbol.name
  }

  case class RigidVar(symbol: Symbol.KindedTypeVarSym) extends EffSymOrRigidVar {
    def name: String = symbol.text match {
      case VarText.Absent => "???"
      case VarText.SourceText(s) => s
    }
  }
}
