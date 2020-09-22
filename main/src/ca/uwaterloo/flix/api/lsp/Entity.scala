/*
 * Copyright 2020 Magnus Madsen
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
package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, TypedAst}

sealed trait Entity {
  def loc: SourceLocation
}

object Entity {

  case class Case(e: TypedAst.Case) extends Entity {
    def loc: SourceLocation = e.loc
  }

  case class Def(e: TypedAst.Def) extends Entity {
    def loc: SourceLocation = e.loc
  }

  case class Enum(e: TypedAst.Enum) extends Entity {
    def loc: SourceLocation = e.loc
  }

  case class Exp(e: TypedAst.Expression) extends Entity {
    def loc: SourceLocation = e.loc
  }

  case class FormalParam(e: TypedAst.FormalParam) extends Entity {
    def loc: SourceLocation = e.loc
  }

  case class Pattern(e: TypedAst.Pattern) extends Entity {
    def loc: SourceLocation = e.loc
  }

  case class LocalVar(sym: Symbol.VarSym) extends Entity {
    def loc: SourceLocation = sym.loc
  }

}
