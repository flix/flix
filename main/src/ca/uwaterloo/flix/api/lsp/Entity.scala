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

import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}

sealed trait Entity {
  def loc: SourceLocation
}

// TODO: Restructure this?

object Entity {

  case class Case(e: TypedAst.Case) extends Entity {
    def loc: SourceLocation = e.loc
  }

  case class Class(e: TypedAst.Class) extends Entity {
    def loc: SourceLocation = e.sym.loc
  }

  case class Def(e: TypedAst.Def) extends Entity {
    def loc: SourceLocation = e.loc
  }

  case class Enum(e: TypedAst.Enum) extends Entity {
    def loc: SourceLocation = e.sym.loc
  }

  case class Exp(e: TypedAst.Expression) extends Entity {
    def loc: SourceLocation = e.loc
  }

  case class Field(e: Name.Field) extends Entity {
    def loc: SourceLocation = e.loc
  }

  case class FormalParam(e: TypedAst.FormalParam) extends Entity {
    def loc: SourceLocation = e.loc
  }

  case class Pattern(e: TypedAst.Pattern) extends Entity {
    def loc: SourceLocation = e.loc
  }

  case class Pred(e: Name.Pred) extends Entity {
    def loc: SourceLocation = e.loc
  }

  // TODO: Split this into LetBound and SelectBound?
  case class LocalVar(sym: Symbol.VarSym, tpe: Type) extends Entity {
    def loc: SourceLocation = sym.loc
  }

  case class TypeCon(tc: TypeConstructor, loc: SourceLocation) extends Entity

}
