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

import ca.uwaterloo.flix.language.ast
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, Symbol, TypedAst}

sealed trait Entity {
  def loc: SourceLocation

  /**
    * Returns `true` if the given range `range` is fully included in `this` entity
    * (i.e. the given range must start later and end earlier.)
    */
  def isInRange(range: Range): Boolean = {
    ((this.loc.endLine < range.end.line) || (this.loc.endLine == range.end.line && this.loc.endCol <= range.end.character)) &&
      ((this.loc.beginLine > range.start.line) || (this.loc.beginLine == range.start.line && this.loc.beginCol >= range.start.character))
  }
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
    def loc: SourceLocation = e.sym.loc
  }

  case class Sig(e: TypedAst.Sig) extends Entity {
    def loc: SourceLocation = e.sym.loc
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

  case class Pred(e: Name.Pred, tpe: ast.Type) extends Entity {
    def loc: SourceLocation = e.loc
  }

  // TODO: Split this into LetBound and SelectBound?
  case class LocalVar(sym: Symbol.VarSym, tpe: ast.Type) extends Entity {
    def loc: SourceLocation = sym.loc
  }

  case class TypeVar(sym: Symbol.KindedTypeVarSym) extends Entity {
    def loc: SourceLocation = sym.loc
  }

  case class Type(t: ast.Type) extends Entity {
    def loc: SourceLocation = t.loc
  }

  case class Effect(eff: TypedAst.Effect) extends Entity {
    def loc: SourceLocation = eff.sym.loc
  }

  case class Op(op: TypedAst.Op) extends Entity {
    def loc: SourceLocation = op.sym.loc
  }

}
