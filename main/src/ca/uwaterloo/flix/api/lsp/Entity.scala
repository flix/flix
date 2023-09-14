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
  /**
    * The source location of the entity.
    */
  def loc: SourceLocation

  /**
    * The precision of an entity.
    */
  def precision: Entity.Precision

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

  /**
    * Precision indicates how descriptive an entity is.
    * Low precision indicates a generic entity, such as a expression or type.
    * High precision indicates a specific entity, such as a definition declaration or the use of a definition symbol.
    */
  sealed trait Precision extends Ordered[Precision] {
    override def compare(that: Precision): Int = (this, that) match {
      case (Precision.Low, Precision.Low) => 0
      case (Precision.Low, Precision.High) => -1
      case (Precision.High, Precision.Low) => 1
      case (Precision.High, Precision.High) => 0
    }
  }

  object Precision {
    case object Low extends Precision
    case object High extends Precision
  }

  case class Case(e: TypedAst.Case) extends Entity {
    def loc: SourceLocation = e.loc
    def precision: Precision = Precision.High
  }

  case class Class(e: TypedAst.Class) extends Entity {
    def loc: SourceLocation = e.sym.loc
    def precision: Precision = Precision.High
  }

  case class Def(e: TypedAst.Def) extends Entity {
    def loc: SourceLocation = e.sym.loc
    def precision: Precision = Precision.High
  }

  case class Sig(e: TypedAst.Sig) extends Entity {
    def loc: SourceLocation = e.sym.loc
    def precision: Precision = Precision.High
  }

  case class Enum(e: TypedAst.Enum) extends Entity {
    def loc: SourceLocation = e.sym.loc
    def precision: Precision = Precision.High
  }

  case class TypeAlias(e: TypedAst.TypeAlias) extends Entity {
    def loc: SourceLocation = e.sym.loc
    def precision: Precision = Precision.High
  }

  case class AssocType(e: TypedAst.AssocTypeSig) extends Entity {
    def loc: SourceLocation = e.sym.loc

    def precision: Precision = Precision.High
  }

  case class Exp(e: TypedAst.Expr) extends Entity {
    def loc: SourceLocation = e.loc
    def precision: Precision = Precision.Low
  }

  case class Label(e: Name.Label) extends Entity {
    def loc: SourceLocation = e.loc

    def precision: Precision = Precision.High
  }

  case class FormalParam(e: TypedAst.FormalParam) extends Entity {
    def loc: SourceLocation = e.loc
    def precision: Precision = Precision.High
  }

  case class Pattern(e: TypedAst.Pattern) extends Entity {
    def loc: SourceLocation = e.loc
    def precision: Precision = Precision.High
  }

  case class Pred(e: Name.Pred, tpe: ast.Type) extends Entity {
    def loc: SourceLocation = e.loc
    def precision: Precision = Precision.High
  }

  // TODO: Split this into LetBound and SelectBound?
  case class LocalVar(sym: Symbol.VarSym, tpe: ast.Type) extends Entity {
    def loc: SourceLocation = sym.loc
    def precision: Precision = Precision.High
  }

  case class TypeVar(sym: Symbol.KindedTypeVarSym) extends Entity {
    def loc: SourceLocation = sym.loc
    def precision: Precision = Precision.High
  }

  case class Type(t: ast.Type) extends Entity {
    def loc: SourceLocation = t.loc
    def precision: Precision = Precision.Low
  }

  case class Effect(eff: TypedAst.Effect) extends Entity {
    def loc: SourceLocation = eff.sym.loc
    def precision: Precision = Precision.High
  }

  case class Op(op: TypedAst.Op) extends Entity {
    def loc: SourceLocation = op.sym.loc
    def precision: Precision = Precision.High
  }

  case class OpUse(sym: Symbol.OpSym, loc: SourceLocation, parent: Entity) extends Entity {
    def precision: Precision = Precision.High
  }

  case class DefUse(sym: Symbol.DefnSym, loc: SourceLocation, parent: Entity) extends Entity {
    def precision: Precision = Precision.High
  }

  case class SigUse(sym: Symbol.SigSym, loc: SourceLocation, parent: Entity) extends Entity {
    def precision: Precision = Precision.High
  }

  case class VarUse(sym: Symbol.VarSym, loc: SourceLocation, parent: Entity) extends Entity {
    def precision: Precision = Precision.High
  }

  case class CaseUse(sym: Symbol.CaseSym, loc: SourceLocation, parent: Entity) extends Entity {
    def precision: Precision = Precision.High
  }

}
