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

import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.{Ast, Name, SourceLocation, Symbol, TypedAst}
import org.json4s.JsonDSL._
import org.json4s._

/**
  * Companion object of [[LocationLink]]
  */
object LocationLink {

  /**
    * Returns a location link to the given symbol `sym`.
    */
  def fromDefSym(sym: Symbol.DefnSym, loc: SourceLocation)(implicit root: Root): LocationLink = {
    val defDecl = root.defs(sym)
    val originSelectionRange = Range.from(loc)
    val targetUri = sym.loc.source.name
    val targetRange = Range.from(sym.loc)
    val targetSelectionRange = Range.from(defDecl.sym.loc)
    LocationLink(originSelectionRange, targetUri, targetRange, targetSelectionRange)
  }

  /**
    * Returns a location link to the given symbol `sym`.
    */
  def fromSigSym(sym: Symbol.SigSym, loc: SourceLocation)(implicit root: Root): LocationLink = {
    val sigDecl = root.sigs(sym)
    val originSelectionRange = Range.from(loc)
    val targetUri = sym.loc.source.name
    val targetRange = Range.from(sym.loc)
    val targetSelectionRange = Range.from(sigDecl.sym.loc)
    LocationLink(originSelectionRange, targetUri, targetRange, targetSelectionRange)
  }

  /**
    * Returns a location link to the given symbol `sym`.
    */
  def fromEnumSym(sym: Symbol.EnumSym, loc: SourceLocation)(implicit root: Root): LocationLink = {
    val enumDecl = root.enums(sym)
    val originSelectionRange = Range.from(loc)
    val targetUri = sym.loc.source.name
    val targetRange = Range.from(enumDecl.loc)
    val targetSelectionRange = Range.from(sym.loc)
    LocationLink(originSelectionRange, targetUri, targetRange, targetSelectionRange)
  }

  /**
   * Returns a location link to the given symbol `sym`.
   */
  def fromStructSym(sym: Symbol.StructSym, loc: SourceLocation)(implicit root: Root): LocationLink = {
    val structDecl = root.structs(sym)
    val originSelectionRange = Range.from(loc)
    val targetUri = sym.loc.source.name
    val targetRange = Range.from(structDecl.loc)
    val targetSelectionRange = Range.from(sym.loc)
    LocationLink(originSelectionRange, targetUri, targetRange, targetSelectionRange)
  }

  /**
    * Returns a location link to the given symbol `sym`.
    */
  def fromCaseSym(sym: Symbol.CaseSym, loc: SourceLocation)(implicit root: Root): LocationLink = {
    val enumDecl = root.enums(sym.enumSym)
    val caseDecl = enumDecl.cases(sym)
    val originSelectionRange = Range.from(loc)
    val targetUri = sym.loc.source.name
    val targetRange = Range.from(caseDecl.loc)
    val targetSelectionRange = Range.from(caseDecl.loc)
    LocationLink(originSelectionRange, targetUri, targetRange, targetSelectionRange)
  }

  /**
   * Returns a location link to the given symbol `sym`.
   */
  def fromStructFieldSym(sym: Symbol.StructFieldSym, loc: SourceLocation)(implicit root: Root): LocationLink = {
    val structDecl = root.structs(sym.structSym)
    val fieldDecl = structDecl.fields(sym)
    val originSelectionRange = Range.from(loc)
    val targetUri = sym.loc.source.name
    val targetRange = Range.from(fieldDecl.loc)
    val targetSelectionRange = Range.from(fieldDecl.loc)
    LocationLink(originSelectionRange, targetUri, targetRange, targetSelectionRange)
  }

  /**
    * Returns a reference to the variable symbol `sym`.
    */
  def fromVarSym(sym: Symbol.VarSym, originLoc: SourceLocation): LocationLink = {
    val originSelectionRange = Range.from(originLoc)
    val targetUri = sym.loc.source.name
    val targetRange = Range.from(sym.loc)
    val targetSelectionRange = Range.from(sym.loc)
    LocationLink(originSelectionRange, targetUri, targetRange, targetSelectionRange)
  }

  /**
    * Returns a reference to the type variable symbol `sym`.
    */
  def fromTypeVarSym(sym: Symbol.KindedTypeVarSym, originLoc: SourceLocation): LocationLink = {
    val originSelectionRange = Range.from(originLoc)
    val targetUri = sym.loc.source.name
    val targetRange = Range.from(sym.loc)
    val targetSelectionRange = Range.from(sym.loc)
    LocationLink(originSelectionRange, targetUri, targetRange, targetSelectionRange)
  }

  /**
    * Returns a reference to the instance node `instance`.
    */
  def fromInstanceTraitSymUse(trt: Ast.TraitSymUse, originLoc: SourceLocation): LocationLink = {
    val originSelectionRange = Range.from(originLoc)
    val targetUri = trt.loc.source.name
    val targetRange = Range.from(trt.loc)
    val targetSelectionRange = Range.from(trt.loc)
    LocationLink(originSelectionRange, targetUri, targetRange, targetSelectionRange)
  }

  /**
    * Returns a reference to the effect symbol `sym`.
    */
  def fromEffectSym(sym: Symbol.EffectSym, originLoc: SourceLocation): LocationLink = {
    val originSelectionRange = Range.from(originLoc)
    val targetUri = sym.loc.source.name
    val targetRange = Range.from(sym.loc)
    val targetSelectionRange = Range.from(sym.loc)
    LocationLink(originSelectionRange, targetUri, targetRange, targetSelectionRange)
  }

  /**
    * Returns a reference to the effect operation symbol `sym`.
    */
  def fromOpSym(sym: Symbol.OpSym, originLoc: SourceLocation): LocationLink = {
    val originSelectionRange = Range.from(originLoc)
    val targetUri = sym.loc.source.name
    val targetRange = Range.from(sym.loc)
    val targetSelectionRange = Range.from(sym.loc)
    LocationLink(originSelectionRange, targetUri, targetRange, targetSelectionRange)
  }
}

/**
  * Represents a `LocationLink` in LSP.
  *
  * @param originSelectionRange Span of the origin of this link. Used as the underlined span for mouse interaction.
  *                             Defaults to the word range at the mouse position.
  * @param targetUri            The target resource identifier of this link.
  * @param targetRange          The full target range of this link. If the target for example is a symbol then target
  *                             range is the range enclosing this symbol not including leading/trailing whitespace but
  *                             everything else like comments. This information is typically used to highlight the
  *                             range in the editor.
  * @param targetSelectionRange The range that should be selected and revealed when this link is being followed,
  *                             e.g the name of a function. Must be contained by the the `targetRange`.
  *                             See also `DocumentSymbol#range`
  */
case class LocationLink(originSelectionRange: Range, targetUri: String, targetRange: Range, targetSelectionRange: Range) {
  def toJSON: JValue =
    ("originSelectionRange" -> originSelectionRange.toJSON) ~
      ("targetUri" -> targetUri) ~
      ("targetRange" -> targetRange.toJSON) ~
      ("targetSelectionRange" -> targetSelectionRange.toJSON)
}
