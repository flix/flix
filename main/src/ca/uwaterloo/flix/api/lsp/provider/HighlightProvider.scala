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
package ca.uwaterloo.flix.api.lsp.provider

import ca.uwaterloo.flix.api.lsp.{DocumentHighlight, DocumentHighlightKind, Entity, Index, Position, Range}
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, Symbol}
import ca.uwaterloo.flix.language.ast.TypedAst.{Expression, Pattern, Root}
import org.json4s.JsonAST.{JArray, JValue}
import org.json4s.JsonDSL._

object HighlightProvider {

  /**
    * Processes a highlight request.
    */
  def processHighlight(requestId: String, uri: String, pos: Position)(implicit index: Index, root: Root): JValue = {
    index.query(uri, pos) match {
      case None => mkNotFound(requestId, uri, pos)

      case Some(entity) => entity match {
        case Entity.Case(caze) => highlightTag(requestId, caze.sym, caze.tag)

        case Entity.Def(defn) => highlightDef(requestId, defn.sym)

        case Entity.Enum(enum) => highlightEnum(requestId, enum.sym)

        case Entity.Exp(exp) => exp match {
          case Expression.Var(sym, _, _) => highlightVar(requestId, sym)
          case Expression.Def(sym, _, _) => highlightDef(requestId, sym)
          case Expression.Tag(sym, tag, _, _, _, _) => highlightTag(requestId, sym, tag)
          case _ => mkNotFound(requestId, uri, pos)
        }

        case Entity.Field(field) => highlightField(requestId, field)

        case Entity.FormalParam(fparam) => highlightVar(requestId, fparam.sym)

        case Entity.Pattern(pat) => pat match {
          case Pattern.Var(sym, _, _) => highlightVar(requestId, sym)
          case Pattern.Tag(sym, tag, _, _, _) => highlightTag(requestId, sym, tag)
          case _ => mkNotFound(requestId, uri, pos)
        }

        case Entity.Pred(pred) => highlightPred(requestId, pred)

        case Entity.LocalVar(sym, _) => highlightVar(requestId, sym)

        case _ => mkNotFound(requestId, uri, pos)
      }
    }
  }

  def highlight(requestId: String, occurrences: List[(SourceLocation, DocumentHighlightKind)]): JValue = {
    // Construct the highlights.
    val highlights = occurrences map {
      case (loc, kind) => DocumentHighlight(Range.from(loc), kind)
    }

    // Construct the JSON result.
    ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> JArray(highlights.map(_.toJSON)))
  }

  def highlightDef(requestId: String, sym: Symbol.DefnSym)(implicit index: Index, root: Root): JValue = {
    val write = (sym.loc, DocumentHighlightKind.Write)
    val reads = index.usesOf(sym).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(requestId, write :: reads)
  }

  def highlightEnum(requestId: String, sym: Symbol.EnumSym)(implicit index: Index, root: Root): JValue = {
    val write = (sym.loc, DocumentHighlightKind.Write)
    val reads = index.usesOf(sym).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(requestId, write :: reads)
  }

  def highlightField(requestId: String, field: Name.Field)(implicit index: Index, root: Root): JValue = {
    val reads = index.usesOf(field).toList.map(loc => (loc, DocumentHighlightKind.Read))
    val writes = index.defsOf(field).toList.map(loc => (loc, DocumentHighlightKind.Write))
    highlight(requestId, reads ::: writes)
  }

  def highlightPred(requestId: String, pred: Name.Pred)(implicit index: Index, root: Root): JValue = {
    val reads = index.usesOf(pred).toList.map(loc => (loc, DocumentHighlightKind.Read))
    val writes = index.defsOf(pred).toList.map(loc => (loc, DocumentHighlightKind.Write))
    highlight(requestId, reads ::: writes)
  }

  def highlightTag(requestId: String, sym: Symbol.EnumSym, tag: Name.Tag)(implicit index: Index, root: Root): JValue = {
    val write = (root.enums(sym).cases(tag).loc, DocumentHighlightKind.Write)
    val reads = index.usesOf(sym, tag).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(requestId, write :: reads)
  }

  def highlightVar(requestId: String, sym: Symbol.VarSym)(implicit index: Index, root: Root): JValue = {
    val write = (sym.loc, DocumentHighlightKind.Write)
    val reads = index.usesOf(sym).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(requestId, write :: reads)
  }

  /**
    * Returns a reply indicating that nothing was found at the `uri` and `pos`.
    */
  private def mkNotFound(requestId: String, uri: String, pos: Position): JValue =
    ("id" -> requestId) ~ ("status" -> "failure") ~ ("message" -> s"Nothing found in '$uri' at '$pos'.")

}
