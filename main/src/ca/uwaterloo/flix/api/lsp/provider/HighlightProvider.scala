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
import ca.uwaterloo.flix.language.ast.TypedAst.{Expression, Pattern, Root}
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, Symbol, TypeConstructor}
import org.json4s.JsonAST.{JArray, JObject}
import org.json4s.JsonDSL._

object HighlightProvider {

  def processHighlight(uri: String, pos: Position)(implicit index: Index, root: Root): JObject = {
    index.query(uri, pos) match {
      case None => mkNotFound(uri, pos)

      case Some(entity) => entity match {
        case Entity.Case(caze) => highlightTag(caze.sym, caze.tag)

        case Entity.Def(defn) => highlightDef(defn.sym)

        case Entity.Sig(sig0) => highlightSig(sig0.sym)

        case Entity.Enum(enum) => highlightEnum(enum.sym)

        case Entity.Exp(exp) => exp match {
          case Expression.Var(sym, _, _) => highlightVar(sym)
          case Expression.Def(sym, _, _) => highlightDef(sym)
          case Expression.Sig(sym, _, _) => highlightSig(sym)
          case Expression.Tag(sym, tag, _, _, _, _) => highlightTag(sym, tag)
          case _ => mkNotFound(uri, pos)
        }

        case Entity.Field(field) => highlightField(field)

        case Entity.FormalParam(fparam) => highlightVar(fparam.sym)

        case Entity.Pattern(pat) => pat match {
          case Pattern.Var(sym, _, _) => highlightVar(sym)
          case Pattern.Tag(sym, tag, _, _, _) => highlightTag(sym, tag)
          case _ => mkNotFound(uri, pos)
        }

        case Entity.Pred(pred) => highlightPred(pred)

        case Entity.LocalVar(sym, _) => highlightVar(sym)

        case Entity.TypeCon(tc, loc) => tc match {
          case TypeConstructor.RecordExtend(field) => highlightField(field)
          case TypeConstructor.SchemaExtend(pred) => highlightPred(pred)
          case TypeConstructor.KindedEnum(sym, _) => highlightEnum(sym)
          case _ => mkNotFound(uri, pos)
        }

        case _ => mkNotFound(uri, pos)
      }
    }
  }

  private def highlight(occurrences: List[(SourceLocation, DocumentHighlightKind)]): JObject = {
    // Construct the highlights.
    val highlights = occurrences map {
      case (loc, kind) => DocumentHighlight(Range.from(loc), kind)
    }

    // Construct the JSON result.
    ("status" -> "success") ~ ("result" -> JArray(highlights.map(_.toJSON)))
  }

  private def highlightDef(sym: Symbol.DefnSym)(implicit index: Index, root: Root): JObject = {
    val write = (sym.loc, DocumentHighlightKind.Write)
    val reads = index.usesOf(sym).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(write :: reads)
  }

  private def highlightSig(sym: Symbol.SigSym)(implicit index: Index, root: Root): JObject = {
    val write = (sym.loc, DocumentHighlightKind.Write)
    val reads = index.usesOf(sym).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(write :: reads)
  }

  private def highlightEnum(sym: Symbol.EnumSym)(implicit index: Index, root: Root): JObject = {
    val write = (sym.loc, DocumentHighlightKind.Write)
    val reads = index.usesOf(sym).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(write :: reads)
  }

  private def highlightField(field: Name.Field)(implicit index: Index, root: Root): JObject = {
    val writes = index.defsOf(field).toList.map(loc => (loc, DocumentHighlightKind.Write))
    val reads = index.usesOf(field).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(reads ::: writes)
  }

  private def highlightPred(pred: Name.Pred)(implicit index: Index, root: Root): JObject = {
    val writes = index.defsOf(pred).toList.map(loc => (loc, DocumentHighlightKind.Write))
    val reads = index.usesOf(pred).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(reads ::: writes)
  }

  private def highlightTag(sym: Symbol.EnumSym, tag: Name.Tag)(implicit index: Index, root: Root): JObject = {
    val write = (root.enums(sym).cases(tag).loc, DocumentHighlightKind.Write)
    val reads = index.usesOf(sym, tag).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(write :: reads)
  }

  private def highlightVar(sym: Symbol.VarSym)(implicit index: Index, root: Root): JObject = {
    val write = (sym.loc, DocumentHighlightKind.Write)
    val reads = index.usesOf(sym).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(write :: reads)
  }

  /**
    * Returns a reply indicating that nothing was found at the `uri` and `pos`.
    */
  private def mkNotFound(uri: String, pos: Position): JObject =
    ("status" -> "failure") ~ ("message" -> s"Nothing found in '$uri' at '$pos'.")

}
