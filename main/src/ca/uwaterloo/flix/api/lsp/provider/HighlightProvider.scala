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

import ca.uwaterloo.flix.api.lsp.{DocumentHighlight, DocumentHighlightKind, Entity, Index, Position, Range, ResponseStatus}
import ca.uwaterloo.flix.language.ast.TypedAst.{Pattern, Root}
import ca.uwaterloo.flix.language.ast.{Ast, Name, SourceLocation, Symbol, Type, TypeConstructor}
import org.json4s.JsonAST.{JArray, JObject}
import org.json4s.JsonDSL._

object HighlightProvider {

  def processHighlight(uri: String, pos: Position)(implicit index: Index, root: Option[Root]): JObject = {
    index.query(uri, pos) match {
      case None => mkNotFound(uri, pos)

      case Some(entity) => entity match {
        case Entity.Case(caze) => highlightCase(caze.sym).getOrElse(mkNotFound(uri, pos))

        case Entity.Def(defn) => highlightDef(defn.sym)

        case Entity.Sig(sig0) => highlightSig(sig0.sym)

        case Entity.Enum(enum) => highlightEnum(enum.sym)

        case Entity.TypeAlias(alias) => highlightTypeAlias(alias.sym)

        case Entity.AssocType(assoc) => highlightAssocType(assoc.sym)

        case Entity.Effect(eff) => highlightEffect(eff.sym)

        case Entity.Op(op) => highlightOp(op.sym)

        case Entity.VarUse(sym, _, _) => highlightVar(sym)

        case Entity.DefUse(sym, _, _) => highlightDef(sym)

        case Entity.SigUse(sym, _, _) => highlightSig(sym)

        case Entity.CaseUse(sym, _, _) => highlightCase(sym).getOrElse(mkNotFound(uri, pos))

        case Entity.Exp(_) => mkNotFound(uri, pos)

        case Entity.Field(field) => highlightField(field)

        case Entity.FormalParam(fparam) => highlightVar(fparam.sym)

        case Entity.Pattern(pat) => pat match {
          case Pattern.Var(sym, _, _) => highlightVar(sym)
          case Pattern.Tag(Ast.CaseSymUse(sym, _), _, _, _) => highlightCase(sym).getOrElse(mkNotFound(uri, pos))
          case _ => mkNotFound(uri, pos)
        }

        case Entity.Pred(pred, _) => highlightPred(pred)

        case Entity.LocalVar(sym, _) => highlightVar(sym)

        case Entity.Type(t) => t match {
          case Type.Cst(tc, _) => tc match {
            case TypeConstructor.RecordRowExtend(label) => highlightField(label)
            case TypeConstructor.SchemaRowExtend(pred) => highlightPred(pred)
            case TypeConstructor.Enum(sym, _) => highlightEnum(sym)
            case TypeConstructor.Effect(sym) => highlightEffect(sym)
            case _ => mkNotFound(uri, pos)
          }
          case Type.Var(sym, loc) => highlightTypeVar(sym)
          case _ => mkNotFound(uri, pos)
        }

        case Entity.OpUse(sym, _, _) => highlightOp(sym)

        case Entity.Class(_) => mkNotFound(uri, pos)
        case Entity.TypeVar(_) => mkNotFound(uri, pos)
      }
    }
  }

  private def highlight(occurrences: List[(SourceLocation, DocumentHighlightKind)]): JObject = {
    // Construct the highlights.
    val highlights = occurrences map {
      case (loc, kind) => DocumentHighlight(Range.from(loc), kind)
    }

    // Construct the JSON result.
    ("status" -> ResponseStatus.Success) ~ ("result" -> JArray(highlights.map(_.toJSON)))
  }

  private def highlightDef(sym: Symbol.DefnSym)(implicit index: Index, root: Option[Root]): JObject = {
    val write = (sym.loc, DocumentHighlightKind.Write)
    val reads = index.usesOf(sym).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(write :: reads)
  }

  private def highlightSig(sym: Symbol.SigSym)(implicit index: Index, root: Option[Root]): JObject = {
    val write = (sym.loc, DocumentHighlightKind.Write)
    val reads = index.usesOf(sym).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(write :: reads)
  }

  private def highlightEnum(sym: Symbol.EnumSym)(implicit index: Index, root: Option[Root]): JObject = {
    val write = (sym.loc, DocumentHighlightKind.Write)
    val reads = index.usesOf(sym).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(write :: reads)
  }

  private def highlightTypeAlias(sym: Symbol.TypeAliasSym)(implicit index: Index, root: Option[Root]): JObject = {
    val write = (sym.loc, DocumentHighlightKind.Write)
    val reads = index.usesOf(sym).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(write :: reads)
  }

  private def highlightAssocType(sym: Symbol.AssocTypeSym)(implicit index: Index, root: Option[Root]): JObject = {
    val write = (sym.loc, DocumentHighlightKind.Write)
    val reads = index.usesOf(sym).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(write :: reads)
  }

  private def highlightField(field: Name.Field)(implicit index: Index, root: Option[Root]): JObject = {
    val writes = index.defsOf(field).toList.map(loc => (loc, DocumentHighlightKind.Write))
    val reads = index.usesOf(field).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(reads ::: writes)
  }

  private def highlightPred(pred: Name.Pred)(implicit index: Index, root: Option[Root]): JObject = {
    val writes = index.defsOf(pred).toList.map(loc => (loc, DocumentHighlightKind.Write))
    val reads = index.usesOf(pred).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(reads ::: writes)
  }

  private def highlightCase(sym: Symbol.CaseSym)(implicit index: Index, root: Option[Root]): Option[JObject] = {
    if (root.isEmpty) return None
    val write = (root.get.enums(sym.enumSym).cases(sym).loc, DocumentHighlightKind.Write)
    val reads = index.usesOf(sym).toList.map(loc => (loc, DocumentHighlightKind.Read))
    Some(highlight(write :: reads))
  }

  private def highlightVar(sym: Symbol.VarSym)(implicit index: Index, root: Option[Root]): JObject = {
    val write = (sym.loc, DocumentHighlightKind.Write)
    val reads = index.usesOf(sym).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(write :: reads)
  }

  private def highlightTypeVar(sym: Symbol.KindedTypeVarSym)(implicit index: Index, root: Option[Root]): JObject = {
    val write = (sym.loc, DocumentHighlightKind.Write)
    val reads = index.usesOf(sym).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(write :: reads)
  }

  private def highlightEffect(sym: Symbol.EffectSym)(implicit index: Index, root: Option[Root]): JObject = {
    val write = (sym.loc, DocumentHighlightKind.Write)
    val reads = index.usesOf(sym).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(write :: reads)
  }

  private def highlightOp(sym: Symbol.OpSym)(implicit index: Index, root: Option[Root]): JObject = {
    val write = (sym.loc, DocumentHighlightKind.Write)
    val reads = index.usesOf(sym).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(write :: reads)
  }

  /**
    * Returns a reply indicating that nothing was found at the `uri` and `pos`.
    */
  private def mkNotFound(uri: String, pos: Position): JObject =
    ("status" -> ResponseStatus.InvalidRequest) ~ ("message" -> s"Nothing found in '$uri' at '$pos'.")

}
