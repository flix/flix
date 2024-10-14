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
import ca.uwaterloo.flix.language.ast.shared.SymUse.CaseSymUse
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, Symbol, Type, TypeConstructor}
import org.json4s.JsonAST.{JArray, JObject}
import org.json4s.JsonDSL.*

object HighlightProvider {

  def processHighlight(uri: String, pos: Position)(implicit index: Index, root: Root): JObject = {
    index.query(uri, pos) match {
      case None => mkNotFound(uri, pos)

      case Some(entity) => entity match {
        case Entity.Case(caze) => highlightCase(uri, caze.sym)

        case Entity.StructField(field) => highlightStructField(uri, field.sym)

        case Entity.Def(defn) => highlightDef(uri, defn.sym)

        case Entity.Sig(sig0) => highlightSig(uri, sig0.sym)

        case Entity.Enum(enum0) => highlightEnum(uri, enum0.sym)

        case Entity.Struct(struct) => highlightStruct(uri, struct.sym)

        case Entity.TypeAlias(alias) => highlightTypeAlias(uri, alias.sym)

        case Entity.AssocType(assoc) => highlightAssocType(uri, assoc.sym)

        case Entity.Effect(eff) => highlightEffect(uri, eff.sym)

        case Entity.Op(op) => highlightOp(uri, op.sym)

        case Entity.VarUse(sym, _, _) => highlightVar(uri, sym)

        case Entity.DefUse(sym, _, _) => highlightDef(uri, sym)

        case Entity.SigUse(sym, _, _) => highlightSig(uri, sym)

        case Entity.CaseUse(sym, _, _) => highlightCase(uri, sym)

        case Entity.StructFieldUse(sym, _, _) => highlightStructField(uri, sym)

        case Entity.Exp(_) => mkNotFound(uri, pos)

        case Entity.Label(label) => highlightLabel(uri, label)

        case Entity.FormalParam(fparam) => highlightVar(uri, fparam.sym)

        case Entity.Pattern(pat) => pat match {
          case Pattern.Var(sym, _, _) => highlightVar(uri, sym)
          case Pattern.Tag(CaseSymUse(sym, _), _, _, _) => highlightCase(uri, sym)
          case _ => mkNotFound(uri, pos)
        }

        case Entity.Pred(pred, _) => highlightPred(uri, pred)

        case Entity.LocalVar(sym, _) => highlightVar(uri, sym)

        case Entity.Type(t) => t match {
          case Type.Cst(tc, _) => tc match {
            case TypeConstructor.RecordRowExtend(label) => highlightLabel(uri, label)
            case TypeConstructor.SchemaRowExtend(pred) => highlightPred(uri, pred)
            case TypeConstructor.Enum(sym, _) => highlightEnum(uri, sym)
            case TypeConstructor.Effect(sym) => highlightEffect(uri, sym)
            case _ => mkNotFound(uri, pos)
          }
          case Type.Var(sym, _) => highlightTypeVar(uri, sym)
          case _ => mkNotFound(uri, pos)
        }

        case Entity.OpUse(sym, _, _) => highlightOp(uri, sym)

        case Entity.Trait(_) => mkNotFound(uri, pos)
        case Entity.TypeVar(_) => mkNotFound(uri, pos)
      }
    }
  }

  private def highlight(uri: String, occurrences: List[(SourceLocation, DocumentHighlightKind)]): JObject = {
    // Filter out highlights outside of document.
    val occurrencesInDoc = occurrences filter {
      case (loc, _) => loc.source.name == uri
    }

    // Construct the highlights.
    val highlights = occurrencesInDoc map {
      case (loc, kind) => DocumentHighlight(Range.from(loc), kind)
    }

    // Construct the JSON result.
    ("status" -> ResponseStatus.Success) ~ ("result" -> JArray(highlights.map(_.toJSON)))
  }

  private def highlightDef(uri: String, sym: Symbol.DefnSym)(implicit index: Index): JObject = {
    val write = (sym.loc, DocumentHighlightKind.Write)
    val reads = index.usesOf(sym).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(uri, write :: reads)
  }

  private def highlightSig(uri: String, sym: Symbol.SigSym)(implicit index: Index): JObject = {
    val write = (sym.loc, DocumentHighlightKind.Write)
    val reads = index.usesOf(sym).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(uri, write :: reads)
  }

  private def highlightEnum(uri: String, sym: Symbol.EnumSym)(implicit index: Index): JObject = {
    val write = (sym.loc, DocumentHighlightKind.Write)
    val reads = index.usesOf(sym).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(uri, write :: reads)
  }

  private def highlightStruct(uri: String, sym: Symbol.StructSym)(implicit index: Index): JObject = {
    val write = (sym.loc, DocumentHighlightKind.Write)
    val reads = index.usesOf(sym).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(uri, write :: reads)
  }

  private def highlightTypeAlias(uri: String, sym: Symbol.TypeAliasSym)(implicit index: Index): JObject = {
    val write = (sym.loc, DocumentHighlightKind.Write)
    val reads = index.usesOf(sym).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(uri, write :: reads)
  }

  private def highlightAssocType(uri: String, sym: Symbol.AssocTypeSym)(implicit index: Index): JObject = {
    val write = (sym.loc, DocumentHighlightKind.Write)
    val reads = index.usesOf(sym).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(uri, write :: reads)
  }

  private def highlightLabel(uri: String, label: Name.Label)(implicit index: Index): JObject = {
    val writes = index.defsOf(label).toList.map(loc => (loc, DocumentHighlightKind.Write))
    val reads = index.usesOf(label).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(uri, reads ::: writes)
  }

  private def highlightPred(uri: String, pred: Name.Pred)(implicit index: Index): JObject = {
    val writes = index.defsOf(pred).toList.map(loc => (loc, DocumentHighlightKind.Write))
    val reads = index.usesOf(pred).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(uri, reads ::: writes)
  }

  private def highlightCase(uri: String, sym: Symbol.CaseSym)(implicit index: Index, root: Root): JObject = {
    val write = root.enums.get(sym.enumSym) match {
      case Some(enm) => enm.cases.get(sym) match {
        case Some(caze) => List((caze.loc, DocumentHighlightKind.Write))
        case _ => Nil
      }
      case _ => Nil
    }
    val reads = index.usesOf(sym).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(uri, write ++ reads)
  }

  private def highlightStructField(uri: String, sym: Symbol.StructFieldSym)(implicit index: Index, root: Root): JObject = {
    val reads = index.usesOf(sym).toList.map(loc => (loc, DocumentHighlightKind.Read))
    val structOpt = root.structs.get(sym.structSym)
    val fieldOpt = structOpt.flatMap(st => st.fields.get(sym))
    val writeOpt = fieldOpt.map(field => (field.sym.loc, DocumentHighlightKind.Write))
    writeOpt match {
      case Some(write) =>
        highlight(uri, write :: reads)
      case None => highlight(uri, reads)
    }
  }

  private def highlightVar(uri: String, sym: Symbol.VarSym)(implicit index: Index): JObject = {
    val write = (sym.loc, DocumentHighlightKind.Write)
    val reads = index.usesOf(sym).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(uri, write :: reads)
  }

  private def highlightTypeVar(uri: String, sym: Symbol.KindedTypeVarSym)(implicit index: Index): JObject = {
    val write = (sym.loc, DocumentHighlightKind.Write)
    val reads = index.usesOf(sym).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(uri, write :: reads)
  }

  private def highlightEffect(uri: String, sym: Symbol.EffectSym)(implicit index: Index): JObject = {
    val write = (sym.loc, DocumentHighlightKind.Write)
    val reads = index.usesOf(sym).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(uri, write :: reads)
  }

  private def highlightOp(uri: String, sym: Symbol.OpSym)(implicit index: Index): JObject = {
    val write = (sym.loc, DocumentHighlightKind.Write)
    val reads = index.usesOf(sym).toList.map(loc => (loc, DocumentHighlightKind.Read))
    highlight(uri, write :: reads)
  }

  /**
    * Returns a reply indicating that nothing was found at the `uri` and `pos`.
    */
  private def mkNotFound(uri: String, pos: Position): JObject =
    ("status" -> ResponseStatus.InvalidRequest) ~ ("message" -> s"Nothing found in '$uri' at '$pos'.")

}
