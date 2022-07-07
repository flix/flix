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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.{Entity, Index, MarkupContent, MarkupKind, Position, Range}
import ca.uwaterloo.flix.language.ast.TypedAst.{Expression, Root}
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.fmt._
import ca.uwaterloo.flix.language.phase.unification.BoolTable
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL._

object HoverProvider {

  implicit val audience: Audience = Audience.External

  def processHover(uri: String, pos: Position, current: Boolean)(implicit index: Index, root: Root, flix: Flix): JObject = {
    index.query(uri, pos) match {
      case None => mkNotFound(uri, pos)

      case Some(entity) => entity match {

        case Entity.Case(caze) => hoverType(caze.sc.base, caze.tag.loc, current)

        case Entity.Exp(exp) =>
          exp match {
            case Expression.Def(sym, _, loc) => hoverDef(sym, loc, current)

            case Expression.Sig(sym, _, loc) => hoverSig(sym, loc, current)

            case _ => hoverTypAndEff(exp.tpe, exp.pur, exp.loc, current)
          }

        case Entity.FormalParam(fparam) => hoverType(fparam.tpe, fparam.loc, current)

        case Entity.Pattern(pat) => hoverType(pat.tpe, pat.loc, current)

        case Entity.Pred(pred, tpe) => hoverType(tpe, pred.loc, current)

        case Entity.LocalVar(sym, tpe) => hoverType(tpe, sym.loc, current)

        case Entity.Type(t) => hoverKind(t, current)

        case _ => mkNotFound(uri, pos)
      }
    }
  }

  private def hoverType(tpe: Type, loc: SourceLocation, current: Boolean)(implicit index: Index, root: Root): JObject = {
    val markup =
      s"""${if (!current) s"**Not Current**${System.lineSeparator()}" else ""}```flix
         |${FormatType.formatWellKindedType(tpe)}
         |```
         |""".stripMargin
    val contents = MarkupContent(MarkupKind.Markdown, markup)
    val range = Range.from(loc)
    val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
    ("status" -> "success") ~ ("result" -> result)
  }

  private def hoverTypAndEff(tpe: Type, eff: Type, loc: SourceLocation, current: Boolean)(implicit index: Index, root: Root, flix: Flix): JObject = {
    val minEff = BoolTable.minimizeType(eff)
    val markup =
      s"""${if (!current) s"**Not Current**${System.lineSeparator()}" else ""}```flix
         |${formatTypAndEff(tpe, minEff)}
         |```
         |""".stripMargin
    val contents = MarkupContent(MarkupKind.Markdown, markup)
    val range = Range.from(loc)
    val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
    ("status" -> "success") ~ ("result" -> result)
  }

  private def hoverDef(sym: Symbol.DefnSym, loc: SourceLocation, current: Boolean)(implicit index: Index, root: Root): JObject = {
    val defDecl = root.defs(sym)
    val markup =
      s"""${if (!current) s"**Not Current**${System.lineSeparator()}" else ""}```flix
         |${FormatSignature.asMarkDown(defDecl)}
         |```
         |
         |${FormatDoc.asMarkDown(defDecl.spec.doc)}
         |""".stripMargin
    val contents = MarkupContent(MarkupKind.Markdown, markup)
    val range = Range.from(loc)
    val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
    ("status" -> "success") ~ ("result" -> result)
  }

  private def hoverSig(sym: Symbol.SigSym, loc: SourceLocation, current: Boolean)(implicit index: Index, root: Root): JObject = {
    val sigDecl = root.sigs(sym)
    val markup =
      s"""${if (!current) s"**Not Current**${System.lineSeparator()}" else ""}```flix
         |${FormatSignature.asMarkDown(sigDecl)}
         |```
         |
         |${FormatDoc.asMarkDown(sigDecl.spec.doc)}
         |""".stripMargin
    val contents = MarkupContent(MarkupKind.Markdown, markup)
    val range = Range.from(loc)
    val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
    ("status" -> "success") ~ ("result" -> result)
  }

  private def formatTypAndEff(tpe0: Type, eff0: Type): String = {
    val t = FormatType.formatWellKindedType(tpe0)
    val e = eff0 match {
      case Type.Cst(TypeConstructor.True, _) => "Pure"
      case Type.Cst(TypeConstructor.False, _) => "Impure"
      case eff => FormatType.formatWellKindedType(eff)
    }
    s"$t & $e"
  }

  private def hoverKind(t: Type, current: Boolean)(implicit index: Index, root: Root): JObject = {
    val markup =
      s"""${if (!current) s"**Not Current**${System.lineSeparator()}" else ""}```flix
         |${FormatKind.formatKind(t.kind)}
         |```
         |""".stripMargin
    val contents = MarkupContent(MarkupKind.Markdown, markup)
    val range = Range.from(t.loc)
    val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
    ("status" -> "success") ~ ("result" -> result)
  }

  /**
    * Returns a reply indicating that nothing was found at the `uri` and `pos`.
    */
  private def mkNotFound(uri: String, pos: Position): JObject =
    ("status" -> "failure") ~ ("message" -> s"Nothing found in '$uri' at '$pos'.")

}
