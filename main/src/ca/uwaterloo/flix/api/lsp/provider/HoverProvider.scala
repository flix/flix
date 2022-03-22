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

import ca.uwaterloo.flix.api.lsp.{Entity, Index, MarkupContent, MarkupKind, Position, Range}
import ca.uwaterloo.flix.language.ast.TypedAst.{Expression, Root}
import ca.uwaterloo.flix.language.ast.{Ast, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.dbg._
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL._

object HoverProvider {

  implicit val audience: Audience = Audience.External

  def processHover(uri: String, pos: Position)(implicit index: Index, root: Root): JObject = {
    index.query(uri, pos) match {
      case None => mkNotFound(uri, pos)

      case Some(entity) => entity match {

        case Entity.Case(caze) => hoverType(caze.sc.base, caze.tag.loc)

        case Entity.Exp(exp) =>
          exp match {
            case Expression.Def(sym, _, loc) => hoverDef(sym, loc)

            case Expression.Sig(sym, _, loc) => hoverSig(sym, loc)

            case _ => hoverTypAndEff(exp.tpe, exp.eff, exp.loc)
          }

        case Entity.FormalParam(fparam) => hoverType(fparam.tpe, fparam.loc)

        case Entity.Pattern(pat) => hoverType(pat.tpe, pat.loc)

        case Entity.LocalVar(sym, tpe) => hoverType(tpe, sym.loc)

        case Entity.TypeCon(tc, loc) => hoverTypeConstructor(tc, loc)

        case _ => mkNotFound(uri, pos)
      }
    }
  }

  private def hoverType(tpe: Type, loc: SourceLocation)(implicit index: Index, root: Root): JObject = {
    val markup =
      s"""```flix
         |${FormatType.formatWellKindedType(tpe)}
         |```
         |""".stripMargin
    val contents = MarkupContent(MarkupKind.Markdown, markup)
    val range = Range.from(loc)
    val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
    ("status" -> "success") ~ ("result" -> result)
  }

  private def hoverTypAndEff(tpe: Type, eff: Type, loc: SourceLocation)(implicit index: Index, root: Root): JObject = {
    val markup =
      s"""```flix
         |${formatTypAndEff(tpe, eff)}
         |```
         |""".stripMargin
    val contents = MarkupContent(MarkupKind.Markdown, markup)
    val range = Range.from(loc)
    val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
    ("status" -> "success") ~ ("result" -> result)
  }

  private def hoverDef(sym: Symbol.DefnSym, loc: SourceLocation)(implicit index: Index, root: Root): JObject = {
    val defDecl = root.defs(sym)
    val markup =
      s"""```flix
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

  private def hoverSig(sym: Symbol.SigSym, loc: SourceLocation)(implicit index: Index, root: Root): JObject = {
    val sigDecl = root.sigs(sym)
    val markup =
      s"""```flix
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

  private def hoverTypeConstructor(tc: TypeConstructor, loc: SourceLocation)(implicit index: Index, root: Root): JObject = {
    val markup =
      s"""```flix
         |${formatKind(tc)}
         |```
         |""".stripMargin
    val contents = MarkupContent(MarkupKind.Markdown, markup)
    val range = Range.from(loc)
    val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
    ("status" -> "success") ~ ("result" -> result)
  }

  private def formatKind(tc: TypeConstructor): String = {
    FormatKind.formatKind(tc.kind)
  }

  /**
    * Returns a reply indicating that nothing was found at the `uri` and `pos`.
    */
  private def mkNotFound(uri: String, pos: Position): JObject =
    ("status" -> "failure") ~ ("message" -> s"Nothing found in '$uri' at '$pos'.")

}
