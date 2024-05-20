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
import ca.uwaterloo.flix.api.lsp.{Entity, Index, MarkupContent, MarkupKind, Position, Range, ResponseStatus}
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.fmt._
import ca.uwaterloo.flix.language.phase.unification.SetFormula
import ca.uwaterloo.flix.language.phase.unification.TypeMinimization.minimizeType
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL._

import scala.annotation.tailrec

object HoverProvider {

  def processHover(uri: String, pos: Position)(implicit index: Index, root: Root, flix: Flix): JObject = {
    index.query(uri, pos) match {
      case None => mkNotFound(uri, pos)

      case Some(entity) => hoverEntity(entity, uri, pos)
    }
  }

  @tailrec
  private def hoverEntity(entity: Entity, uri: String, pos: Position)(implicit Index: Index, root: Root, flix: Flix): JObject = entity match {

    case Entity.Case(caze) => hoverType(caze.tpe, caze.sym.loc)

    case Entity.DefUse(sym, loc, _) => hoverDef(sym, loc)

    case Entity.SigUse(sym, loc, _) => hoverSig(sym, loc)

    case Entity.VarUse(_, _, parent) => hoverEntity(parent, uri, pos)

    case Entity.CaseUse(_, _, parent) => hoverEntity(parent, uri, pos)

    case Entity.Exp(exp) => hoverTypeAndEff(exp.tpe, exp.eff, exp.loc)

    case Entity.FormalParam(fparam) => hoverType(fparam.tpe, fparam.loc)

    case Entity.Pattern(pat) => hoverType(pat.tpe, pat.loc)

    case Entity.Pred(pred, tpe) => hoverType(tpe, pred.loc)

    case Entity.LocalVar(sym, tpe) => hoverType(tpe, sym.loc)

    case Entity.Type(t) => hoverKind(t)

    case Entity.OpUse(sym, loc, _) => hoverOp(sym, loc)

    case Entity.Trait(_) => mkNotFound(uri, pos)
    case Entity.Def(_) => mkNotFound(uri, pos)
    case Entity.Effect(_) => mkNotFound(uri, pos)
    case Entity.Enum(_) => mkNotFound(uri, pos)
    case Entity.TypeAlias(_) => mkNotFound(uri, pos)
    case Entity.AssocType(_) => mkNotFound(uri, pos)
    case Entity.Label(_) => mkNotFound(uri, pos)
    case Entity.Op(_) => mkNotFound(uri, pos)
    case Entity.Sig(_) => mkNotFound(uri, pos)
    case Entity.TypeVar(_) => mkNotFound(uri, pos)
  }

  private def hoverType(tpe: Type, loc: SourceLocation)(implicit root: Root, flix: Flix): JObject = {
    val minTpe = minimizeType(tpe)
    val lowerAndUpperBounds = SetFormula.formatLowerAndUpperBounds(minTpe)(root)
    val markup =
      s"""```flix
         |${FormatType.formatType(minTpe)}$lowerAndUpperBounds
         |```
         |""".stripMargin
    val contents = MarkupContent(MarkupKind.Markdown, markup)
    val range = Range.from(loc)
    val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
    ("status" -> ResponseStatus.Success) ~ ("result" -> result)
  }

  private def hoverTypeAndEff(tpe: Type, eff: Type, loc: SourceLocation)(implicit flix: Flix): JObject = {
    val minEff = minimizeType(eff)
    val minTpe = minimizeType(tpe)
    val markup =
      s"""```flix
         |${formatTypAndEff(minTpe, minEff)}
         |```
         |""".stripMargin
    val contents = MarkupContent(MarkupKind.Markdown, markup)
    val range = Range.from(loc)
    val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
    ("status" -> ResponseStatus.Success) ~ ("result" -> result)
  }

  private def hoverDef(sym: Symbol.DefnSym, loc: SourceLocation)(implicit root: Root, flix: Flix): JObject = {
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
    ("status" -> ResponseStatus.Success) ~ ("result" -> result)
  }

  private def hoverSig(sym: Symbol.SigSym, loc: SourceLocation)(implicit root: Root, flix: Flix): JObject = {
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
    ("status" -> ResponseStatus.Success) ~ ("result" -> result)
  }

  private def hoverOp(sym: Symbol.OpSym, loc: SourceLocation)(implicit root: Root, flix: Flix): JObject = {
    val opDecl = root.effects(sym.eff).ops.find(_.sym == sym).get // guaranteed to be present
    val markup =
      s"""```flix
         |${FormatSignature.asMarkDown(opDecl)}
         |```
         |
         |${FormatDoc.asMarkDown(opDecl.spec.doc)}
         |""".stripMargin
    val contents = MarkupContent(MarkupKind.Markdown, markup)
    val range = Range.from(loc)
    val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
    ("status" -> ResponseStatus.Success) ~ ("result" -> result)
  }

  private def formatTypAndEff(tpe0: Type, eff0: Type)(implicit flix: Flix): String = {
    // TODO deduplicate with CompletionProvider
    val t = FormatType.formatType(tpe0)

    val p = eff0 match {
      case Type.Cst(TypeConstructor.Pure, _) => ""
      case eff => raw" \ " + FormatType.formatType(eff)
    }

    s"$t$p"
  }

  private def hoverKind(t: Type): JObject = {
    val markup =
      s"""```flix
         |${FormatKind.formatKind(t.kind)}
         |```
         |""".stripMargin
    val contents = MarkupContent(MarkupKind.Markdown, markup)
    val range = Range.from(t.loc)
    val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
    ("status" -> ResponseStatus.Success) ~ ("result" -> result)
  }

  /**
   * Returns a reply indicating that nothing was found at the `uri` and `pos`.
   */
  private def mkNotFound(uri: String, pos: Position): JObject =
    ("status" -> ResponseStatus.InvalidRequest) ~ ("message" -> s"Nothing found in '$uri' at '$pos'.")

}
