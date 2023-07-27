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

  def processHover(uri: String, pos: Position, current: Boolean)(implicit index: Index, root: Option[Root], flix: Flix): JObject = {
    index.query(uri, pos) match {
      case None => mkNotFound(uri, pos)

      case Some(entity) => hoverEntity(entity, uri, pos, current)
    }
  }

  @tailrec
  private def hoverEntity(entity: Entity, uri: String, pos: Position, current: Boolean)(implicit Index: Index, root: Option[Root], flix: Flix): JObject = entity match {

    case Entity.Case(caze) => hoverType(caze.tpe, caze.sym.loc, current).getOrElse(mkNotFound(uri, pos))

    case Entity.DefUse(sym, loc, _) => hoverDef(sym, loc, current).getOrElse(mkNotFound(uri, pos))

    case Entity.SigUse(sym, loc, _) => hoverSig(sym, loc, current).getOrElse(mkNotFound(uri, pos))

    case Entity.VarUse(_, _, parent) => hoverEntity(parent, uri, pos, current)

    case Entity.CaseUse(_, _, parent) => hoverEntity(parent, uri, pos, current)

    case Entity.Exp(exp) => hoverTypeAndEff(exp.tpe, exp.eff, exp.loc, current).getOrElse(mkNotFound(uri, pos))

    case Entity.FormalParam(fparam) => hoverType(fparam.tpe, fparam.loc, current).getOrElse(mkNotFound(uri, pos))

    case Entity.Pattern(pat) => hoverType(pat.tpe, pat.loc, current).getOrElse(mkNotFound(uri, pos))

    case Entity.Pred(pred, tpe) => hoverType(tpe, pred.loc, current).getOrElse(mkNotFound(uri, pos))

    case Entity.LocalVar(sym, tpe) => hoverType(tpe, sym.loc, current).getOrElse(mkNotFound(uri, pos))

    case Entity.Type(t) => hoverKind(t, current)

    case Entity.OpUse(sym, loc, _) => hoverOp(sym, loc, current).getOrElse(mkNotFound(uri, pos))

    case Entity.Class(_) => mkNotFound(uri, pos)
    case Entity.Def(_) => mkNotFound(uri, pos)
    case Entity.Effect(_) => mkNotFound(uri, pos)
    case Entity.Enum(_) => mkNotFound(uri, pos)
    case Entity.TypeAlias(_) => mkNotFound(uri, pos)
    case Entity.AssocType(_) => mkNotFound(uri, pos)
    case Entity.Field(_) => mkNotFound(uri, pos)
    case Entity.Op(_) => mkNotFound(uri, pos)
    case Entity.Sig(_) => mkNotFound(uri, pos)
    case Entity.TypeVar(_) => mkNotFound(uri, pos)
  }

  private def hoverType(tpe: Type, loc: SourceLocation, current: Boolean)(implicit index: Index, root: Option[Root], flix: Flix): Option[JObject] = {
    root.map {
      case r =>
        val minTpe = minimizeType(tpe)
        val lowerAndUpperBounds = SetFormula.formatLowerAndUpperBounds(minTpe)(r)
        val markup =
          s"""${mkCurrentMsg(current)}
             |```flix
             |${FormatType.formatType(minTpe)}$lowerAndUpperBounds
             |```
             |""".stripMargin
        val contents = MarkupContent(MarkupKind.Markdown, markup)
        val range = Range.from(loc)
        val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
        ("status" -> ResponseStatus.Success) ~ ("result" -> result)
    }
  }

  private def hoverTypeAndEff(tpe: Type, eff: Type, loc: SourceLocation, current: Boolean)(implicit index: Index, root: Option[Root], flix: Flix): Option[JObject] = {
    root.map {
      case r =>
        val minEff = minimizeType(eff)
        val minTpe = minimizeType(tpe)
        val lowerAndUpperBounds = SetFormula.formatLowerAndUpperBounds(minTpe)(r)
        val markup =
          s"""${mkCurrentMsg(current)}
             |```flix
             |${formatTypAndEff(minTpe, minEff)}$lowerAndUpperBounds
             |```
             |""".stripMargin
        val contents = MarkupContent(MarkupKind.Markdown, markup)
        val range = Range.from(loc)
        val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
        ("status" -> ResponseStatus.Success) ~ ("result" -> result)
    }
  }

  private def hoverDef(sym: Symbol.DefnSym, loc: SourceLocation, current: Boolean)(implicit index: Index, root: Option[Root], flix: Flix): Option[JObject] = {
    root.map {
      case r =>
        val defDecl = r.defs(sym)
        val markup =
          s"""${mkCurrentMsg(current)}
             |```flix
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
  }

  private def hoverSig(sym: Symbol.SigSym, loc: SourceLocation, current: Boolean)(implicit index: Index, root: Option[Root], flix: Flix): Option[JObject] = {
    root.map {
      case r =>
        val sigDecl = r.sigs(sym)
        val markup =
          s"""${mkCurrentMsg(current)}
             |```flix
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
  }

  private def hoverOp(sym: Symbol.OpSym, loc: SourceLocation, current: Boolean)(implicit index: Index, root: Option[Root], flix: Flix): Option[JObject] = {
    root.map {
      case r =>
        val opDecl = r.effects(sym.eff).ops.find(_.sym == sym).get // guaranteed to be present
        val markup =
          s"""${mkCurrentMsg(current)}
             |```flix
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
  }

  private def formatTypAndEff(tpe0: Type, eff0: Type)(implicit flix: Flix): String = {
    // TODO deduplicate with CompletionProvider
    val t = FormatType.formatType(tpe0)

    // don't show purity if bool effects are turned off
    val p = if (flix.options.xnobooleffects) {
      ""
    } else {
      eff0 match {
        case Type.Cst(TypeConstructor.Pure, _) => ""
        case Type.Cst(TypeConstructor.EffUniv, _) => raw" \ IO"
        case eff => raw" \ " + FormatType.formatType(eff)
      }
    }

    s"$t$p"
  }

  private def hoverKind(t: Type, current: Boolean)(implicit index: Index, root: Option[Root]): JObject = {
    val markup =
      s"""${mkCurrentMsg(current)}
         |```flix
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

  private def mkCurrentMsg(current: Boolean): String =
    if (!current) "(Information may not be current)" else ""

}
