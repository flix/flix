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
import ca.uwaterloo.flix.api.lsp.{MarkupContent, MarkupKind, Position, Range, ResponseStatus, StackConsumer, Visitor}
import ca.uwaterloo.flix.language.ast.Symbol.StructFieldSym
import ca.uwaterloo.flix.language.ast.TypedAst.*
import ca.uwaterloo.flix.language.ast.shared.Modifier
import ca.uwaterloo.flix.language.ast.shared.SymUse.{DefSymUse, OpSymUse, SigSymUse, StructFieldSymUse}
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.fmt.*
import ca.uwaterloo.flix.language.phase.unification.SetFormula
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL.*

object HoverProvider {

  def processHover(uri: String, pos: Position)(implicit root: Root, flix: Flix): JObject = {
    val consumer = StackConsumer()
    Visitor.visitRoot(root, consumer, Visitor.InsideAcceptor(uri, pos))

    consumer.getHead match {
      case None => mkNotFound(uri, pos)
      case Some(head) => hoverAny(head, uri, pos)
    }
  }

  private def hoverAny(x: AnyRef, uri: String, pos: Position)(implicit root: Root, flix: Flix): JObject = x match {
    case tpe: Type => hoverKind(tpe)
    case exp: Expr => hoverTypeAndEff(exp.tpe, exp.eff, exp.loc)
    case DefSymUse(sym, loc) => hoverDef(sym, loc)
    case SigSymUse(sym, loc) => hoverSig(sym, loc)
    case OpSymUse(symUse, loc) => hoverOp(symUse, loc)
    case StructFieldSymUse(sym, loc) => hoverStructFieldSymUse(sym, loc)
    case _ => mkNotFound(uri, pos)
  }

  private def hoverStructFieldSymUse(sym: StructFieldSym, loc: SourceLocation)(implicit root: Root, flix: Flix) = {
    val structOpt = root.structs.get(sym.structSym)

    structOpt match {
      case None => ("status" -> ResponseStatus.InvalidRequest) ~ ("message" -> s"struct `${sym.structSym.text}` is malformed")
      case Some(struct) =>
        struct.fields.get(sym) match {
          case None => ("status" -> ResponseStatus.InvalidRequest) ~ ("message" -> s"struct `${sym.structSym.text}` is malformed")
          case Some(field) =>
            val mods = field.mod.mod.flatMap {
              case Modifier.Mutable => Some("mut")
              case Modifier.Public => Some("pub")
              case _ => None
            }.foldRight(""){case (mod, acc) => s"$mod $acc"}
            val markup =
              s"""```flix
                 |${struct.sym.name}->$mods${sym.name}: ${FormatType.formatType(field.tpe)}
                 |```
                 |""".stripMargin
            val contents = MarkupContent(MarkupKind.Markdown, markup)
            val range = Range.from(loc)
            val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
            ("status" -> ResponseStatus.Success) ~ ("result" -> result)
        }
    }
  }

  private def hoverType(tpe: Type, loc: SourceLocation)(implicit root: Root, flix: Flix): JObject = {
    val lowerAndUpperBounds = SetFormula.formatLowerAndUpperBounds(tpe)(root)
    val markup =
      s"""```flix
         |${FormatType.formatType(tpe)}$lowerAndUpperBounds
         |```
         |""".stripMargin
    val contents = MarkupContent(MarkupKind.Markdown, markup)
    val range = Range.from(loc)
    val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
    ("status" -> ResponseStatus.Success) ~ ("result" -> result)
  }

  private def hoverTypeAndEff(tpe: Type, eff: Type, loc: SourceLocation)(implicit flix: Flix): JObject = {
    val markup =
      s"""```flix
         |${formatTypAndEff(tpe, eff)}
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
