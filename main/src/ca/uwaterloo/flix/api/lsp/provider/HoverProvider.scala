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
import ca.uwaterloo.flix.language.ast.TypedAst.{Expression, Pattern, Root}
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps
import ca.uwaterloo.flix.language.ast.{Ast, Type, TypeConstructor}
import ca.uwaterloo.flix.language.debug._
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL._

object HoverProvider {

  def processHover(uri: String, pos: Position)(implicit index: Index, root: Root): JObject = {

    implicit val audience: Audience = Audience.External

    def formatTypAndEff(tpe0: Type, eff0: Type): String = {
      val t = FormatType.formatType(tpe0)
      eff0 match {
        case Type.Cst(TypeConstructor.True) => t
        case Type.Cst(TypeConstructor.False) => s"$t & Impure"
        case eff => s"$t & ${FormatType.formatType(eff)}"
      }
    }

    def formatStratification(stf: Ast.Stratification): String = {
      val sb = new StringBuilder()
      val max = stf.m.values.max
      for (i <- 0 to max) {
        sb.append("Stratum ").append(i).append(":").append("\r\n")
        for ((sym, stratum) <- stf.m) {
          if (i == stratum) {
            sb.append("  ").append(sym).append("\r\n")
          }
        }
      }
      sb.toString()
    }

    index.query(uri, pos) match {
      case Some(Entity.Exp(exp)) =>
        exp match {
          case Expression.Def(sym, _, _) =>
            val decl = root.defs(sym)
            val markup =
              s"""${FormatSignature.asMarkDown(decl)}
                 |
                 |${FormatDoc.asMarkDown(decl.doc)}
                 |""".stripMargin
            val contents = MarkupContent(MarkupKind.Markdown, markup)
            val range = Range.from(exp.loc)
            val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
            ("status" -> "success") ~ ("result" -> result)

          case Expression.Hole(sym, _, _, _) =>
            val holes = TypedAstOps.holesOf(root)
            val holeContext = holes(sym)
            val env = holeContext.env.map {
              case (localSym, localTpe) => s"${localSym.text}: ${FormatType.formatType(localTpe)}"
            }
            val markup =
              s"""Hole: ${sym.name}
                 |
                 |Context:
                 |  ${holeContext.env.mkString("\r\n  ")}
                 |""".stripMargin
            val contents = MarkupContent(MarkupKind.Markdown, markup)
            val range = Range.from(exp.loc)
            val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
            ("status" -> "success") ~ ("result" -> result)

          case Expression.Tag(sym, tag, _, _, _, _) =>
            val decl = root.enums(sym)
            val caze = decl.cases(tag)
            val markup =
              s"""${FormatCase.asMarkDown(caze)}
                 |
                 |${FormatDoc.asMarkDown(decl.doc)}
                 |""".stripMargin
            val contents = MarkupContent(MarkupKind.Markdown, markup)
            val range = Range.from(exp.loc)
            val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
            ("status" -> "success") ~ ("result" -> result)

          case Expression.FixpointConstraintSet(_, stf, _, _) =>
            val markup =
              s"""${formatTypAndEff(exp.tpe, exp.eff)}
                 |
                 |${formatStratification(stf)}
                 |""".stripMargin
            val contents = MarkupContent(MarkupKind.Markdown, markup)
            val range = Range.from(exp.loc)
            val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
            ("status" -> "success") ~ ("result" -> result)

          case Expression.FixpointCompose(_, _, stf, _, _, _) =>
            val markup =
              s"""${formatTypAndEff(exp.tpe, exp.eff)}
                 |
                 |${formatStratification(stf)}
                 |""".stripMargin
            val contents = MarkupContent(MarkupKind.Markdown, markup)
            val range = Range.from(exp.loc)
            val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
            ("status" -> "success") ~ ("result" -> result)

          case Expression.FixpointSolve(_, stf, _, _, _) =>
            val markup =
              s"""${formatTypAndEff(exp.tpe, exp.eff)}
                 |
                 |${formatStratification(stf)}
                 |""".stripMargin
            val contents = MarkupContent(MarkupKind.Markdown, markup)
            val range = Range.from(exp.loc)
            val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
            ("status" -> "success") ~ ("result" -> result)

          case _ =>
            //
            // Other Expression.
            //
            val markup = formatTypAndEff(exp.tpe, exp.eff)
            val contents = MarkupContent(MarkupKind.Markdown, markup)
            val range = Range.from(exp.loc)
            val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
            ("status" -> "success") ~ ("result" -> result)
        }

      case Some(Entity.Pattern(pat)) => pat match {
        case Pattern.Tag(sym, tag, _, _, _) =>
          val decl = root.enums(sym)
          val caze = decl.cases(tag)
          val markup =
            s"""${FormatCase.asMarkDown(caze)}
               |
               |${FormatDoc.asMarkDown(decl.doc)}
               |""".stripMargin
          val contents = MarkupContent(MarkupKind.Markdown, markup)
          val range = Range.from(pat.loc)
          val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
          ("status" -> "success") ~ ("result" -> result)

        case _ =>
          //
          // Other pattern.
          //
          val markup = FormatType.formatType(pat.tpe)
          val contents = MarkupContent(MarkupKind.Markdown, markup)
          val range = Range.from(pat.loc)
          val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
          ("status" -> "success") ~ ("result" -> result)
      }

      case Some(Entity.LocalVar(sym, tpe)) =>
        val markup = formatTypAndEff(tpe, Type.Pure)
        val contents = MarkupContent(MarkupKind.Markdown, markup)
        val range = Range.from(sym.loc)
        val result = ("contents" -> contents.toJSON) ~ ("range" -> range.toJSON)
        ("status" -> "success") ~ ("result" -> result)

      case _ =>
        mkNotFound(uri, pos)
    }
  }

  /**
    * Returns a reply indicating that nothing was found at the `uri` and `pos`.
    */
  private def mkNotFound(uri: String, pos: Position): JObject =
    ("status" -> "failure") ~ ("message" -> s"Nothing found in '$uri' at '$pos'.")

}
