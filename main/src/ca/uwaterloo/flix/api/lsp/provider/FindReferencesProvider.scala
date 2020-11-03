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

import ca.uwaterloo.flix.api.lsp.{Entity, Index, Location, Position}
import ca.uwaterloo.flix.language.ast.TypedAst.{Expression, Pattern, Root}
import ca.uwaterloo.flix.language.ast.{Name, Symbol}
import org.json4s.JsonAST.JValue
import org.json4s.JsonDSL._

object FindReferencesProvider {

  def findReferences(requestId: String, uri: String, pos: Position)(implicit index: Index, root: Root): JValue = {
    index.query(uri, pos) match {
      case None => mkNotFound(requestId, uri, pos)
      case Some(entity) => entity match {

        case Entity.Case(caze) => findTag(requestId, caze.sym, caze.tag)

        case Entity.Def(defn) =>
          val uses = index.usesOf(defn.sym)
          val locs = uses.toList.map(Location.from)
          ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> locs.map(_.toJSON))

        case Entity.Exp(exp) => exp match {
          case Expression.Def(sym, _, _) =>
            val uses = index.usesOf(sym)
            val locs = uses.toList.map(Location.from)
            ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> locs.map(_.toJSON))

          case Expression.Var(sym, _, _) =>
            val uses = index.usesOf(sym)
            val locs = uses.toList.map(Location.from)
            ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> locs.map(_.toJSON))

          case Expression.Tag(sym, tag, _, _, _, _) => findTag(requestId, sym, tag)

          case _ => mkNotFound(requestId, uri, pos)
        }

        case Entity.FormalParam(param) =>
          val uses = index.usesOf(param.sym)
          val locs = uses.toList.map(Location.from)
          ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> locs.map(_.toJSON))

        case Entity.Pattern(pat) => pat match {
          case Pattern.Var(sym, _, _) =>
            val uses = index.usesOf(sym)
            val locs = uses.toList.map(Location.from)
            ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> locs.map(_.toJSON))

          case Pattern.Tag(sym, tag, _, _, _) =>
            val uses = index.usesOf(sym, tag)
            val locs = uses.toList.map(Location.from)
            ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> locs.map(_.toJSON))

          case _ => mkNotFound(requestId, uri, pos)
        }

        case Entity.Pred(pred) =>
          val defs = index.defsOf(pred)
          val uses = index.usesOf(pred)
          val locs = (defs ++ uses).toList.map(Location.from)
          ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> locs.map(_.toJSON))

        case Entity.LocalVar(sym, _) =>
          val uses = index.usesOf(sym)
          val locs = uses.toList.map(Location.from)
          ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> locs.map(_.toJSON))

      }
    }
  }

  private def findTag(requestId: String, sym: Symbol.EnumSym, tag: Name.Tag)(implicit index: Index, root: Root): JValue = {
    val uses = index.usesOf(sym, tag)
    val locs = uses.toList.map(Location.from)
    ("id" -> requestId) ~ ("status" -> "success") ~ ("result" -> locs.map(_.toJSON))
  }

  /**
    * Returns a reply indicating that nothing was found at the `uri` and `pos`.
    */
  private def mkNotFound(requestId: String, uri: String, pos: Position): JValue =
    ("id" -> requestId) ~ ("status" -> "failure") ~ ("message" -> s"Nothing found in '$uri' at '$pos'.")

}
