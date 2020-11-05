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
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL._

object FindReferencesProvider {

  def findRefs(uri: String, pos: Position)(implicit index: Index, root: Root): JObject = {
    index.query(uri, pos) match {
      case None => mkNotFound(uri, pos)
      case Some(entity) => entity match {

        case Entity.Case(caze) => findTagUses(caze.sym, caze.tag)

        case Entity.Def(defn) => findDefUses(defn.sym)

        case Entity.Enum(enum0) => findEnumUses(enum0.sym)

        case Entity.Exp(exp) => exp match {
          case Expression.Def(sym, _, _) => findDefUses(sym)
          case Expression.Var(sym, _, _) => findVarUses(sym)
          case Expression.Tag(sym, tag, _, _, _, _) => findTagUses(sym, tag)
          case _ => mkNotFound(uri, pos)
        }

        case Entity.Field(field) => findFieldUses(field)

        case Entity.FormalParam(param) => findVarUses(param.sym)

        case Entity.Pattern(pat) => pat match {
          case Pattern.Var(sym, _, _) => findVarUses(sym)
          case Pattern.Tag(sym, tag, _, _, _) => findTagUses(sym, tag)
          case _ => mkNotFound(uri, pos)
        }

        case Entity.Pred(pred) => findPredUses(pred)

        case Entity.LocalVar(sym, _) => findVarUses(sym)

        case _ => mkNotFound(uri, pos)

      }
    }
  }

  private def findDefUses(sym: Symbol.DefnSym)(implicit index: Index, root: Root): JObject = {
    val uses = index.usesOf(sym)
    val locs = uses.toList.map(Location.from)
    ("status" -> "success") ~ ("result" -> locs.map(_.toJSON))
  }

  private def findEnumUses(sym: Symbol.EnumSym)(implicit index: Index, root: Root): JObject = {
    val uses = index.usesOf(sym)
    val locs = uses.toList.map(Location.from)
    ("status" -> "success") ~ ("result" -> locs.map(_.toJSON))
  }

  private def findFieldUses(field: Name.Field)(implicit index: Index, root: Root): JObject = {
    val defs = index.defsOf(field)
    val uses = index.usesOf(field)
    val locs = (defs ++ uses).toList.map(Location.from)
    ("status" -> "success") ~ ("result" -> locs.map(_.toJSON))
  }

  private def findPredUses(pred: Name.Pred)(implicit index: Index, root: Root): JObject = {
    val defs = index.defsOf(pred)
    val uses = index.usesOf(pred)
    val locs = (defs ++ uses).toList.map(Location.from)
    ("status" -> "success") ~ ("result" -> locs.map(_.toJSON))
  }

  private def findTagUses(sym: Symbol.EnumSym, tag: Name.Tag)(implicit index: Index, root: Root): JObject = {
    val uses = index.usesOf(sym, tag)
    val locs = uses.toList.map(Location.from)
    ("status" -> "success") ~ ("result" -> locs.map(_.toJSON))
  }

  private def findVarUses(sym: Symbol.VarSym)(implicit index: Index, root: Root): JObject = {
    val uses = index.usesOf(sym)
    val locs = uses.toList.map(Location.from)
    ("status" -> "success") ~ ("result" -> locs.map(_.toJSON))
  }

  /**
    * Returns a reply indicating that nothing was found at the `uri` and `pos`.
    */
  private def mkNotFound(uri: String, pos: Position): JObject =
    ("status" -> "failure") ~ ("message" -> s"Nothing found in '$uri' at '$pos'.")

}
