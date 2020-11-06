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

import ca.uwaterloo.flix.api.lsp.{Entity, Index, LocationLink, Position}
import ca.uwaterloo.flix.language.ast.TypeConstructor
import ca.uwaterloo.flix.language.ast.TypedAst.{Expression, Pattern, Root}
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL._

object GotoProvider {

  /**
    * Processes a goto request.
    */
  def processGoto(uri: String, pos: Position)(implicit index: Index, root: Root): JObject = {
    index.query(uri, pos) match {
      case None => mkNotFound(uri, pos)

      case Some(entity) => entity match {

        case Entity.Exp(exp) => exp match {
          case Expression.Def(sym, _, loc) =>
            ("status" -> "success") ~ ("result" -> LocationLink.fromDefSym(sym, root, loc).toJSON)

          case Expression.Var(sym, _, loc) =>
            ("status" -> "success") ~ ("result" -> LocationLink.fromVarSym(sym, loc).toJSON)

          case Expression.Tag(sym, tag, _, _, _, _) =>
            ("status" -> "success") ~ ("result" -> LocationLink.fromEnumAndTag(sym, tag, root, tag.loc).toJSON)

          case _ => mkNotFound(uri, pos)
        }

        case Entity.Pattern(pat) => pat match {
          case Pattern.Tag(sym, tag, _, _, _) =>
            ("status" -> "success") ~ ("result" -> LocationLink.fromEnumAndTag(sym, tag, root, tag.loc).toJSON)

          case _ => mkNotFound(uri, pos)
        }

        case Entity.TypeCon(tc, loc) => tc match {
          case TypeConstructor.Enum(sym, _) =>
            ("status" -> "success") ~ ("result" -> LocationLink.fromEnumSym(sym, root, loc).toJSON)

          case _ => mkNotFound(uri, pos)
        }

        case _ => mkNotFound(uri, pos)
      }
    }
  }

  /**
    * Returns a reply indicating that nothing was found at the `uri` and `pos`.
    */
  private def mkNotFound(uri: String, pos: Position): JObject =
    ("status" -> "failure") ~ ("message" -> s"Nothing found in '$uri' at '$pos'.")

}
