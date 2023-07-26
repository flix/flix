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

import ca.uwaterloo.flix.api.lsp.{Entity, Index, LocationLink, Position, ResponseStatus}
import ca.uwaterloo.flix.language.ast.TypedAst.{Pattern, Root}
import ca.uwaterloo.flix.language.ast.{Ast, Type, TypeConstructor}
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL._

object GotoProvider {

  /**
    * Processes a goto request.
    */
  def processGoto(uri: String, pos: Position)(implicit index: Index, root: Option[Root]): JObject = root match {
    case None => mkNotFound(uri, pos)
    case Some(someRoot) =>
      index.query(uri, pos) match {
        case None => mkNotFound(uri, pos)

        case Some(entity) => entity match {

          case Entity.DefUse(sym, loc, _) =>
            ("status" -> ResponseStatus.Success) ~ ("result" -> LocationLink.fromDefSym(sym, loc)(someRoot).toJSON)

          case Entity.SigUse(sym, loc, _) =>
            ("status" -> ResponseStatus.Success) ~ ("result" -> LocationLink.fromSigSym(sym, loc)(someRoot).toJSON)

          case Entity.VarUse(sym, loc, _) =>
            ("status" -> ResponseStatus.Success) ~ ("result" -> LocationLink.fromVarSym(sym, loc).toJSON)

          case Entity.CaseUse(sym, loc, _) =>
            ("status" -> ResponseStatus.Success) ~ ("result" -> LocationLink.fromCaseSym(sym, loc)(someRoot).toJSON)

          case Entity.Exp(_) => mkNotFound(uri, pos)

          case Entity.Pattern(pat) => pat match {
            case Pattern.Tag(Ast.CaseSymUse(sym, loc), _, _, _) =>
              ("status" -> ResponseStatus.Success) ~ ("result" -> LocationLink.fromCaseSym(sym, loc)(someRoot).toJSON)

            case _ => mkNotFound(uri, pos)
          }

          case Entity.Type(t) => t match {
            case Type.Cst(TypeConstructor.Enum(sym, _), loc) =>
              ("status" -> ResponseStatus.Success) ~ ("result" -> LocationLink.fromEnumSym(sym, loc)(someRoot).toJSON)

            case Type.Cst(TypeConstructor.Effect(sym), loc) =>
              ("status" -> ResponseStatus.Success) ~ ("result" -> LocationLink.fromEffectSym(sym, loc).toJSON)

            case Type.Var(sym, loc) =>
              ("status" -> ResponseStatus.Success) ~ ("result" -> LocationLink.fromTypeVarSym(sym, loc).toJSON)

            case _ => mkNotFound(uri, pos)
          }

          case Entity.OpUse(sym, loc, _) =>
            ("status" -> ResponseStatus.Success) ~ ("result" -> LocationLink.fromOpSym(sym, loc).toJSON)

          case Entity.Case(_) => mkNotFound(uri, pos)
          case Entity.Class(_) => mkNotFound(uri, pos)
          case Entity.Def(_) => mkNotFound(uri, pos)
          case Entity.Effect(_) => mkNotFound(uri, pos)
          case Entity.Enum(_) => mkNotFound(uri, pos)
          case Entity.TypeAlias(_) => mkNotFound(uri, pos)
          case Entity.AssocType(_) => mkNotFound(uri, pos)
          case Entity.Field(_) => mkNotFound(uri, pos)
          case Entity.FormalParam(_) => mkNotFound(uri, pos)
          case Entity.LocalVar(_, _) => mkNotFound(uri, pos)
          case Entity.Op(_) => mkNotFound(uri, pos)
          case Entity.Pred(_, _) => mkNotFound(uri, pos)
          case Entity.Sig(_) => mkNotFound(uri, pos)
          case Entity.TypeVar(_) => mkNotFound(uri, pos)
        }
      }
  }

  /**
    * Returns a reply indicating that nothing was found at the `uri` and `pos`.
    */
  private def mkNotFound(uri: String, pos: Position): JObject =
    ("status" -> ResponseStatus.InvalidRequest) ~ ("message" -> s"Nothing found in '$uri' at '$pos'.")

}
