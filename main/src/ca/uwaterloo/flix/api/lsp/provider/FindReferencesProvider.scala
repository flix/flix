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

import ca.uwaterloo.flix.api.lsp.acceptors.{AllAcceptor, InsideAcceptor}
import ca.uwaterloo.flix.api.lsp.consumers.StackConsumer
import ca.uwaterloo.flix.api.lsp.*
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.shared.SymUse
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL.*

object FindReferencesProvider {

  def findRefs(uri: String, pos: Position)(implicit root: Root): JObject = {
    val left = searchLeftOfCursor(uri, pos)
    val right = searchRightOfCursor(uri, pos)

    right
      .orElse(left)
      .flatMap(getOccurs)
      .map(mkResponse)
      .getOrElse(mkNotFound(uri, pos))
  }

  private def searchLeftOfCursor(uri: String, pos: Position)(implicit root: Root): Option[AnyRef] = {
    if (pos.character >= 2) {
      val left = Position(pos.line, pos.character - 1)
      search(uri, left)
    } else {
      search(uri, pos)
    }
  }

  private def searchRightOfCursor(uri: String, pos: Position)(implicit root: Root): Option[AnyRef] = search(uri, pos)

  private def search(uri: String, pos: Position)(implicit root: Root): Option[AnyRef] = {
    val consumer = StackConsumer()
    Visitor.visitRoot(root, consumer, InsideAcceptor(uri, pos))
    consumer.getStack.headOption
  }

  private def getOccurs(x: AnyRef)(implicit root: Root): Option[Set[SourceLocation]] = x match {
    // Defs
    case TypedAst.Def(sym, _, _, _) => Some(getDefnSymOccurs(sym))
    case SymUse.DefSymUse(sym, _) => Some(getDefnSymOccurs(sym))
    // Enums
    case TypedAst.Enum(_, _, _, sym, _, _, _, _) => Some(getEnumSymOccurs(sym))
    case Type.Cst(TypeConstructor.Enum(sym, _), _) => Some(getEnumSymOccurs(sym))
    case _ => None
  }

  private def getEnumSymOccurs(sym: Symbol.EnumSym)(implicit root: Root): Set[SourceLocation] = {
    var occurs: Set[SourceLocation] = Set.empty

    def consider(s: Symbol.EnumSym, loc: SourceLocation): Unit = {
      if (s == sym) { occurs += loc }
    }

    object EnumSymConsumer extends Consumer {
      override def consumeEnum(enm: TypedAst.Enum): Unit = consider(enm.sym, enm.sym.loc)
      override def consumeType(tpe: Type): Unit = tpe match {
        case Type.Cst(TypeConstructor.Enum(sym, _), loc) => consider(sym, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, EnumSymConsumer, AllAcceptor)

    occurs
  }

  private def getDefnSymOccurs(sym: Symbol.DefnSym)(implicit root: Root): Set[SourceLocation] = {
    var occurs: Set[SourceLocation] = Set.empty

    def consider(s: Symbol.DefnSym, loc: SourceLocation): Unit = {
      if (s == sym) { occurs += loc }
    }

    object DefnSymConsumer extends Consumer {
      override def consumeDef(defn: TypedAst.Def): Unit = consider(defn.sym, defn.sym.loc)
      override def consumeDefSymUse(sym: SymUse.DefSymUse): Unit = consider(sym.sym, sym.loc)
    }

    Visitor.visitRoot(root, DefnSymConsumer, AllAcceptor)

    occurs
  }

  private def mkResponse(refs: Set[SourceLocation]): JObject = {
    ("status" -> ResponseStatus.Success) ~ ("result" -> refs.map(Location.from).map(_.toJSON))
  }

  /**
    * Returns a reply indicating that nothing was found at the `uri` and `pos`.
    */
  private def mkNotFound(uri: String, pos: Position): JObject =
    ("status" -> ResponseStatus.InvalidRequest) ~ ("message" -> s"Nothing found in '$uri' at '$pos'.")

}
