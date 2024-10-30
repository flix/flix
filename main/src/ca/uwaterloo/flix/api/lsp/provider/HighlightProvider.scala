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

import ca.uwaterloo.flix.api.lsp.{DocumentHighlight, DocumentHighlightKind, Entity, Index, Position, Range, ResponseStatus, StackConsumer, Visitor}
import ca.uwaterloo.flix.language.ast.TypedAst.{Pattern, Root}
import ca.uwaterloo.flix.language.ast.shared.SymUse.CaseSymUse
import ca.uwaterloo.flix.language.ast.{Ast, Name, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.jvm.JvmName.Exception
import org.json4s.JsonAST.{JArray, JObject}
import org.json4s.JsonDSL.*

object HighlightProvider {

  def processHighlight(uri: String, pos: Position)(implicit root: Root): JObject = {
    val stackConsumer = StackConsumer()
    Visitor.visitRoot(root, stackConsumer, Visitor.InsideAcceptor(uri, pos))

    val highlights = for {
      sym <- stackConsumer.getStack.headOption.map { case sym: Symbol => sym }

      occurConsumer = SymbolOccurrenceConsumer(sym)
      acceptor = Visitor.FileAcceptor(uri)

      _ = Visitor.visitRoot(root, occurConsumer, acceptor)

      writeLoc <- getSymLoc(sym)

      write = DocumentHighlight(Range.from(writeLoc), DocumentHighlightKind.Write)
      reads = occurConsumer.occurances.flatMap(
        occur => getSymLoc(occur) match {
          case None => None
          case Some(loc) => Some(DocumentHighlight(Range.from(loc), DocumentHighlightKind.Read))
        }
      )

      highlights = write :: reads

    } yield highlights

    highlights match {
      case None => mkNotFound(uri, pos)
      case Some(highlights) => ("status" -> ResponseStatus.Success) ~ ("result" -> JArray(highlights.map(_.toJSON)))
    }
  }

  private def getSymLoc(sym: Symbol): Option[SourceLocation] = sym match {
    case sym: Symbol.VarSym => Some(sym.loc)
    case sym: Symbol.KindedTypeVarSym => Some(sym.loc)
    case sym: Symbol.UnkindedTypeVarSym => Some(sym.loc)
    case sym: Symbol.DefnSym => Some(sym.loc)
    case sym: Symbol.EnumSym => Some(sym.loc)
    case sym: Symbol.StructSym => Some(sym.loc)
    case sym: Symbol.RestrictableEnumSym => Some(sym.loc)
    case sym: Symbol.CaseSym => Some(sym.loc)
    case sym: Symbol.StructFieldSym => Some(sym.loc)
    case sym: Symbol.RestrictableCaseSym => Some(sym.loc)
    case sym: Symbol.TraitSym => Some(sym.loc)
    case sym: Symbol.SigSym => Some(sym.loc)
    case _: Symbol.LabelSym => None
    case sym: Symbol.HoleSym => Some(sym.loc)
    case sym: Symbol.TypeAliasSym => Some(sym.loc)
    case sym: Symbol.AssocTypeSym => Some(sym.loc)
    case sym: Symbol.EffectSym => Some(sym.loc)
    case sym: Symbol.OpSym => Some(sym.loc)
    case _: Symbol.ModuleSym => None
  }

  /**
    * Returns a reply indicating that nothing was found at the `uri` and `pos`.
    */
  private def mkNotFound(uri: String, pos: Position): JObject =
    ("status" -> ResponseStatus.InvalidRequest) ~ ("message" -> s"Nothing found in '$uri' at '$pos'.")

}
