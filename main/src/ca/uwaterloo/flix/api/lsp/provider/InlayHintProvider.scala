/*
 * Copyright 2022 Nicola Dardanis
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

import ca.uwaterloo.flix.api.lsp.Entity.LocalVar
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.api.lsp.{Index, InlayHint, InlayHintKind, Position, Range}
import ca.uwaterloo.flix.language.ast.{SourceLocation, Type, TypeConstructor}
import ca.uwaterloo.flix.language.fmt.{Audience, FormatType}

object InlayHintProvider {

  def processInlayHints(uri: String, range: Range)(implicit index: Index, root: Root): List[InlayHint] = {
    index.queryByRange(uri, range) match {
      case Nil => Nil
      case entities => entities.foldLeft(Nil: List[InlayHint]) {
        case (acc, LocalVar(sym, tpe)) => getTypeHint(sym.loc, tpe) match {
          case Some(hint) => hint :: acc
          case None => acc
        }
        case (acc, _) => acc
      }
    }
  }

  private def getTypeHint(loc: SourceLocation, tpe: Type): Option[InlayHint] = {
    if (tpe.loc.isSynthetic || tpe.loc == SourceLocation.Unknown) {
      val pos = Position(loc.endLine - 1, loc.endCol - 1)
      val label = ": " + FormatType.formatWellKindedType(tpe)(Audience.External)
      Some(InlayHint(pos, label, Some(InlayHintKind.Type), Nil, ""))
    } else {
      None
    }
  }

}
