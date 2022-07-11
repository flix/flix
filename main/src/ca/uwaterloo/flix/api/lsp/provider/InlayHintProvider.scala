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

import ca.uwaterloo.flix.api.lsp.Entity
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.api.lsp.{Index, InlayHint, InlayHintKind, Position, Range}
import ca.uwaterloo.flix.language.ast.{Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.fmt.{Audience, FormatType}

object InlayHintProvider {

  /**
    * Returns the inlay hints in the given `uri` in the given `range`.
    */
  def processInlayHints(uri: String, range: Range)(implicit index: Index, root: Root): List[InlayHint] = {
    index.queryByRange(uri, range) match {
      case Nil => Nil
      case entities => entities.flatMap {
        // case Entity.LocalVar(sym, tpe) => getLocalVarHint(sym, tpe) // TODO: Disabled until we figure out a way to not overwhelm the user.
        case Entity.FormalParam(fparam) => getFormalParamHint(fparam)
        case _ => None
      }
    }
  }

  /**
    * Returns an inlay hint for the given formal param `fparam`.
    */
  private def getFormalParamHint(fparam: TypedAst.FormalParam): Option[InlayHint] = {
    val pos = Position.fromEnd(fparam.loc)
    val label = ": " + FormatType.formatWellKindedType(fparam.tpe)(Audience.External)
    Some(InlayHint(pos, label, Some(InlayHintKind.Type), Nil, ""))
  }

  /**
    * Returns an inlay hint for the local var  type `tpe` at the given source location `loc`.
    */
  private def getLocalVarHint(sym: Symbol.VarSym, tpe: Type): Option[InlayHint] = {
    val pos = Position.fromEnd(sym.loc)
    val label = ": " + FormatType.formatWellKindedType(tpe)(Audience.External)
    Some(InlayHint(pos, label, Some(InlayHintKind.Type), Nil, ""))
  }

}
