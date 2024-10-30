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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.{Entity, Index, InlayHint, InlayHintKind, Position, Range}
import ca.uwaterloo.flix.language.ast.Ast.TypeSource
import ca.uwaterloo.flix.language.ast.TypedAst.FormalParam
import ca.uwaterloo.flix.language.ast.{Type, TypedAst}
import ca.uwaterloo.flix.language.fmt.FormatType

object InlayHintProvider {

  /**
    * Returns the inlay hints in the given `uri` in the given `range`.
    */
  def processInlayHints(uri: String, range: Range)(implicit index: Index, flix: Flix): List[InlayHint] = {
    index.queryByRange(uri, range) match {
      case Nil => Nil
      case entities => entities.flatMap {
        case Entity.FormalParam(fparam) => getFormalParamHint(fparam)
        case _ => None
      }
    }
  }

  /**
    * Returns an inlay hint for the given formal param `fparam`.
    */
  private def getFormalParamHint(fparam: TypedAst.FormalParam)(implicit flix: Flix): Option[InlayHint] = fparam match {
    case FormalParam(sym, _, tpe, src, loc) => src match {
      case TypeSource.Ascribed =>
        // We do not show any inlay hint if the type is already there.
        None

      case TypeSource.Inferred =>
        val pos = Position.fromEnd(fparam.loc)
        val label = ": " + FormatType.formatType(tpe)

        // Hide long inlay hints.
        if (isTypeVar(tpe))
          None
        else
          Some(InlayHint(pos, abbreviate(label, 14), Some(InlayHintKind.Type), Nil, ""))
    }
  }

  /**
    * Returns `true` if the given type `tpe` is a type variable.
    */
  private def isTypeVar(tpe: Type): Boolean = tpe match {
    case Type.Var(_, _) => true
    case _ => false
  }

  /**
    * Returns `s` if it less than or equal to `l` chars.
    *
    * Otherwise returns a prefix of `s` with …
    */
  private def abbreviate(s: String, l: Int): String =
    if (s.length <= l)
      s
    else
      s.substring(0, l - 1) + "…"

}
