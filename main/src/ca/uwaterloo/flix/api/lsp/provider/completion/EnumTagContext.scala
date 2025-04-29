/*
 * Copyright 2025 Chenhao Gao
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

package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.{LspUtil, Position}
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.TypedAst.Expr
import ca.uwaterloo.flix.language.ast.shared.SymUse

sealed trait EnumTagContext

object EnumTagContext {
  /**
    * Represents an expression in a complete enum context.
    *
    * For example, in `Clr.Red|` where Red is a valid case, we say that cursor position is `AfterCompleteEnumTag`.
    */
  case class AfterCompleteEnumTag(sym: SymUse.CaseSymUse) extends EnumTagContext

  /**
    * Represents an expression in all other contexts.
    */
  case object Unknown extends EnumTagContext

  /**
    * Returns the context of the enum tag at the given position.
    * Only a CaseSymUse followed by a Tag expression is considered AfterCompleteEnumTag context.
    */
  def getEnumTagContext(uri: String, pos: Position)(implicit root: TypedAst.Root, flix: Flix): EnumTagContext = {
    LspUtil.getStack(uri, pos) match {
      case (symUse@SymUse.CaseSymUse(_, _)) :: Expr.Tag(_, _, _, _, _) :: _ =>
        // The leaf is a case symbol followed by a Tag expression.
        EnumTagContext.AfterCompleteEnumTag(symUse)
      case _ => EnumTagContext.Unknown
    }
  }
}


