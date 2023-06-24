/*
 * Copyright 2023 Holger Dal Mogensen
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
import ca.uwaterloo.flix.api.lsp.{CodeAction, CodeActionContext, CodeActionKind, Entity, Index, Position, Range, TextEdit, WorkspaceEdit}
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.TypedAst.Root

object CodeActionProvider {

  def getCodeActions(uri: String, range: Range, context: CodeActionContext)(implicit index: Index, root: Option[Root], flix: Flix): List[CodeAction] = {
    println(s"uri = $uri, $range = $range, ctx = $context")


    index.query(uri, range.start) match {
      case None => Nil // No code actions.

      case Some(entity) => entity match {
        case Entity.Enum(e) =>
          val sym = e.sym
          List(mkDeriveEq(sym, uri), mkDeriveOrder(sym, uri), mkDeriveToString(sym, uri))
        case _ =>
          Nil // No code actions.
      }
    }
  }

  /**
    * A code action to derive the `Eq` type class.
    */
  private def mkDeriveEq(sym: Symbol.EnumSym, uri: String): CodeAction = mkDerive(sym, "Eq", uri)

  /**
    * A code action to derive the `Order` type class.
    */
  private def mkDeriveOrder(sym: Symbol.EnumSym, uri: String): CodeAction = mkDerive(sym, "Order", uri)

  /**
    * A code action to derive the `ToString` type class.
    */
  private def mkDeriveToString(sym: Symbol.EnumSym, uri: String): CodeAction = mkDerive(sym, "ToString", uri)

  // TODO: Add Hash, Sendable type classes.

  /**
    * A code action to derive the given type class `clazz` for the given enum symbol `sym`.
    */
  private def mkDerive(sym: Symbol.EnumSym, clazz: String, uri: String): CodeAction = CodeAction(
    title = s"Derive $clazz for $sym",
    kind = CodeActionKind.Refactor,
    edit = Some(WorkspaceEdit(
      Map(uri -> List(TextEdit(
        Range(Position.fromEnd(sym.loc), Position.fromEnd(sym.loc)),
        s" with $clazz"
      )))
    )),
    command = None
  )

}
