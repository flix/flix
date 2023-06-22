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
package ca.uwaterloo.flix.api.lsp

import org.json4s.JValue
import org.json4s.JsonDSL._

/**
  * Represents a `CodeAction` in LSP.
  *
  * A `CodeAction` should be associated with an edit and/or a command.
  *
  * @param title          A short, human-readable, title for this code action.
  * @param kind           The kind of the code action. Used to filter code actions.
  * @param diagnostic     The diagnostics that this code action resolves.
  * @param isPreferred    Marks this as a preferred action. Preferred actions are used by the
  *                       `auto fix` command and can be targeted by keybindings.
  * @param disabledReason If `Some`, marks that the code action cannot currently be applied.
  *                       It will appear greyed out with the supplied string displayed as the reason.
  * @param edit           The workspace edit this code action performs.
  * @param command        A command this code action executes. If a code action
  *                       provides an edit and a command, first the edit is
  *                       executed and then the command.
  */
case class CodeAction(title: String,
                      kind: CodeActionKind,
                      diagnostic: Option[Diagnostic] = None,
                      isPreferred: Boolean = false,
                      disabledReason: Option[String] = None,
                      edit: Option[WorkspaceEdit],
                      command: Option[Command]) {

  def toJSON: JValue =
    ("title" -> title) ~
      ("kind" -> kind.toJSON) ~
      ("diagnostic" -> diagnostic.map(_.toJSON)) ~
      ("isPreferred" -> isPreferred) ~
      ("disabled" -> disabledReason.map("reason" -> _)) ~
      ("edit" -> edit.map(_.toJSON)) ~
      ("command" -> command.map(_.toJSON))
}
