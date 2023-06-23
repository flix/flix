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

import ca.uwaterloo.flix.api.lsp.{CodeAction, CodeActionContext, Range}

object CodeActionProvider {

  /**
    * Return all available code actions
    */
  def getCodeActions(uri: String, range: Range, context: CodeActionContext): List[CodeAction] = {
    // All received document positions are 1-based, but all returned positions should be 0-based

    // Example - Inserting 'Hello Code Actions!' at the beginning of the current document:
    //
    //    List(CodeAction(
    //      title = "Hello Code Actions!",
    //      kind = CodeActionKind.QuickFix,
    //      edit = Some(WorkspaceEdit(
    //        Map(uri -> List(TextEdit(
    //          Range(Position(0, 0), Position(0, 0)),
    //          "Hello Code Actions!"
    //        )))
    //      )),
    //      command = None
    //    ))
    //

    List()
  }
}
