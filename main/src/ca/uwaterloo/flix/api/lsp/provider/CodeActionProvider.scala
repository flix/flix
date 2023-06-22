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
