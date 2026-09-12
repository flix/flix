/*
 * Copyright 2024 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.lsp.Range

object ExprSnippetCompleter {
  def generateDefaultHandlerSnippet(label: String, range: Range) : Completion =
    Completion.SnippetCompletion(label, range, Priority.Low(0),
      """@DefaultHandler
        |pub def runWithIO(f: Unit -> a \\ ef): a \ (ef - ${1:Eff}) + IO =
        |    run {
        |        f()
        |    } with ${0:?HandlerForEff}""".stripMargin,
      "snippet for a default handler for Eff")

  def getCompletions(range: Range): Iterable[Completion] = List(
    // NB: Please keep the list alphabetically sorted.
    generateDefaultHandlerSnippet("default handler", range),
    Completion.SnippetCompletion("main", range, Priority.High(0),
      "def main(): Unit \\ IO = \n    println(\"Hello World!\")",
      "snippet for Hello World Program"),
  )

}
