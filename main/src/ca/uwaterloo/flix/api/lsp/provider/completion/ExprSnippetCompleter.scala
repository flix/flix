/*
 * Copyright 2024 Magnus Madsen
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
