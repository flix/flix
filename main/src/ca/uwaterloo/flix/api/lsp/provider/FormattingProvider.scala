/*
 * Copyright 2025 Din Jakupi
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
import ca.uwaterloo.flix.api.lsp.{FormattingOptions, Position, Formatter, TextEdit, Range}

import scala.annotation.unused

object FormattingProvider {

  def formatDocument(uri: String, @unused options: FormattingOptions)(implicit @unused flix: Flix): List[TextEdit] = {
    val parsedAst = flix.getParsedAst
    val formattedSource = Formatter.format(parsedAst, uri)
    formattedSource
  }
}
