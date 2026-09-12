/*
 * Copyright 2025 Din Jakupi
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp.provider

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.{FormattingOptions, Position, FormatterLsp, TextEdit, Range}

import scala.annotation.unused

object FormattingProvider {

  def formatDocument(uri: String, @unused options: FormattingOptions)(implicit @unused flix: Flix): List[TextEdit] = {
    val parsedAst = flix.getParsedAst
    FormatterLsp.format(parsedAst, uri)
  }
}
