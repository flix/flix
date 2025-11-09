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
package ca.uwaterloo.flix.api.lsp

import org.json4s.MonadicJValue.jvalueToMonadic
import org.json4s.JsonAST.{JBool, JInt, JValue}
import org.eclipse.lsp4j

/**
  * Represents `FormattingOptions` in LSP.
  *
  * @param tabSize                  Size of a tab in spaces.
  * @param insertSpaces             Prefer spaces over tabs.
  * @param trimTrailingWhitespace   Trim trailing whitespace on a line.
  * @param insertFinalNewline       Insert a newline character at the end of the file if one does not exist.
  * @param trimFinalNewlines        Trim all newlines after the final newline at the end of the file.
  */
case class FormattingOptions(tabSize: Int,
                              insertSpaces: Boolean,
                              trimTrailingWhitespace: Option[Boolean] = None,
                              insertFinalNewline: Option[Boolean] = None,
                              trimFinalNewlines: Option[Boolean] = None) {}

object FormattingOptions {
  def fromLsp4j(options: lsp4j.FormattingOptions): FormattingOptions = {
    FormattingOptions(
      tabSize = options.getTabSize,
      insertSpaces = options.isInsertSpaces,
      trimTrailingWhitespace = Some(options.isTrimTrailingWhitespace),
      insertFinalNewline = Some(options.isInsertFinalNewline),
      trimFinalNewlines = Some(options.isTrimFinalNewlines)
    )
  }

  def parse(jv: JValue): FormattingOptions = {
    val tabSize: Int = jv \ "tabSize" match {
      case JInt(n) => n.toInt
      case _ => 4
    }

    val insertSpaces: Boolean = jv \ "insertSpaces" match {
      case JBool(b) => b
      case _ => true
    }

    def optBool(field: String): Option[Boolean] = jv \ field match {
      case JBool(b) => Some(b)
      case _ => None
    }

    FormattingOptions(
      tabSize = tabSize,
      insertSpaces = insertSpaces,
      trimTrailingWhitespace = optBool("trimTrailingWhitespace"),
      insertFinalNewline = optBool("insertFinalNewline"),
      trimFinalNewlines = optBool("trimFinalNewlines")
    )
  }
}
