/*
 * Copyright 2025 Din Jakupi
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
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
