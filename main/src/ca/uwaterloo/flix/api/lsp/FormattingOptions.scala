/*
 * Copyright 2025 gwydd
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
import org.json4s.JObject
import org.json4s.JsonAST.{JBool, JInt, JValue}
import org.json4s.JsonDSL._

import org.eclipse.lsp4j

case class FormattingOptions(
                              tabSize: Int,
                              insertSpaces: Boolean,
                              trimTrailingWhitespace: Option[Boolean] = None,
                              insertFinalNewline: Option[Boolean] = None,
                              trimFinalNewlines: Option[Boolean] = None
                            ) {

  def toJSON: JObject = {
    var obj: JObject = ("tabSize" -> tabSize) ~ ("insertSpaces" -> insertSpaces)

    // FormattingOptions that are optional and were introduced in version 3.15 (unused)
    trimTrailingWhitespace.foreach(v => obj = obj ~ ("trimTrailingWhitespace" -> v))
    insertFinalNewline.foreach(v => obj = obj ~ ("insertFinalNewline" -> v))
    trimFinalNewlines.foreach(v => obj = obj ~ ("trimFinalNewlines" -> v))

    obj
  }

  def toLsp4j: lsp4j.FormattingOptions = new lsp4j.FormattingOptions(tabSize, insertSpaces)
}

object FormattingOptions {
  def fromJSON(jv: JValue): lsp4j.FormattingOptions = {
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
    ).toLsp4j
  }
}
