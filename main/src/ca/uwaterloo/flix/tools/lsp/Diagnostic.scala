/*
 * Copyright 2020 Magnus Madsen
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
package ca.uwaterloo.flix.tools.lsp

import org.json4s.JsonAST.{JField, JInt, JObject, JString}

/**
  * Represent a `Diagnostic` in LSP.
  */
case class Diagnostic(range: Range, code: String, message: String) {
  def toJSON: JObject =
    JObject(
      JField("range", range.toJSON),
      JField("severity", JInt(1)),
      JField("code", JString(code)),
      JField("message", JString(message)),
    )
}
