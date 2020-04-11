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
package ca.uwaterloo.flix.api.lsp

import org.json4s.JsonAST.{JArray, JField, JObject, JString}

case class PublishDiagnosticsParams(uri: String, diagnostics: List[Diagnostic]) {
  def toJSON: JObject =
    JObject(
      JField("uri", JString(uri)),
      JField("diagnostics", JArray(diagnostics.map(_.toJSON)))
    )
}
