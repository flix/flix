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

import org.json4s.JsonDSL.*
import org.json4s.*

/**
  * Represents a `WorkspaceEdit` in LSP.
  *
  * @param changes A map from URIs to lists of changes.
  */
case class WorkspaceEdit(changes: Map[String, List[TextEdit]]) {
  def toJSON: JValue = ("changes" -> mapValues(changes)(xs => xs.map(_.toJSON)))

  private def mapValues[K, V1, V2](m: Map[K, V1])(f: V1 => V2): Map[K, V2] = m map {
    case (k, v) => k -> f(v)
  }
}
