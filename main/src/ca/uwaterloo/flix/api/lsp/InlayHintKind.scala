/*
 * Copyright 2022 Nicola Dardanis
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
import org.json4s.{JInt, JValue}

/** Represents an `InlayHintKind` in LSP. */
sealed trait InlayHintKind {
  def toJSON: JValue = this match {
    case InlayHintKind.Type => JInt(1)
    case InlayHintKind.Parameter => JInt(2)
  }
}

object InlayHintKind {
  /** An inlay hint that for a type annotation. */
  case object Type extends InlayHintKind
  /** An inlay hint that is for a parameter. */
  case object Parameter extends InlayHintKind
}
