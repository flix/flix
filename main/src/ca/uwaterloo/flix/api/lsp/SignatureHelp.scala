/*
 * Copyright 2025 Chenhao Gao
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

import org.json4s.JValue
import org.json4s.JsonDSL.*

import scala.jdk.CollectionConverters.SeqHasAsJava

case class SignatureHelp(signatures: List[SignatureInformation], activeSignature: Int, activeParameter: Int) {
  def toJSON: JValue = {
    ("signatures" -> signatures.map(_.toJSON)) ~
      ("activeSignature" -> activeSignature) ~
      ("activeParameter" -> activeParameter)
  }

  def toLsp4j: org.eclipse.lsp4j.SignatureHelp = {
    val sigHelp = new org.eclipse.lsp4j.SignatureHelp()
    sigHelp.setSignatures(signatures.map(_.toLsp4j).asJava)
    sigHelp.setActiveSignature(activeSignature)
    sigHelp.setActiveParameter(activeParameter)
    sigHelp
  }
}
