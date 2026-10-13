/*
 * Copyright 2025 Chenhao Gao
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
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
