/*
 * Copyright 2025 Chenhao Gao
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Symbol, TypedAst}
import org.json4s.JValue
import org.json4s.JsonDSL.*

import scala.jdk.CollectionConverters.SeqHasAsJava

object SignatureInformation {
  def from(sym: Symbol, spec: TypedAst.Spec, activeParameter: Int)(implicit flix: Flix): SignatureInformation = {
    val label = sym.toString + LspUtil.getLabelForSpec(spec)
    val documentation = spec.doc.text
    val parameters = spec.fparams.toList.map(ParameterInformation.from)
    SignatureInformation(label, Some(documentation), parameters, activeParameter)
  }
}

case class SignatureInformation(label: String, documentation: Option[String], parameters: List[ParameterInformation], activeParameter: Int) {
  def toJSON: JValue = {
    ("label" -> label) ~
      ("documentation" -> documentation) ~
      ("parameters" -> parameters.map(_.toJSON)) ~
      ("activeParameter" -> activeParameter)
  }

  def toLsp4j: org.eclipse.lsp4j.SignatureInformation = {
    val sig = new org.eclipse.lsp4j.SignatureInformation(label)
    sig.setDocumentation(documentation.map(doc => new org.eclipse.lsp4j.MarkupContent("markdown", doc)).orNull)
    sig.setParameters(parameters.map(_.toLsp4j).asJava)
    sig.setActiveParameter(activeParameter)
    sig
  }
}
