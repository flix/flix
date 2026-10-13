/*
 * Copyright 2025 Chenhao Gao
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.fmt.FormatType
import org.json4s.JValue
import org.json4s.JsonDSL.*

object ParameterInformation {
  def from(param: TypedAst.FormalParam)(implicit flix: Flix): ParameterInformation = {
    val label = s"${param.bnd.sym.text}: ${FormatType.formatType(param.tpe)}"
    ParameterInformation(label, None)
  }
}

case class ParameterInformation(label: String, documentation: Option[String]) {
  def toJSON: JValue = {
    ("label" -> label) ~
      ("documentation" -> documentation)
  }

  def toLsp4j: org.eclipse.lsp4j.ParameterInformation = {
    val param = new org.eclipse.lsp4j.ParameterInformation(label)
    param.setDocumentation(documentation.map(new org.eclipse.lsp4j.MarkupContent("markdown", _)).orNull)
    param
  }
}
