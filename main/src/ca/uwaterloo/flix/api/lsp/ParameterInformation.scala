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

import ca.uwaterloo.flix.language.ast.TypedAst

object ParameterInformation {
  def from(param: TypedAst.FormalParam): ParameterInformation = {
    val label = param.bnd.sym.toString
    ParameterInformation(label, None)
  }
}

case class ParameterInformation(label: String, documentation: Option[String]) {
  def toLsp4j: org.eclipse.lsp4j.ParameterInformation = {
    val param = new org.eclipse.lsp4j.ParameterInformation(label)
    param.setDocumentation(documentation.map(new org.eclipse.lsp4j.MarkupContent("markdown", _)).orNull)
    param
  }
}
