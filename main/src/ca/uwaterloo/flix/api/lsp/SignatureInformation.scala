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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.fmt.FormatType
import org.json4s.JValue
import org.json4s.JsonDSL.*

import scala.jdk.CollectionConverters.SeqHasAsJava

object SignatureInformation {
  def from(defn: TypedAst.Def, activeParameter: Int)(implicit flix: Flix): SignatureInformation = {
    val label = defn.sym.toString + getLabelForSpec(defn.spec)
    val documentation = defn.spec.doc.text
    val parameters = defn.spec.fparams.map(ParameterInformation.from)
    SignatureInformation(label, Some(documentation), parameters, activeParameter)
  }

  private def getLabelForSpec(spec: TypedAst.Spec)(implicit flix: Flix): String = spec match {
    case TypedAst.Spec(_, _, _, _, fparams, _, retTpe0, eff0, _, _) =>
      val args = if (fparams.length == 1 && fparams.head.tpe == Type.Unit)
        Nil
      else
        fparams.map {
          fparam => s"${fparam.bnd.sym.text}: ${FormatType.formatType(fparam.tpe)}"
        }

      val retTpe = FormatType.formatType(retTpe0)

      val eff = eff0 match {
        case Type.Cst(TypeConstructor.Pure, _) => ""
        case p => raw" \ " + FormatType.formatType(p)
      }

      s"(${args.mkString(", ")}): $retTpe$eff"
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
