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
package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.language.ast.{Type, TypeConstructor, TypedAst}

object FormatSignature {

  /**
    * Returns a markdown string for the signature of the given definition.
    */
  def asMarkDown(defn: TypedAst.Def)(implicit audience: Audience): String = {
    formatSpec(defn.sym.name, defn.spec)
  }

  /**
    * Returns a markdown string for the signature of the given definition.
    */
  def asMarkDown(sig: TypedAst.Sig)(implicit audience: Audience): String = {
    formatSpec(sig.sym.name, sig.spec)
  }

  /**
    * Returns a markdown string for the given `name` and `spec`.
    */
  private def formatSpec(name: String, spec: TypedAst.Spec)(implicit audience: Audience): String = {
    s"""def **${name}**(${formatFormalParams(spec.fparams)}): ${formatResultTypeAndEff(spec.retTpe, spec.eff)}
       |""".stripMargin

  }

  /**
    * Returns a formatted string of the formal parameters.
    */
  private def formatFormalParams(fparams: List[TypedAst.FormalParam])(implicit audience: Audience): String = {
    val formattedArgs = fparams.map {
      case TypedAst.FormalParam(sym, _, tpe, _, _) => s"${sym.text}: ${FormatType.formatType(tpe)}"
    }

    formattedArgs.mkString(", ")
  }

  /**
    * Returns a formatted string of the result type and effect.
    */
  private def formatResultTypeAndEff(tpe: Type, eff: Type)(implicit audience: Audience): String = eff match {
    case Type.Cst(TypeConstructor.True, _) => FormatType.formatType(tpe)
    case Type.Cst(TypeConstructor.False, _) => s"${FormatType.formatType(tpe)} & Impure"
    case eff => s"${FormatType.formatType(tpe)} & ${FormatType.formatType(eff)}"
  }
}
