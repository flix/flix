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

import ca.uwaterloo.flix.language.ast.{Scheme, Type, TypeConstructor, TypedAst}

object FormatSignature {

  /**
    * Returns a markdown string for the signature of the given definition.
    */
  def asMarkDown(defn: TypedAst.Def)(implicit audience: Audience): String = {
    s"""def **${defn.sym.name}**(${formatFormalParams(defn.fparams)}): ${formatResultTypeAndEff(defn.declaredScheme)}
       |""".stripMargin
  }

  /**
    * Returns a markdown string for the signature of the given definition.
    */
  def asMarkDown(sig: TypedAst.Sig)(implicit audience: Audience): String = {
    s"""def **${sig.sym.name}**(${formatFormalParams(sig.fparams)}): ${formatResultTypeAndEff(sig.sc)}
       |""".stripMargin
  }

  /**
    * Returns a formatted string of the formal parameters.
    */
  private def formatFormalParams(fparams: List[TypedAst.FormalParam])(implicit audience: Audience): String = {
    val formattedArgs = fparams.map {
      case TypedAst.FormalParam(sym, _, tpe, _) => s"${sym.text}: ${FormatType.formatType(tpe)}"
    }

    formattedArgs.mkString(", ")
  }

  /**
    * Returns a formatted string of the result type and effect.
    */
  private def formatResultTypeAndEff(sc: Scheme)(implicit audience: Audience): String = {
    val baseType = sc.base
    val resultTyp = getResultType(baseType)
    val resultEff = getEffectType(baseType)

    resultEff match {
      case Type.Cst(TypeConstructor.True, _) => FormatType.formatType(resultTyp)
      case Type.Cst(TypeConstructor.False, _) => s"${FormatType.formatType(resultTyp)} & Impure"
      case eff => s"${FormatType.formatType(resultTyp)} & ${FormatType.formatType(eff)}"
    }
  }

  /**
    * Returns the return type of the given function type `tpe0`.
    */
  private def getResultType(tpe0: Type): Type = tpe0.typeArguments.last

  /**
    * Returns the effect of the given function type `tpe0`.
    */
  private def getEffectType(tpe0: Type): Type = tpe0.typeConstructor match {
    case Some(TypeConstructor.Arrow(_)) => tpe0.typeArguments.head
    case _ => tpe0
  }

}
