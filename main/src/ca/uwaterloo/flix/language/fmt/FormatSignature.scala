/*
 * Copyright 2020 Matthew Lutze
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
package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Type, TypeConstructor, TypedAst}

object FormatSignature {

  /**
    * Returns a markdown string for the signature of the given definition.
    */
  def asMarkDown(defn: TypedAst.Def)(implicit flix: Flix): String = {
    formatSpec(defn.sym.name, defn.spec)
  }

  /**
    * Returns a markdown string for the signature of the given definition.
    */
  def asMarkDown(sig: TypedAst.Sig)(implicit flix: Flix): String = {
    formatSpec(sig.sym.name, sig.spec)
  }

  /**
    * Returns a markdown string for the signature of the given definition.
    */
  def asMarkDown(op: TypedAst.Op)(implicit flix: Flix): String = {
    formatSpec(op.sym.name, op.spec)
  }

  /**
    * Returns a markdown string for the given `name` and `spec`.
    */
  private def formatSpec(name: String, spec: TypedAst.Spec)(implicit flix: Flix): String = {
    s"def $name(${formatFormalParams(spec.fparams)}): ${formatResultTypeAndEff(spec.retTpe, spec.eff)}"

  }

  /**
    * Returns a formatted string of the formal parameters.
    */
  private def formatFormalParams(fparams0: List[TypedAst.FormalParam])(implicit flix: Flix): String = fparams0 match {
    // Case 1: Single Unit type parameter. This gets sugared into a nullary function: `foo()`
    case fparam :: Nil if fparam.tpe == Type.Unit => ""
    // Case 2: Some list of parameters. Format each and join them: `foo(x: Int32, y: Bool)`
    case fparams =>
      val formattedArgs = fparams.map {
        case TypedAst.FormalParam(bnd, _, tpe, _, _) => s"${bnd.sym.text}: ${FormatType.formatType(tpe)}"
      }
      formattedArgs.mkString(", ")

  }

  /**
    * Returns a formatted string of the result type and effect.
    */
  private def formatResultTypeAndEff(tpe: Type, eff: Type)(implicit flix: Flix): String = eff match {
    case Type.Cst(TypeConstructor.Pure, _) => FormatType.formatType(tpe)
    case Type.Cst(TypeConstructor.Univ, _) => s"${FormatType.formatType(tpe)} \\ IO"
    case eff => s"${FormatType.formatType(tpe)} \\ ${FormatType.formatType(eff)}"
  }
}
