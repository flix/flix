/*
 * Copyright 2020 Matthew Lutze
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.util.collection.Nel

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
  private def formatFormalParams(fparams0: Nel[TypedAst.FormalParam])(implicit flix: Flix): String = fparams0 match {
    // Case 1: Single Unit type parameter. This gets sugared into a nullary function: `foo()`
    case Nel(fparam, Nil) if fparam.tpe == Type.Unit => ""
    // Case 2: Some list of parameters. Format each and join them: `foo(x: Int32, y: Bool)`
    case fparams =>
      val formattedArgs = fparams.map {
        case TypedAst.FormalParam(bnd, tpe, _, _, _) => s"${bnd.sym.text}: ${FormatType.formatType(tpe)}"
      }
      formattedArgs.mkString(", ")

  }

  /**
    * Returns a formatted string of the result type and effect.
    */
  private def formatResultTypeAndEff(tpe: Type, eff: Type)(implicit flix: Flix): String = eff match {
    case Type.Cst(TypeConstructor.Pure, _) => FormatType.formatType(tpe)
    case Type.Cst(TypeConstructor.Univ, _) => s"${FormatType.formatType(tpe)} \\ IO"
    case otherEff => s"${FormatType.formatType(tpe)} \\ ${FormatType.formatType(otherEff)}"
  }
}
