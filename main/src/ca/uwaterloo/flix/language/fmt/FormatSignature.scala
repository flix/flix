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
    formatSpecWithOptions(name, spec, FormatOptions.Default)
  }

  /**
    * Returns a string for the given `name` and `spec`, written as the declaration would be, e.g.
    * `def sell(x: a, n: Int32): Int32 \ IO with ToString[a]`.
    *
    * The constraints are part of the signature: what a declaration requires of the types it is
    * given is as much a part of how it may be called as the types themselves.
    */
  def formatSpecWithOptions(name: String, spec: TypedAst.Spec, fmt: FormatOptions): String = {
    val params = formatFormalParams(spec.fparams, fmt)
    val result = formatResultTypeAndEff(spec.retTpe, spec.eff, fmt)
    // N.B.: The constraints are taken from the declared scheme. `Spec` holds the equality
    // constraints as `TypedAst.EqualityConstraint`, which does not name the associated type, so
    // there is not enough in one to write it out.
    val sc = spec.declaredScheme
    s"def $name($params): $result${Pretty.constraints(sc.tconstrs, sc.econstrs)(fmt).plain}"
  }

  /**
    * Returns a formatted string of the formal parameters.
    */
  private def formatFormalParams(fparams0: Nel[TypedAst.FormalParam], fmt: FormatOptions): String = fparams0 match {
    // Case 1: Single Unit type parameter. This gets sugared into a nullary function: `foo()`
    case Nel(fparam, Nil) if fparam.tpe == Type.Unit => ""
    // Case 2: Some list of parameters. Format each and join them: `foo(x: Int32, y: Bool)`
    case fparams =>
      val formattedArgs = fparams.map {
        case TypedAst.FormalParam(bnd, tpe, _, _, _) => s"${bnd.sym.text}: ${FormatType.formatTypeWithOptions(tpe, fmt)}"
      }
      formattedArgs.mkString(", ")

  }

  /**
    * Returns a formatted string of the result type and effect.
    */
  private def formatResultTypeAndEff(tpe: Type, eff: Type, fmt: FormatOptions): String = eff match {
    case Type.Cst(TypeConstructor.Pure, _) => FormatType.formatTypeWithOptions(tpe, fmt)
    case otherEff => s"${FormatType.formatTypeWithOptions(tpe, fmt)} \\ ${FormatType.formatTypeWithOptions(otherEff, fmt)}"
  }
}
