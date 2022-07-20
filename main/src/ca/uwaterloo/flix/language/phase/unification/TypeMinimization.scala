/*
 * Copyright 2022 Matthew Lutze
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
package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Ast, Kind, Scheme, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.unification.BoolFormula.{fromBoolType, fromEffType, toType}
import ca.uwaterloo.flix.language.phase.unification.BoolTable.minimizeFormula
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.collection.Bimap

import scala.collection.immutable.SortedSet

/**
  * Performs minimization on types,
  * reducing boolean formulas and set formulas to more concise equivalent forms.
  */
object TypeMinimization {

  /**
    * Minimizes the given type, reducing it to a more concise equivalent form.
    */
  def minimizeType(t: Type)(implicit flix: Flix): Type = t.kind match {
    case Kind.Effect => minimizeBoolAlg(t)
    case Kind.Bool => minimizeBoolAlg(t)
    case _ => t match {
      case tpe: Type.KindedVar => tpe
      case tpe: Type.Cst => tpe
      case Type.Apply(tpe1, tpe2, loc) => Type.Apply(minimizeType(tpe1), minimizeType(tpe2), loc)
      case Type.Alias(cst, args, tpe, loc) => Type.Alias(cst, args.map(minimizeType), minimizeType(tpe), loc)

      case _: Type.UnkindedVar => throw InternalCompilerException("unexpected unkinded type")
      case _: Type.Ascribe => throw InternalCompilerException("unexpected unkinded type")
      case _: Type.UnkindedArrow => throw InternalCompilerException("unexpected unkinded type")
      case _: Type.ReadWrite => throw InternalCompilerException("unexpected unkinded type")
    }
  }

  /**
    * Attempts to minimize the given Boolean formula `tpe`.
    *
    * Returns the same formula or a smaller formula that is equivalent.
    */
  private def minimizeBoolAlg(tpe0: Type)(implicit flix: Flix): Type = {
    // Check whether minimization via tabling is disabled.
    if (flix.options.xnobooltable) {
      return tpe0
    }

    // Check that the `tpe` argument is a Boolean formula.
    if (tpe0.kind != Kind.Bool && tpe0.kind != Kind.Effect) {
      throw InternalCompilerException(s"Unexpected non-Bool/non-Effect kind: '${tpe0.kind}'.")
    }

    // Erase aliases to get a processable type
    val tpe = Type.eraseAliases(tpe0)

    // Compute the size of  `tpe`.
    val currentSize = tpe.size

    // Return `tpe` immediately if it is "small".
    if (currentSize < BoolTable.Threshold) {
      return tpe
    }

    // Compute the variables in `tpe`.
    val tvars = tpe.typeVars.toList.map(tvar => BoolFormula.VarOrEff.Var(tvar.sym))
    val effs = getEffects(tpe).toList.map(BoolFormula.VarOrEff.Eff)

    // Construct a bi-directional map from type variables to indices.
    // The idea is that the first variable becomes x0, the next x1, and so forth.
    val m = (tvars ++ effs).zipWithIndex.foldLeft(Bimap.empty[BoolFormula.VarOrEff, BoolTable.Variable]) {
      case (macc, (sym, x)) => macc + (sym -> x)
    }

    // Convert the type `tpe` to a Boolean formula.
    val input = tpe.kind match {
      case Kind.Bool => fromBoolType(tpe, m)
      case Kind.Effect => fromEffType(tpe, m)
      case _ => throw InternalCompilerException(s"Unexpected non-Bool/non-Effect kind: '${tpe.kind}'.")
    }

    // Minimize the Boolean formula.
    val minimized = minimizeFormula(input)

    // Convert the formula back to a type.
    toType(minimized, m, tpe.kind, tpe.loc)
  }


  /**
    * Gets all the effects in the given type.
    */
  private def getEffects(t: Type): SortedSet[Symbol.EffectSym] = t match {
    case Type.Cst(TypeConstructor.Effect(sym), _) => SortedSet(sym)

    case _: Type.Cst => SortedSet.empty
    case _: Type.KindedVar => SortedSet.empty

    case Type.Apply(tpe1, tpe2, loc) => getEffects(tpe1) ++ getEffects(tpe2)
    case Type.Alias(cst, args, tpe, loc) => getEffects(tpe)

    case _: Type.Ascribe => throw InternalCompilerException("Unexpected unkinded type.")
    case _: Type.UnkindedVar => throw InternalCompilerException("Unexpected unkinded type.")
    case _: Type.UnkindedArrow => throw InternalCompilerException("Unexpected unkinded type.")
    case _: Type.ReadWrite => throw InternalCompilerException("Unexpected unkinded type.")
  }
}
