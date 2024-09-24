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
import ca.uwaterloo.flix.language.ast.{Ast, Kind, Scheme, Type}
import ca.uwaterloo.flix.language.phase.unification.BoolFormula.{fromBoolType, fromEffType, toType}
import ca.uwaterloo.flix.language.phase.unification.BoolFormulaTable.minimizeFormula
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.collection.Bimap

/**
  * Performs minimization on types,
  * reducing boolean formulas and set formulas to more concise equivalent forms.
  */
object TypeMinimization {

  /** Minimizes the given type, reducing it to a more concise equivalent form. */
  def minimizeType(t: Type): Type = t.kind match {
    case Kind.Eff => minimizeBoolAlg(t)
    case Kind.Bool => minimizeBoolAlg(t)
    case _ => t match {
      case tpe: Type.Var => tpe
      case tpe: Type.Cst => tpe
      case Type.Apply(tpe1, tpe2, loc) => Type.Apply(minimizeType(tpe1), minimizeType(tpe2), loc)
      case Type.Alias(cst, args, tpe, loc) => Type.Alias(cst, args.map(minimizeType), minimizeType(tpe), loc)
      case Type.AssocType(cst, args, kind, loc) => Type.AssocType(cst, args.map(minimizeType), kind, loc)
      case Type.JvmToType(tpe, loc) => Type.JvmToType(minimizeType(tpe), loc)
      case Type.JvmToEff(tpe, loc) => Type.JvmToEff(minimizeType(tpe), loc)
      case Type.UnresolvedJvmType(member, loc) => Type.UnresolvedJvmType(member.map(minimizeType), loc)
    }
  }

  /** Minimizes the given scheme, reducing it to a more concise equivalent form. */
  def minimizeScheme(sc: Scheme): Scheme = sc match {
    case Scheme(quantifiers, tconstrs, econstrs, base) =>
      val newBase = minimizeType(base)
      val tvars = newBase.typeVars.map(_.sym)

      // filter out unused quantifiers
      val newQuants = quantifiers.filter(tvars.contains)

      // filter out unused type constraints
      val newTconstrs = tconstrs.filter {
        case Ast.TraitConstraint(_, Type.Var(sym, _), _) if tvars.contains(sym) => true
        case _ => false
      }
      Scheme(newQuants, newTconstrs, econstrs, newBase)
  }

  /**
    * Attempts to minimize the given Boolean formula `tpe`.
    *
    * Returns the same formula or a smaller formula that is equivalent.
    */
  private def minimizeBoolAlg(tpe0: Type): Type = {

    // Check that the `tpe` argument is a Boolean formula.
    tpe0.kind match {
      case Kind.Eff => // OK
      case Kind.Bool => // OK
      case _ => throw InternalCompilerException(s"Unexpected non-Bool/non-Effect kind: '${tpe0.kind}'.", tpe0.loc)
    }

    // Erase aliases to get a processable type
    val tpe = Type.eraseAliases(tpe0)

    // Compute the size of  `tpe`.
    val currentSize = tpe.size

    // Return `tpe` immediately if it is "small".
    if (currentSize < BoolFormulaTable.Threshold) {
      return tpe
    }

    // Compute the variables in `tpe`.
    val tvars = tpe.typeVars.toList.map(tvar => BoolFormula.IrreducibleEff.Var(tvar.sym))
    val effs = tpe.effects.toList.map(BoolFormula.IrreducibleEff.Eff.apply)
    val assocs = tpe.assocs.toList.map(assoc => BoolFormula.IrreducibleEff.Assoc(assoc.cst.sym, assoc.arg))
    val jvmToEffs = tpe.jvmToEffs.toList.map(tpe => BoolFormula.IrreducibleEff.JvmToEff(tpe))

    // Construct a bi-directional map from type variables to indices.
    // The idea is that the first variable becomes x0, the next x1, and so forth.
    val m = (tvars ++ effs ++ assocs ++ jvmToEffs).zipWithIndex.foldLeft(Bimap.empty[BoolFormula.IrreducibleEff, BoolFormulaTable.Variable]) {
      case (macc, (sym, x)) => macc + (sym -> x)
    }

    // Convert the type `tpe` to a Boolean formula.
    val input = tpe.kind match {
      case Kind.Eff => fromEffType(tpe, m)
      case Kind.Bool => fromBoolType(tpe, m)
      case _ => throw InternalCompilerException(s"Unexpected non-Bool/non-Effect/non-Case kind: '${tpe.kind}'.", tpe.loc)
    }

    // Minimize the Boolean formula.
    val minimized = minimizeFormula(input)

    // Convert the formula back to a type.
    toType(minimized, m, tpe.kind, tpe.loc)
  }
}
