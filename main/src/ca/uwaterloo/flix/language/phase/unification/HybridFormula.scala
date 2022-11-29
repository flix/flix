/*
 * Copyright 2022 Magnus Madsen
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

import ca.uwaterloo.flix.language.ast.{Symbol, Type}
import ca.uwaterloo.flix.language.phase.unification.BddFormula.BddFormula
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.collection.Bimap
import org.sosy_lab.pjbdd.api.DD

import scala.collection.immutable.SortedSet

trait Formula {}

object HybridFormula {

  implicit val boolAlg: BoolAlg[BoolFormula] = BoolFormula.AsBoolAlg
  implicit val bddAlg: BoolAlg[BddFormula] = BddFormula.AsBoolAlg

  def convertToBddFormula(f: Formula): BddFormula = f match {
    case boolForm: BoolFormula => convertBoolFormToBddForm(boolForm)
    case bddForm: BddFormula => bddForm
  }

  def convertBoolFormToBddForm(f: BoolFormula): BddFormula = f match {
    case BoolFormula.True => bddAlg.mkTrue
    case BoolFormula.False => bddAlg.mkFalse
    case BoolFormula.Var(id) => bddAlg.mkVar(id)
    case BoolFormula.And(f1, f2) =>
      bddAlg.mkAnd(convertBoolFormToBddForm(f1), convertBoolFormToBddForm(f2))
    case BoolFormula.Or(f1, f2) =>
      bddAlg.mkOr(convertBoolFormToBddForm(f1), convertBoolFormToBddForm(f2))
    case BoolFormula.Not(f1) => bddAlg.mkNot(convertBoolFormToBddForm(f1))
  }

  def convertToBoolFormula(f: Formula): BoolFormula = f match {
    case boolForm: BoolFormula => boolForm
    case bddForm: BddFormula => convertBddFormToBoolForm(bddForm.getDD(), boolAlg.mkTrue)
  }

  def convertBddFormToBoolForm(dd: DD, acc: BoolFormula): BoolFormula = {
    if (dd.isLeaf()) {
      return if (dd.isTrue()) acc else boolAlg.mkFalse
    }

    val currentVar = dd.getVariable()
    val varForm = boolAlg.mkVar(currentVar)

    val lowForm = boolAlg.mkAnd(acc, boolAlg.mkNot(varForm))
    val lowRes = convertBddFormToBoolForm(dd.getLow(), lowForm)
    val highForm = boolAlg.mkAnd(acc, varForm)
    val highRes = convertBddFormToBoolForm(dd.getHigh(), highForm)

    (lowRes, highRes) match {
      case (BoolFormula.False, BoolFormula.False) => BoolFormula.False
      case (BoolFormula.False, _) => highRes
      case (_, BoolFormula.False) => lowRes
      case (_, _) => boolAlg.mkOr(lowRes, highRes)
    }
  }

  class HybridFormula(val formula: Formula, val vars: SortedSet[Int]) extends Formula {
    val size = vars.size
  }

  implicit val AsBoolAlg: BoolAlg[HybridFormula] = new BoolAlg[HybridFormula] {
    override def isTrue(f: HybridFormula): Boolean = f.formula match {
      case boolForm: BoolFormula => boolAlg.isTrue(boolForm)
      case bddForm: BddFormula => bddAlg.isTrue(bddForm)
    }

    override def isFalse(f: HybridFormula): Boolean = f.formula match {
      case boolForm: BoolFormula => boolAlg.isFalse(boolForm)
      case bddForm: BddFormula => bddAlg.isFalse(bddForm)
    }

    override def isVar(f: HybridFormula): Boolean = f.formula match {
      case boolForm: BoolFormula => boolAlg.isVar(boolForm)
      case bddForm: BddFormula => bddAlg.isVar(bddForm)
    }

    override def mkTrue: HybridFormula = new HybridFormula(boolAlg.mkTrue, SortedSet.empty)

    override def mkFalse: HybridFormula = new HybridFormula(boolAlg.mkFalse, SortedSet.empty)

    override def mkVar(id: Int): HybridFormula = new HybridFormula(boolAlg.mkVar(id), SortedSet(id))

    override def mkNot(f: HybridFormula): HybridFormula = f.formula match {
      case boolForm: BoolFormula => new HybridFormula(boolAlg.mkNot(boolForm), f.vars)
      case bddForm: BddFormula => new HybridFormula(bddAlg.mkNot(bddForm), f.vars)
    }

    override def mkOr(f1: HybridFormula, f2: HybridFormula): HybridFormula = {
      val collectedVars = f1.vars ++ f2.vars
      if(collectedVars.size > 4) {
        //convert to BDDFormulas
        (f1.formula, f2.formula) match {
          case (boolForm1: BoolFormula, boolForm2: BoolFormula) =>
            new HybridFormula(bddAlg.mkOr(convertToBddFormula(boolForm1), convertToBddFormula(boolForm2)), collectedVars)
          case (boolForm: BoolFormula, bddForm: BddFormula) =>
            new HybridFormula(bddAlg.mkOr(convertToBddFormula(boolForm), bddForm), collectedVars)
          case (bddForm: BddFormula, boolForm: BoolFormula) =>
            new HybridFormula(bddAlg.mkOr(bddForm, convertToBddFormula(boolForm)), collectedVars)
          case (bddForm1: BddFormula, bddForm2: BddFormula) =>
            new HybridFormula(bddAlg.mkOr(bddForm1, bddForm2), collectedVars)
        }
      } else {
        //stick with BoolFormulas
        (f1.formula, f2.formula) match {
          case (boolForm1: BoolFormula, boolForm2: BoolFormula) =>
            new HybridFormula(boolAlg.mkOr(boolForm1, boolForm2), collectedVars)
          case _ => throw InternalCompilerException("Big mistake. Big. Huge!")
        }
      }
    }

    override def mkAnd(f1: HybridFormula, f2: HybridFormula): HybridFormula = {
      val collectedVars = f1.vars ++ f2.vars
      if (collectedVars.size > 4) {
        //convert to BDDFormulas
        (f1.formula, f2.formula) match {
          case (boolForm1: BoolFormula, boolForm2: BoolFormula) =>
            new HybridFormula(bddAlg.mkAnd(convertToBddFormula(boolForm1), convertToBddFormula(boolForm2)), collectedVars)
          case (boolForm: BoolFormula, bddForm: BddFormula) =>
            new HybridFormula(bddAlg.mkAnd(convertToBddFormula(boolForm), bddForm), collectedVars)
          case (bddForm: BddFormula, boolForm: BoolFormula) =>
            new HybridFormula(bddAlg.mkAnd(bddForm, convertToBddFormula(boolForm)), collectedVars)
          case (bddForm1: BddFormula, bddForm2: BddFormula) =>
            new HybridFormula(bddAlg.mkAnd(bddForm1, bddForm2), collectedVars)
        }
      } else {
        //stick with BoolFormulas
        (f1.formula, f2.formula) match {
          case (boolForm1: BoolFormula, boolForm2: BoolFormula) =>
            new HybridFormula(boolAlg.mkAnd(boolForm1, boolForm2), collectedVars)
          case _ => throw InternalCompilerException("Big mistake. Big. Huge!")
        }
      }
    }

    override def freeVars(f: HybridFormula): SortedSet[Int] = f.vars

    override def map(f: HybridFormula)(fn: Int => HybridFormula): HybridFormula = f.formula match {
      case boolForm: BoolFormula =>
        val mapRes = boolAlg.map(boolForm)(fn andThen (g => convertToBoolFormula(g.formula)))
        val newVars =  boolAlg.freeVars(mapRes)
        if(newVars.size > 4)
          new HybridFormula(convertToBddFormula(mapRes), newVars)
        else new HybridFormula(mapRes, newVars)
      case bddForm: BddFormula =>
        val mapRes = bddAlg.map(bddForm)(fn andThen (g => convertToBddFormula(g.formula)))
        val newVars = bddAlg.freeVars(mapRes)
        if (newVars.size > 4)
          new HybridFormula(mapRes, newVars)
        else new HybridFormula(convertToBoolFormula(mapRes), newVars)
    }

    override def satisfiable(f: HybridFormula): Boolean = f.formula match {
      case boolForm: BoolFormula => boolAlg.satisfiable(boolForm)
      case bddForm: BddFormula => bddAlg.satisfiable(bddForm)
    }

    override def minimize(f: HybridFormula): HybridFormula = f.formula match {
      case boolForm: BoolFormula =>
        val minRes = boolAlg.minimize(boolForm)
        new HybridFormula(minRes, boolAlg.freeVars(minRes))
      case _: BddFormula => f
    }

    override def toType(f: HybridFormula, env: Bimap[Symbol.KindedTypeVarSym, Int]): Type = f.formula match {
      case boolForm: BoolFormula => boolAlg.toType(boolForm, env)
      case bddForm: BddFormula => bddAlg.toType(bddForm, env)
    }
  }
}
