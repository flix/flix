/*
 * Copyright 2022 Anna Blume
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
/*package ca.uwaterloo.flix.language.phase.unification

import org.sosy_lab.pjbdd.api.DD

object BoolFormulaBDD {
  def minimizeFormulaWithBDD(bf: BoolFormula): BoolFormula = {
    //create the BDD for the formula
    val dd = createBDDFromFormula(bf)

    //create a boolean formula for the BDD and return it
    createFormulaFromBDD(dd)
  }

  def createBDDFromFormula(bf: BoolFormula): DD = bf match {
    case BoolFormula.True => BddFormula.creator.makeTrue()
    case BoolFormula.False => BddFormula.creator.makeFalse()
    case BoolFormula.Var(x) => BddFormula.creator.makeIthVar(x)
    case BoolFormula.Not(f) => BddFormula.creator.makeNot(createBDDFromFormula(f))
    case BoolFormula.And(f1, f2) => BddFormula.creator.makeAnd(createBDDFromFormula(f1), createBDDFromFormula(f2))
    case BoolFormula.Or(f1,f2) => BddFormula.creator.makeOr(createBDDFromFormula(f1), createBDDFromFormula(f2))
  }

  def createFormulaFromBDD(dd: DD): BoolFormula = {
    createFormulaFromBDDAux(dd, BoolFormula.True)
  }

  private def createFormulaFromBDDAux(dd: DD, bf: BoolFormula): BoolFormula = {
    if(dd.isLeaf()) {
      return if (dd.isTrue()) bf else BoolFormula.False
    }

    val currentVar = dd.getVariable()
    val formVar = BoolFormula.Var(currentVar)
    val lowForm = createFormulaFromBDDAux(dd.getLow(), BoolFormula.And(bf,BoolFormula.Not(formVar)))
    val highForm = createFormulaFromBDDAux(dd.getHigh(), BoolFormula.And(bf,formVar))

    (lowForm, highForm) match {
      case (BoolFormula.False, BoolFormula.False) => BoolFormula.False
      case (BoolFormula.False, _) => highForm
      case (_, BoolFormula.False) => lowForm
      case (_,_) => BoolFormula.Or(lowForm, highForm)
    }
  }

}
*/
