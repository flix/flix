/*
 * Copyright 2022 Anna Blume Jakobsen
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

import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.collection.Bimap

import scala.collection.immutable.SortedSet
import org.sosy_lab.pjbdd.api.{Builders, Creator, DD}

object BddFormula {

  //Thread-safe factory for creating BDDs
  private val GlobalBddBuilder: Creator = Builders.bddBuilder().build()

  class BddFormula(val dd: DD) extends Formula { }

  implicit val AsBoolAlg: BoolAlg[BddFormula] = new BoolAlg[BddFormula] {
    override def isTrue(f: BddFormula): Boolean = f.dd.isTrue

    override def isFalse(f: BddFormula): Boolean = f.dd.isFalse

    override def isVar(f: BddFormula): Boolean =
      f.dd.equalsTo(f.dd.getVariable, GlobalBddBuilder.makeFalse(), GlobalBddBuilder.makeTrue())

    override def mkTrue: BddFormula = {
      new BddFormula(GlobalBddBuilder.makeTrue())
    }

    override def mkFalse: BddFormula = {
      new BddFormula(GlobalBddBuilder.makeFalse())
    }

    override def mkVar(id: Int): BddFormula = {
      new BddFormula(GlobalBddBuilder.makeIthVar(id))
    }

    override def mkNot(f: BddFormula): BddFormula = {
      new BddFormula(GlobalBddBuilder.makeNot(f.dd))
    }

    override def mkOr(f1: BddFormula, f2: BddFormula): BddFormula = {
      new BddFormula(GlobalBddBuilder.makeOr(f1.dd, f2.dd))
    }

    override def mkAnd(f1: BddFormula, f2: BddFormula): BddFormula = {
      new BddFormula(GlobalBddBuilder.makeAnd(f1.dd, f2.dd))
    }

    override def mkXor(f1: BddFormula, f2: BddFormula): BddFormula = {
      new BddFormula(GlobalBddBuilder.makeXor(f1.dd, f2.dd))
    }

    override def freeVars(f: BddFormula): SortedSet[Int] = {
      freeVarsAux(f.dd)
    }

    private def freeVarsAux(dd: DD): SortedSet[Int] = {
      if (dd.isLeaf) {
        SortedSet.empty
      } else {
        SortedSet(dd.getVariable) ++
          freeVarsAux(dd.getLow) ++
          freeVarsAux(dd.getHigh)
      }
    }

    override def map(f: BddFormula)(fn: Int => BddFormula): BddFormula = {
      new BddFormula(mapAux(f.dd)(fn andThen (f => f.dd)))
    }

    private def mapAux(dd: DD)(fn: Int => DD): DD = {
      if (dd.isLeaf) {
        dd
      } else {
        val currentVar = dd.getVariable
        val substDD = fn(currentVar)

        val lowRes = mapAux(dd.getLow)(fn)
        val highRes = mapAux(dd.getHigh)(fn)
        GlobalBddBuilder.makeIte(substDD, highRes, lowRes)
      }
    }

    override def minimize(f: BddFormula): BddFormula = f

    override def toType(f: BddFormula, env: Bimap[Symbol.KindedTypeVarSym, Int]): Type = {
      createTypeFromBDDAux(f.dd, Type.True, env)
    }

    //TODO: Optimize (2-level minimization)
    private def createTypeFromBDDAux(dd: DD, tpe: Type, env: Bimap[Symbol.KindedTypeVarSym, Int]): Type = {
      if (dd.isLeaf) {
        return if (dd.isTrue) tpe else Type.False
      }

      val currentVar = dd.getVariable
      val typeVar = env.getBackward(currentVar) match {
        case Some(sym) => Type.Var(sym, sym.loc)
        case None => throw InternalCompilerException(s"unexpected unknown ID: $currentVar")
      }

      val lowType = Type.mkApply(Type.And, List(tpe, Type.Apply(Type.Not, typeVar, typeVar.loc)), typeVar.loc)
      val lowRes = createTypeFromBDDAux(dd.getLow, lowType, env)
      val highType = Type.mkApply(Type.And, List(tpe, typeVar), typeVar.loc)
      val highRes = createTypeFromBDDAux(dd.getHigh, highType, env)

      (lowRes, highRes) match {
        case (Type.False, Type.False) => Type.False
        case (Type.False, _) => highRes
        case (_, Type.False) => lowRes
        case (t1, _) => Type.mkApply(Type.Or, List(lowRes, highRes), t1.loc)
      }
    }

    override def satisfiable(f: BddFormula): Boolean = !f.dd.isFalse

  }
}
