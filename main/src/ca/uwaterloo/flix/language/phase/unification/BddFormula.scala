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

import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.collection.Bimap
import com.github.javabdd.{BDD, BDDFactory, JFactory}

import java.util.concurrent.locks.ReentrantLock
import scala.collection.immutable.SortedSet

object BddFormula {

  val lock = new ReentrantLock()
  var factory: BDDFactory = JFactory.init(20, 500)
  factory.setVarNum(20)

  class BddFormula(val dd: BDD) {
    def getDD(): BDD = dd
  }

  implicit val AsBoolAlg: BoolAlg[BddFormula] = new BoolAlg[BddFormula] {
    override def isTrue(f: BddFormula): Boolean = f.getDD().isOne

    override def isFalse(f: BddFormula): Boolean = f.getDD().isZero

    override def isVar(f: BddFormula): Boolean = f.getDD().equalsBDD(factory.ithVar(f.getDD().`var`()))

    override def mkTrue: BddFormula = {
      new BddFormula(factory.one())
    }

    override def mkFalse: BddFormula = {
      new BddFormula(factory.zero())
    }

    override def mkVar(id: Int): BddFormula = {
      new BddFormula(factory.ithVar(id))
    }

    override def mkNot(f: BddFormula): BddFormula = {
      new BddFormula(f.getDD().not())
    }

    override def mkOr(f1: BddFormula, f2: BddFormula): BddFormula = {
      new BddFormula(f1.getDD().or(f2.getDD()))
    }

    override def mkAnd(f1: BddFormula, f2: BddFormula): BddFormula = {
      new BddFormula(f1.getDD().and(f2.getDD()))
    }

    override def mkXor(f1: BddFormula, f2: BddFormula): BddFormula = {
      new BddFormula(f1.getDD().xor(f2.getDD()))
    }

    override def freeVars(f: BddFormula): SortedSet[Int] = {
      freeVarsAux(f.getDD())
    }

    private def freeVarsAux(dd: BDD): SortedSet[Int] = {
      if (dd.isOne || dd.isZero) {
        SortedSet.empty
      } else {
        SortedSet(dd.`var`()) ++
          freeVarsAux(dd.low()) ++
          freeVarsAux(dd.high())
      }
    }

    override def map(f: BddFormula)(fn: Int => BddFormula): BddFormula = {
      new BddFormula(mapAux(f.getDD())(fn))
    }

    private def mapAux(dd: BDD)(fn: Int => BddFormula): BDD = {
      if (dd.isOne() || dd.isZero()) {
        dd
      } else {
        val currentVar = dd.`var`()
        val substDD = fn(currentVar).getDD()

        if(substDD.isOne()) {
          mapAux(dd.high())(fn)
        } else if(substDD.isZero()) {
          mapAux(dd.low())(fn)
        } else {
          val lowRes = mapAux(dd.low())(fn)
          val highRes = mapAux(dd.high())(fn)
          substDD.ite(highRes, lowRes)
        }
      }
    }

    override def minimize(f: BddFormula): BddFormula = f

    override def getEnv(fs: List[Type]): Bimap[Symbol.KindedTypeVarSym, Int] =
    {
      // Compute the variables in `tpe`.
      val tvars = fs.flatMap(_.typeVars).map(_.sym).to(SortedSet)

      // Construct a bi-directional map from type variables to indices.
      // The idea is that the first variable becomes x0, the next x1, and so forth.
      tvars.zipWithIndex.foldLeft(Bimap.empty[Symbol.KindedTypeVarSym, Int]) {
        case (macc, (sym, x)) => macc + (sym -> x)
      }
    }

    override def toType(f: BddFormula, env: Bimap[Symbol.KindedTypeVarSym, Int]): Type = {
      createTypeFromBDDAux(f.getDD(), Type.True, env)
    }

    //TODO: Optimize
    private def createTypeFromBDDAux(dd: BDD, tpe: Type, env: Bimap[Symbol.KindedTypeVarSym, Int]): Type = {
      if (dd.isOne || dd.isZero) {
        return if (dd.isOne) tpe else Type.False
      }

      val currentVar = dd.`var`()
      val typeVar = env.getBackward(currentVar) match {
        case Some(sym) => Type.Var(sym, SourceLocation.Unknown)
        case None => throw InternalCompilerException(s"unexpected unknown ID: $currentVar", tpe.loc)
      }

      val lowType = Type.mkApply(Type.And, List(tpe, Type.Apply(Type.Not, typeVar, SourceLocation.Unknown)), SourceLocation.Unknown)
      val lowRes = createTypeFromBDDAux(dd.low(), lowType, env)
      val highType = Type.mkApply(Type.And, List(tpe, typeVar), SourceLocation.Unknown)
      val highRes = createTypeFromBDDAux(dd.high(), highType, env)

      (lowRes, highRes) match {
        case (Type.False, Type.False) => Type.False
        case (Type.False, _) => highRes
        case (_, Type.False) => lowRes
        case (_, _) => Type.mkApply(Type.Or, List(lowRes, highRes), SourceLocation.Unknown)
      }
    }

    override def satisfiable(f: BddFormula): Boolean = !f.getDD().isZero()

    override def reset(): Unit = {
      factory.reset()
      factory.setVarNum(20)
    }
  }
}
