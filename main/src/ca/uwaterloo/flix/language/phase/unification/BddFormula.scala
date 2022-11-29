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

import scala.collection.immutable.SortedSet
import org.sosy_lab.pjbdd.api.{Builders, DD}

object BddFormula {

  val creator = Builders.bddBuilder().build()

  class BddFormula(val dd: DD) extends Formula {
    def getDD(): DD = dd
  }

  implicit val AsBoolAlg: BoolAlg[BddFormula] = new BoolAlg[BddFormula] {
    /**
      * Returns `true` if `f` represents TRUE.
      */
    override def isTrue(f: BddFormula): Boolean = f.getDD().isTrue()

    /**
      * Returns `true` if `f` represents FALSE.
      */
    override def isFalse(f: BddFormula): Boolean = f.getDD().isFalse()

    /**
      * Returns `true` if `f` represents a variable.
      */
    override def isVar(f: BddFormula): Boolean =
      f.getDD().equalsTo(f.getDD().getVariable, creator.makeFalse(), creator.makeTrue())

    /**
      * Returns a representation of TRUE.
      */
    override def mkTrue: BddFormula = {
      new BddFormula(creator.makeTrue())
    }

    /**
      * Returns a representation of FALSE.
      */
    override def mkFalse: BddFormula = {
      new BddFormula(creator.makeFalse())
    }

    /**
      * Returns a representation of the variable with the given `id`.
      */
    override def mkVar(id: Int): BddFormula = {
      new BddFormula(creator.makeIthVar(id))
    }

    /**
      * Returns a representation of the complement of `f`.
      */
    override def mkNot(f: BddFormula): BddFormula = {
      new BddFormula(creator.makeNot(f.getDD()))
    }

    /**
      * Returns a representation of the disjunction of `f1` and `f2`.
      */
    override def mkOr(f1: BddFormula, f2: BddFormula): BddFormula = {
      new BddFormula(creator.makeOr(f1.getDD(), f2.getDD()))
    }

    /**
      * Returns a representation of the conjunction of `f1` and `f2`.
      */
    override def mkAnd(f1: BddFormula, f2: BddFormula): BddFormula = {
      new BddFormula(creator.makeAnd(f1.getDD(), f2.getDD()))
    }

    /**
      * Returns a representation of the formula `f1 == f2`.
      */
    override def mkXor(f1: BddFormula, f2: BddFormula): BddFormula = {
      new BddFormula(creator.makeXor(f1.getDD(), f2.getDD()))
    }

    /**
      * Returns the set of free variables in `f`.
      */
    override def freeVars(f: BddFormula): SortedSet[Int] = {
      freeVarsAux(f.getDD())
    }

    private def freeVarsAux(dd: DD): SortedSet[Int] = {
      if (dd.isLeaf()) {
        SortedSet.empty
      } else {
        SortedSet(dd.getVariable()) ++
          freeVarsAux(dd.getLow) ++
          freeVarsAux(dd.getHigh)
      }
    }

    /**
      * Applies the function `fn` to every variable in `f`.
      */
    override def map(f: BddFormula)(fn: Int => BddFormula): BddFormula = {
      new BddFormula(mapAux(f.getDD())(fn))
    }

    private def mapAux(dd: DD)(fn: Int => BddFormula): DD = {
      if (dd.isLeaf()) {
        dd
      } else {
        val currentVar = dd.getVariable()
        val substDD = fn(currentVar).getDD()

        val lowRes = mapAux(dd.getLow())(fn)
        val highRes = mapAux(dd.getHigh())(fn)
        creator.makeIte(substDD, highRes, lowRes)
      }
    }

    /**
      * Returns a representation equivalent to `f` (but potentially smaller).
      */
    override def minimize(f: BddFormula): BddFormula = f

    /**
      * Converts the given formula f into a type.
      */
    override def toType(f: BddFormula, env: Bimap[Symbol.KindedTypeVarSym, Int]): Type = {
      createTypeFromBDDAux(f.getDD(), Type.True, env)
    }

    //TODO: Optimize
    private def createTypeFromBDDAux(dd: DD, tpe: Type, env: Bimap[Symbol.KindedTypeVarSym, Int]): Type = {
      if (dd.isLeaf()) {
        return if (dd.isTrue()) tpe else Type.False
      }

      val currentVar = dd.getVariable()
      val typeVar = env.getBackward(currentVar) match {
        case Some(sym) => Type.Var(sym, SourceLocation.Unknown)
        case None => throw InternalCompilerException(s"unexpected unknown ID: $currentVar")
      }

      val lowType = Type.mkApply(Type.And, List(tpe, Type.Apply(Type.Not, typeVar, SourceLocation.Unknown)), SourceLocation.Unknown)
      val lowRes = createTypeFromBDDAux(dd.getLow(), lowType, env)
      val highType = Type.mkApply(Type.And, List(tpe, typeVar), SourceLocation.Unknown)
      val highRes = createTypeFromBDDAux(dd.getHigh(), highType, env)

      (lowRes, highRes) match {
        case (Type.False, Type.False) => Type.False
        case (Type.False, _) => highRes
        case (_, Type.False) => lowRes
        case (_, _) => Type.mkApply(Type.Or, List(lowRes, highRes), SourceLocation.Unknown)
      }
    }

    /**
      * Optional operation. Returns `None` if not implemented.
      *
      * Returns `Some(true)` if `f` is satisfiable (i.e. has a satisfying assignment).
      * Returns `Some(false)` otherwise.
      */
    override def satisfiable(f: BddFormula): Boolean = !f.getDD().isFalse()

  }
}
