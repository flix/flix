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

import scala.collection.immutable.SortedSet
import java.util.concurrent.locks.ReentrantLock
import scala.sys.exit

object BddFormula {

  val factory: BDDFactory = JFactory.init(1000,100)
  factory.setVarNum(100)
  val lock = new ReentrantLock()

  class BddFormula(val dd: BDD) {
    def getDD(): BDD = dd
  }

  implicit val AsBoolAlg: BoolAlg[BddFormula] = new BoolAlg[BddFormula] {
    /**
      * Returns `true` if `f` represents TRUE.
      */
    override def isTrue(f: BddFormula): Boolean = f.getDD().isOne

    /**
      * Returns `true` if `f` represents FALSE.
      */
    override def isFalse(f: BddFormula): Boolean = f.getDD().isZero

    /**
      * Returns `true` if `f` represents a variable.
      */
    override def isVar(f: BddFormula): Boolean = f.getDD().equalsBDD(factory.ithVar(f.getDD().`var`()))

    /**
      * Returns a representation of TRUE.
      */
    override def mkTrue: BddFormula = {
      new BddFormula(factory.one())
    }

    /**
      * Returns a representation of FALSE.
      */
    override def mkFalse: BddFormula = {
      new BddFormula(factory.zero())
    }

    /**
      * Returns a representation of the variable with the given `id`.
      */
    override def mkVar(id: Int): BddFormula = {
      new BddFormula(factory.ithVar(id))
    }

    /**
      * Returns a representation of the complement of `f`.
      */
    override def mkNot(f: BddFormula): BddFormula = {
      new BddFormula(f.getDD().not())
    }

    /**
      * Returns a representation of the disjunction of `f1` and `f2`.
      */
    override def mkOr(f1: BddFormula, f2: BddFormula): BddFormula = {
      new BddFormula(f1.getDD().and(f2.getDD()))
    }

    /**
      * Returns a representation of the conjunction of `f1` and `f2`.
      */
    override def mkAnd(f1: BddFormula, f2: BddFormula): BddFormula = {
      new BddFormula(f1.getDD().or(f2.getDD()))
    }

    /**
      * Returns a representation of the formula `f1 == f2`.
      */
    override def mkXor(f1: BddFormula, f2: BddFormula): BddFormula = {
      new BddFormula(f1.getDD().xor(f2.getDD()))
    }

    /**
      * Returns the set of free variables in `f`.
      */
    //TODO: Optimize!
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

    /**
      * Applies the function `fn` to every variable in `f`.
      */
    override def map(f: BddFormula)(fn: Int => BddFormula): BddFormula = {
      /*println("New f")
      f.getDD().printDot()*/
      val res = mapAux(f.getDD())(fn)
      /*println("Mapped f")
      res.printDot()
      println()*/
      new BddFormula(res)
    }

    private def mapAux(dd: BDD)(fn: Int => BddFormula): BDD = {
      if (dd.isOne() || dd.isZero()) {
        dd
      } else {
        val currentVar = dd.`var`()
        val substDD = fn(currentVar).getDD()
        /*println("var " + currentVar + " ->")
        substDD.printDot()*/

        if(substDD.isOne()) {
          val res = mapAux(dd.high())(fn)
          /*println("Set " + currentVar + " to T")
          res.printDot()*/
          res
        } else if(substDD.isZero()) {
          val res = mapAux(dd.low())(fn)
          /*println("Set " + currentVar + " to F")
          res.printDot()*/
          res
        } else {
          val lowRes = mapAux(dd.low())(fn)
          val highRes = mapAux(dd.high())(fn)
          val res = substDD.ite(highRes, lowRes)
          /*println("Result after mapping " + currentVar)
          res.printDot()*/

          res
        }
      }
    }

    /**
      * Applies the function `fn` to every variable in `f`.
      */
      //TODO: Check correctness
    /*override def map(f: BddFormula)(fn: Int => BddFormula): BddFormula = {
      if(f.getDD().isOne || f.getDD().isZero) {
        f
      } else {
        val varSetF = freeVars(f)

        var varSetFull = varSetF
        for(i <- varSetF) {
          val subst = fn(i)
          val varSetI = freeVars(subst)
          varSetFull = varSetFull ++ varSetI
        }

        //make x -> x' map
        val maxVar = varSetFull.max
        val noVars = varSetFull.size
        val newVarNames = (maxVar+1 to maxVar+noVars).toList
        val varMap = varSetFull.zip(newVarNames).foldLeft(Bimap.empty[Int, Int]) {
          case (macc, (old_x, new_x)) => macc + (old_x -> new_x)
        }

        var res = f.getDD()

        //for each i in varSet create BddFormula' with primed variables
        //and compose f with BddFormula'
        for (var_i <- varSetF) {
          val subst = fn(var_i)
          val substVarSet = freeVars(subst)

          var substDD = subst.getDD()

          //create the substitute BDD with the new names
          for (var_j <- substVarSet) {
            val j_prime = varMap.getForward(var_j) match {
              case Some(j) => j
              case None => ??? //should never happen
            }
            substDD = substDD.compose(factory.ithVar(j_prime), var_j)
          }
          res = res.compose(substDD, var_i)
        }

        val varSetPrime = freeVarsAux(res)

        //for each x' map back to x in f
        for (var_i_prime <- varSetPrime) {
          val old_i = varMap.getBackward(var_i_prime) match {
            case Some(i) => i
            case None => ??? //should never happen
          }
          res = res.compose(factory.ithVar(old_i), var_i_prime)
        }

        val resForm = new BddFormula(res)
        resForm
      }
    }*/

    /**
      * Returns a representation equivalent to `f` (but potentially smaller).
      */
    override def minimize(f: BddFormula): BddFormula = f

    /**
      * Returns an environment built from the given types mapping between type variables and formula variables.
      *
      * This environment should be used in the functions [[toType]] and [[fromType]].
      */
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

    /**
      * Converts the given formula f into a type.
      */
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
        case None => throw InternalCompilerException(s"unexpected unknown ID: $currentVar")
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

    /**
      * Optional operation. Returns `None` if not implemented.
      *
      * Returns `Some(true)` if `f` is satisfiable (i.e. has a satisfying assignment).
      * Returns `Some(false)` otherwise.
      */
    override def satisfiable(f: BddFormula): Boolean = !f.getDD().isZero()

  }
}
