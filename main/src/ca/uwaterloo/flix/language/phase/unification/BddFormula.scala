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
import scala.jdk.CollectionConverters._
import com.juliasoft.beedeedee.bdd.BDD
import com.juliasoft.beedeedee.factories.Factory

object BddFormula {

  val factory: Factory = Factory.mk(1000 * 1000, 100000)

  class BddFormula(val dd: BDD) {
    def getDD(): BDD = dd
  }

  implicit val AsBoolAlgTrait: BoolAlg[BddFormula] = new BoolAlg[BddFormula] {
    /**
      * Returns `true` if `f` represents TRUE.
      */
    override def isTrue(f: BddFormula): Boolean = f.getDD().isOne()

    /**
      * Returns `true` if `f` represents FALSE.
      */
    override def isFalse(f: BddFormula): Boolean = f.getDD().isZero()

    /**
      * Returns a representation of TRUE.
      */
    override def mkTrue: BddFormula = {
      new BddFormula(factory.makeOne())
    }

    /**
      * Returns a representation of FALSE.
      */
    override def mkFalse: BddFormula = {
      new BddFormula(factory.makeZero())
    }

    /**
      * Returns a representation of the variable with the given `id`.
      */
    override def mkVar(id: Int): BddFormula = {
      new BddFormula(factory.makeVar(id))
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
      new BddFormula(f1.getDD().or(f2.getDD()))
    }

    /**
      * Returns a representation of the conjunction of `f1` and `f2`.
      */
    override def mkAnd(f1: BddFormula, f2: BddFormula): BddFormula = {
      new BddFormula(f1.getDD().and(f2.getDD()))
    }

    /**
      * Returns a representation of the formula `f1 xor f2`.
      */
    override def mkXor(f1: BddFormula, f2: BddFormula): BddFormula = {
      new BddFormula(f1.getDD().xor(f2.getDD()))
    }

    /**
      * Returns the set of free variables in `f`.
      */
    override def freeVars(f: BddFormula): SortedSet[Int] = {
      freeVarsAux(f.getDD())
    }

    /**
      * Helper function for freeVars (works on internal BDD)
      */
    def freeVarsAux(dd: BDD): SortedSet[Int] = {
      var res: SortedSet[Int] = SortedSet.empty
      if (dd.isOne || dd.isZero) {
        return res
      }
      val bitset = dd.vars()
      var i = bitset.nextSetBit(0)
      while (i >= 0) {
        res = res ++ SortedSet(i)
        i = bitset.nextSetBit(i + 1)
      }
      res
    }

    /**
      * Applies the function `fn` to every variable in `f`.
      */
    override def map(f: BddFormula)(fn: Int => BddFormula): BddFormula = {
      //if f is true or false, map has no effect
      if(f.getDD().isOne || f.getDD().isZero) {
        f
      } else {
        /*to avoid problems with overlapping variables in f and the results of fn
          each variable x in f or a result of fn on a variable from f, must get a
          fresh name x'. Then for each variable i in f, substitute all variables
          in fn(i) with their primed versions. Then substitute i for the primed
          version of fn(i). At the end go through all primed variables in f and
          replace them with their unprimed versions.
        */

        //collect all the variables in f and the substitutions for variables in f
        val varSetF = freeVars(f)
        var varSetFull = varSetF
        for(var_i <- varSetF) {
          val subst_i = fn(var_i)
          val varSet_i = freeVars(subst_i)
          varSetFull = varSetFull ++ varSet_i
        }

        //make x -> x' map and x' -> x map
        val maxVar = varSetFull.max
        val noVars = varSetFull.size
        val newVarNames = (maxVar+1 to maxVar+noVars).toList
        val varMapForward = varSetFull.zip(newVarNames).foldLeft(Map.empty[Int, Int]) {
          case (macc, (old_x, new_x)) => macc + (old_x -> new_x)
        }
        var varMapBackward = varSetFull.zip(newVarNames).foldLeft(Map.empty[Int, Int]) {
          case (macc, (old_x, new_x)) => macc + (new_x -> old_x)
        }

        //take a copy so f.DD does not get garbage collected
        var res = f.getDD().copy()

        //for each i in the variable set for f, get fn(i), create fn(i)'
        //with primed variables and compose f with fn(i)'
        //if fn(i) is true or false, just restrict f
        //if fn(i) = i, do not do anything
        for (var_i <- varSetF) {
          val subst = fn(var_i)
          val substDD = subst.getDD()
          if(substDD.isOne()) {
            val resRestrictT = res.restrict(var_i, true)
            res.free()
            res = resRestrictT
          } else if(substDD.isZero()) {
            val resRestrictF = res.restrict(var_i, false)
            res.free()
            res = resRestrictF
          } else if(!substDD.isEquivalentTo(factory.makeVar(var_i))) {
            val replacedSubst = substDD.replace(varMapForward.asJava.asInstanceOf[java.util.Map[Integer,Integer]])
            replacedSubst.free()
            val resCompose = res.compose(substDD, var_i)
            res.free()
            res = resCompose
          }
        }

        //replace all primed xs in the result with their unprimed versions
        //to avoid problems with the replace operation, handle all variables
        //that occur both in primed and unprimed versions separately
        val varSetRes = freeVarsAux(res)
        for(var_k <- varSetRes) {
          //check whether k is an unprimed variable
          if(varMapForward contains var_k) {
            val k_prime = varMapForward.get(var_k) match  {
              case Some(k) => k
              case None => ??? //cannot happen
            }

            //removed the primed version of k to avoid problems
            varMapBackward -= k_prime

            //replace any primed occurrences of k with the unprimed version
            if(varSetRes.contains(k_prime)) {
              val composeDD = factory.makeVar(var_k)
              composeDD.free()
              val resCompose = res.compose(composeDD, k_prime)
              res.free()
              res = resCompose
            }
          }
        }

        //now the backward map only contains the primed variables that
        //do not occur as unprimed so the replacement is safe
        res.replaceWith(varMapBackward.asJava.asInstanceOf[java.util.Map[Integer,Integer]])

        new BddFormula(res)
      }
    }

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

    /**
      * Helper function for toType that collects true paths in the BDD and OR them together
      * TODO: Optimize using 2-level minimization
      */
    private def createTypeFromBDDAux(dd: BDD, tpe: Type, env: Bimap[Symbol.KindedTypeVarSym, Int]): Type = {
      //at the bottom of a true path - return the collected path
      if(dd.isOne()) return tpe
      //at the bottom of a false path - throw away this path
      if(dd.isZero()) return Type.False

      //not at the bottom yet - get the current variable and
      //find the corresponding type var in the environment
      val currentVar = dd.`var`()
      val typeVar = env.getBackward(currentVar) match {
        case Some(sym) => Type.Var(sym, SourceLocation.Unknown)
        case None => throw InternalCompilerException(s"unexpected unknown ID: $currentVar")
      }

      //expand the path so far into low and high paths
      //(make and AND with the type var in positive or negative form)
      //and call recursively on the low and high branches
      val lowType = Type.mkApply(Type.And, List(tpe, Type.Apply(Type.Not, typeVar, SourceLocation.Unknown)), SourceLocation.Unknown)
      val lowRes = createTypeFromBDDAux(dd.low(), lowType, env)
      val highType = Type.mkApply(Type.And, List(tpe, typeVar), SourceLocation.Unknown)
      val highRes = createTypeFromBDDAux(dd.high(), highType, env)

      //if the result from either branch is false, just use the other
      //if both are not false, make an OR of them
      (lowRes, highRes) match {
        case (Type.False, _) => highRes
        case (_, Type.False) => lowRes
        case (_, _) => Type.mkApply(Type.Or, List(lowRes, highRes), SourceLocation.Unknown)
      }
    }

    /**
      * Returns `Some(true)` if `f` is satisfiable (i.e. has a satisfying assignment).
      * Returns `Some(false)` otherwise.
      */
    override def satisfiable(f: BddFormula): Option[Boolean] = Some(!isFalse(f))

  }
}
