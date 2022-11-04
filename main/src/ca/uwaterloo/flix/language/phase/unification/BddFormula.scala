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
import com.juliasoft.beedeedee.bdd.BDD
import com.juliasoft.beedeedee.factories.Factory

import java.util.concurrent.locks.ReentrantLock
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.sys.exit


object BddFormula {

  val factory: Factory = Factory.mk(1000 * 1000, 100000)
  val lock = new ReentrantLock()

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

    override def mkXor(f1: BddFormula, f2: BddFormula): BddFormula = {
      new BddFormula(f1.getDD().xor(f2.getDD()))
    }

    /**
      * Returns the set of free variables in `f`.
      */
    override def freeVars(f: BddFormula): SortedSet[Int] = {
      freeVarsAux(f.getDD())
    }

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
      //TODO: Check correctness
    override def map(f: BddFormula)(fn: Int => BddFormula): BddFormula = {
      if(f.getDD().isOne || f.getDD().isZero) {
        f
      } else {
        val varSetF = freeVars(f)

        var varSetFull = varSetF
        for(var_i <- varSetF) {
          val subst_i = fn(var_i)
          val varSet_i = freeVars(subst_i)
          varSetFull = varSetFull ++ varSet_i
        }

        //make x -> x' map
        val maxVar = varSetFull.max
        val noVars = varSetFull.size
        val newVarNames = (maxVar+1 to maxVar+noVars).toList
        val varMapForward = varSetFull.zip(newVarNames).foldLeft(Map.empty[Int, Int]) {
          case (macc, (old_x, new_x)) => macc + (old_x -> new_x)
        }
        val varMapBackward = varSetFull.zip(newVarNames).foldLeft(mutable.Map.empty[Int, Int]) {
          case (macc, (old_x, new_x)) => macc + (new_x -> old_x)
        }
        var res = f.getDD()

        //for each i in varSet create BddFormula' with primed variables
        //and compose f with BddFormula'
        for (var_i <- varSetF) {
          val subst = fn(var_i)
          var substDD = subst.getDD()
          if(substDD.isOne()) {
            res = res.restrict(var_i, true)
          } else if(substDD.isZero()) {
            res = res.restrict(var_i, false)
          } else if(!substDD.isEquivalentTo(factory.makeVar(var_i))) {
            substDD = substDD.replace(varMapForward.asJava.asInstanceOf[java.util.Map[Integer,Integer]])
            res = res.compose(substDD, var_i)
          }
        }

        val varSetRes = freeVarsAux(res)
        for(var_k <- varSetRes) {
          if(varMapForward contains var_k) {
            val k_prime = varMapForward.get(var_k) match  {
              case Some(k) => k
              case None => ??? //cannot happen
            }
            varMapBackward.remove(k_prime)
            if(varSetRes.contains(k_prime)) {
              res = res.compose(factory.makeVar(var_k), k_prime)
            }
          }
        }

        res = res.replace(varMapBackward.asJava.asInstanceOf[java.util.Map[Integer,Integer]])

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
      //printBdd(f.getDD(), "toType")
      createTypeFromBDDAux(f.getDD(), Type.True, env)
    }

    //TODO: Optimize
    private def createTypeFromBDDAux(dd: BDD, tpe: Type, env: Bimap[Symbol.KindedTypeVarSym, Int]): Type = {
      if(dd.isOne()) return tpe
      if(dd.isZero()) return Type.False

      val currentVar = dd.`var`()
      val typeVar = env.getBackward(currentVar) match {
        case Some(sym) => Type.Var(sym, SourceLocation.Unknown)
        case None => /*printBdd(dd, "Wrong dd"); println(env);*/ throw InternalCompilerException(s"unexpected unknown ID: $currentVar")
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
    override def satisfiable(f: BddFormula): Option[Boolean] = Some(!isFalse(f))

    def printBdd(bdd : BDD, title : String) {
      lock.lock()
      println(title)
      __printBdd(" ", bdd, false);
      lock.unlock()
    }

    //HELPER workhorse function for printing the bdd
    def __printBdd (prefix : String, bdd : BDD, isLeft : Boolean) {
      print(prefix)
      print(if (isLeft) "├─T─" else "└─F─")

      if (bdd.isOne || bdd.isZero) {
        println(bdd.isOne())
        return
      } else {
        println("x" + bdd.`var`())
      }

      val newString : String = if (isLeft) "│   " else "    "
      // enter the next tree level - left and right branch
      __printBdd(prefix + newString, bdd.high(), true)
      __printBdd(prefix + newString, bdd.low(), false)
    }
  }
}
