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

    /**
      * Returns the set of free variables in `f`.
      */
    override def freeVars(f: BddFormula): SortedSet[Int] = {
      var res : SortedSet[Int] = SortedSet.empty
      if (f.getDD().isOne || f.getDD().isZero) {
        return res
      }
      val bitset = f.getDD().vars()
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
      /*lock.lock()
      val x1 = factory.makeVar(1)
      val notx1 = x1.not()
      val x2 = factory.makeVar(2)
      val notx2 = x2.not()
      val nand = x1.nand(x2)
      printBdd(nand, "Original f: x1 NAND x2")

      val and1 = notx1.and(factory.makeOne())
      val and2 = x1.and(notx2)
      val or = and1.or(and2)
      printBdd(or, "f|1 <- x1 using formula")

      val res = nand.compose(x1, 1)
      printBdd(res, "f|1 <- x1 using makeCompose")

      System.exit(-1)

      //lock.unlock()*/

      if(f.getDD().isOne || f.getDD().isZero) {
        f
      } else {
        val varSetF = freeVars(f)
        //println("DD var set: " + varSetF)

        var varSetFull = varSetF
        for(var_i <- varSetF) {
          val subst_i = fn(var_i)
          val varSet_i = freeVars(subst_i)
          varSetFull = varSetFull ++ varSet_i
        }
        //println("Full var set: " + varSetFull)
        if(varSetF != varSetFull) {
          /*println("Var sets not equal")
          println("Orig: " + varSetF)
          println("Full: " + varSetFull)*/
        }

        //make x -> x' map
        val maxVar = varSetFull.max
        val noVars = varSetFull.size
        val newVarNames = (maxVar+1 to maxVar+noVars).toList
        val varMapForward = varSetFull.zip(newVarNames).foldLeft(Map.empty[Int, Int]) {
          case (macc, (old_x, new_x)) => macc + (old_x -> new_x)
        }
        val varMapBackward = varSetFull.zip(newVarNames).foldLeft(Map.empty[Int, Int]) {
          case (macc, (old_x, new_x)) => macc + (new_x -> old_x)
        }
        var res = f.getDD()
        //printBdd(res, "Original BDD")

        //for each i in varSet create BddFormula' with primed variables
        //and compose f with BddFormula'
        for (var_i <- varSetF) {
          val subst = fn(var_i)
          var substDD = subst.getDD()
          if(substDD.isOne()) {
            //println("Var: " + var_i + " = true")
            res = res.restrict(var_i, true)
          } else if(substDD.isZero()) {
            //println("Var: " + var_i + " = false")
            res = res.restrict(var_i, false)
          } else if(!substDD.isEquivalentTo(factory.makeVar(var_i))) {
            //println("Var: " + var_i)
            //printBdd(substDD, "Subst before")
            substDD = substDD.replace(varMapForward.asJava.asInstanceOf[java.util.Map[Integer,Integer]])
            //printBdd(substDD, "Subst after")
            res = res.compose(substDD, var_i)
            //printBdd(res, "Res after subst")
          }
        }

        //printBdd(res, "Res before replacement")
        res = res.replace(varMapBackward.asJava.asInstanceOf[java.util.Map[Integer,Integer]])
        //printBdd(res, "Res after replacement")
        //println()

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
    override def satisfiable(f: BddFormula): Option[Boolean] = Some(!f.getDD().isZero())

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
