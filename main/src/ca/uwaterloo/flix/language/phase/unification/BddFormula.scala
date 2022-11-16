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
import org.sosy_lab.pjbdd.util.parser.DotExporter

import java.util.concurrent.locks.ReentrantLock
import scala.sys.exit

object BddFormula {

  val creator = Builders.bddBuilder().build()
  val lock = new ReentrantLock()
  val exp = new DotExporter

  class BddFormula(val dd: DD) {
    def getDD(): DD = dd
  }

  implicit val AsBoolAlgTrait: BoolAlg[BddFormula] = new BoolAlg[BddFormula] {
    /**
      * Returns `true` if `f` represents TRUE.
      */
    override def isTrue(f: BddFormula): Boolean = f.getDD().isTrue()

    /**
      * Returns `true` if `f` represents FALSE.
      */
    override def isFalse(f: BddFormula): Boolean = f.getDD().isFalse()

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
    //TODO: Optimize!
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
      val creator = Builders.bddBuilder().build()
      val x1 = creator.makeIthVar(1)
      val x2 = creator.makeIthVar(2)
      val bot = creator.makeFalse()
      val ite_pjbdd = creator.makeIte(x1, x2, bot)
      val ite_formula = creator.makeOr(creator.makeAnd(x1, x2), creator.makeAnd(creator.makeNot(x1), bot))

      println("If x1 then x2 else false by formula")
      println(exp.bddToString(ite_formula))

      println("If x1 then x2 else false by PJBDD")
      println(exp.bddToString(ite_pjbdd))

      exit(-1)


      //lock.lock()
      println("f")
      println(exp.bddToString(f.getDD()))
      val res = new BddFormula(mapAux(f.getDD())(fn))
      println("mapped f")
      println(exp.bddToString(res.getDD()))
      println()
      //lock.unlock()
      res
    }

    private def mapAux(dd: DD)(fn: Int => BddFormula): DD = {
      if (dd.isLeaf()) {
        dd
      } else {
        val currentVar = dd.getVariable()
        val substDD = fn(currentVar).getDD()

        println(currentVar + " -> ")
        println(exp.bddToString(substDD))

        val lowRes = mapAux(dd.getLow())(fn)
        val highRes = mapAux(dd.getHigh())(fn)
        val res = creator.makeIte(substDD, highRes, lowRes)

        println("result after subst on " + currentVar)
        println(exp.bddToString(res))

        res
      }
    }

    //TODO: Check correctness
    /*override def map(f: BddFormula)(fn: Int => BddFormula): BddFormula = {
      if(f.getDD().isLeaf()) {
        f
      } else {
        val varSet = freeVars(f)
        var varSetFull = varSet
        for(var_i <- varSet) {
          val varSetF = freeVars(fn(var_i))
          varSetFull = varSetFull ++ varSetF
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
        for (var_i <- varSet) {
          val subst = fn(var_i)
          val substVarSet = freeVars(subst)
          var substDD = subst.getDD()

          if(substDD.equals(creator.makeTrue())) {
            res = creator.restrict(res, var_i, true)
          } else if(substDD.equals(creator.makeFalse())) {
            res = creator.restrict(res, var_i, false)
          } else if(!substDD.equals(creator.makeIthVar(var_i))) {

            //create the substitute BDD with the new names
            for (var_j <- substVarSet) {
              val j_prime = varMap.getForward(var_j) match {
                case Some(j) => j
                case None => ??? //should never happen
              }
              substDD = creator.makeReplace(substDD, creator.makeIthVar(var_j), creator.makeIthVar(j_prime))
            }
            res = creator.makeCompose(res, var_i, substDD)

          }
        }

        val varSetPrime = freeVarsAux(res)

        //for each x' map back to x in f
        for (var_i_prime <- varSetPrime) {
          res = varMap.getBackward(var_i_prime) match {
            case Some(old_i) => creator.makeReplace(res, creator.makeIthVar(var_i_prime), creator.makeIthVar(old_i))
            case None => res //if the variable was never replaced
          }
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
    override def satisfiable(f: BddFormula): Option[Boolean] = Some(!f.getDD().isFalse())
  }
}
