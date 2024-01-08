/*
 *  Copyright 2022 Anna Blume Jakobsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.collection.Bimap

import scala.collection.mutable
import scala.collection.immutable.IntMap

/**
  * The representation of a variable being true, false
  * or either ("don't care").
  */
sealed trait BoolVal

object BoolVal {
  case object True extends BoolVal

  case object False extends BoolVal

  case object DontCare extends BoolVal
}

object QuineMcCluskey {

  /**
    * The Quine-McCluskey algorithm
    * Takes the min terms of a formula as input
    * Collects prime implicants and uses them
    * to find a cover, then translates the cover
    * to a Type based on the environment
    *
    * Note: the implementation does not find a
    * minimal, but instead a greedy cover
    */
  def qmcToType(minTerms: Set[IntMap[BoolVal]], env: Bimap[BoolFormula.VarOrEff, Int]): Type = {
    val primeImplicants = collectPrimeImplicants(minTerms)
    val cover = findCover(minTerms, primeImplicants)
    coverToType(cover, env)
  }

  /**
    * The Quine-McCluskey algorithm
    * Takes the min terms of a formula as input
    * Collects prime implicants and uses them
    * to find a cover, then translates the cover
    * to a BoolFormula
    *
    * Note: the implementation does not find a
    * minimal, but instead a greedy cover
    */
  def qmcToBoolFormula(minTerms: Set[IntMap[BoolVal]]): BoolFormula = {
    val primeImplicants = collectPrimeImplicants(minTerms)
    val cover = findCover(minTerms, primeImplicants)
    coverToBoolFormula(cover)
  }

  /**
    * Converting a cover to a Type by making each prime
    * implicant into an AND and OR'ing them together
    */
  private def coverToType(cover: Set[IntMap[BoolVal]], env: Bimap[BoolFormula.VarOrEff, Int]): Type = {
    val typeList = cover.foldLeft(List.empty[Type])((acc, m) => acc ++ List(primeImpToType(m, env)))
    if (typeList.size == 1) {
      typeList.head
    } else {
      Type.mkUnion(typeList, SourceLocation.Unknown)
    }
  }

  /**
    * Converting a prime implicant to an AND Type
    * "Don't care"'s are thrown away, 0's are mapped to
    * NOTs of vars and 1's are mapped to vars
    */
  private def primeImpToType(primeImp: IntMap[BoolVal], env: Bimap[BoolFormula.VarOrEff, Int]): Type = {
    val typeVars = primeImp.filter{ case (_, boolValue) => boolValue != BoolVal.DontCare}.map[Type]{
      case (formVar, boolValue) =>
      val tpe = env.getBackward(formVar) match {
        case Some(BoolFormula.VarOrEff.Var(sym)) => Type.Var(sym, SourceLocation.Unknown)
        case Some(BoolFormula.VarOrEff.Eff(sym)) => Type.Cst(TypeConstructor.Effect(sym), SourceLocation.Unknown)
        case None => throw InternalCompilerException(s"unexpected unknown ID: $formVar", SourceLocation.Unknown)
      }
      if (boolValue == BoolVal.False) {
        Type.mkComplement(tpe, SourceLocation.Unknown)
      } else {
        tpe
      }
    }.toList
    Type.mkIntersection(typeVars, SourceLocation.Unknown)
  }

  /**
    * Converting a cover to a BoolFormula by making each prime
    * implicant into an AND and OR'ing them together
    */
  private def coverToBoolFormula(cover: Set[IntMap[BoolVal]]): BoolFormula = {
    if(cover.isEmpty) {
      return BoolFormula.False
    }
    val formList = cover.foldLeft(List.empty[BoolFormula])((acc, m) => acc ++ List(primeImpToBoolFormula(m)))
    formList.reduce((b1, b2) => BoolFormula.Or(b1, b2))
  }

  /**
    * Converting a prime implicant to a BoolFormula
    * "Don't care"'s are thrown away, 0's are mapped to
    * NOTs of vars and 1's are mapped to vars and these
    * are AND'ed together
    */
  private def primeImpToBoolFormula(primeImp: IntMap[BoolVal]): BoolFormula = {
    val formVars: List[BoolFormula] = primeImp.filter{ case (_, boolValue) => boolValue != BoolVal.DontCare}.map[BoolFormula]{
      case (formVar, boolValue) =>
      if (boolValue == BoolVal.False) {
        BoolFormula.Not(BoolFormula.Var(formVar))
      } else {
        BoolFormula.Var(formVar)
      }
    }.toList
    if(formVars.isEmpty) {
      BoolFormula.True
    } else {
      formVars.reduce((b1, b2) => BoolFormula.And(b1, b2))
    }
  }

  /**
    * Finds the prime implicants from the min terms.
    * Checks every pair of min terms to see if they
    * can be combined until a fixpoint is reached.
    * Removes all terms from the result that are covered
    * by other terms.
    */
  private def collectPrimeImplicants(minTerms: Set[IntMap[BoolVal]]): Set[IntMap[BoolVal]] = {
    //keeps track of all seen terms
    val collectedSoFar: mutable.Set[IntMap[BoolVal]] = mutable.Set.empty
    //the terms where we have not checked for merges with collectedSoFar yet
    var newTerms: mutable.Set[IntMap[BoolVal]] = mutable.Set.empty
    //the resulting prime implicants
    val result: mutable.Set[IntMap[BoolVal]] = mutable.Set.empty

    collectedSoFar.addAll(minTerms)
    newTerms.addAll(minTerms)
    result.addAll(minTerms)

    var somethingChanged = true
    //next rounds "newTerms"
    var toAdd: mutable.Set[IntMap[BoolVal]] = mutable.Set.empty

    //Fixpoint algorithm - checks pairs from
    //collectedSoFar and newTerms to find new
    //ways to merge terms
    //When a new term is found remove the
    //inputs from "result" if they are covered by
    //the new term
    while(somethingChanged) {
      for(m1 <- collectedSoFar) {
        for(m2 <- newTerms) {
          val (newMap, i_covered, j_covered) = offByOne(m1, m2)
          if(newMap.nonEmpty) {
            if(!(collectedSoFar contains newMap)) {
              toAdd.add(newMap)
            }
            if(i_covered) {
              result.remove(m1)
            }
            if(j_covered) {
              result.remove(m2)
            }
          }
        }
      }

      //set-up for next round
      somethingChanged = toAdd.nonEmpty
      collectedSoFar.addAll(toAdd)
      newTerms = toAdd
      result.addAll(toAdd)
      toAdd = mutable.Set.empty
    }

    result.toSet
  }

  /**
    * Checks whether two terms can be merged to a new term
    * Returns the new term and whether the input terms
    * are covered by this new term
    * Returns an empty map to signal failure to merge
    */
  private def offByOne(i: IntMap[BoolVal], j: IntMap[BoolVal]): (IntMap[BoolVal], Boolean, Boolean) = {
    var newMap: IntMap[BoolVal] = IntMap.empty
    var eqSoFar = true
    var i_covered = true
    var j_covered = true

    //Check compatibility on every variable (the
    //set of variables is the same for any term)
    //If the two terms agree on the value for the
    //variable, add that value to the result
    //If they disagree, but one value is "don't care"
    //use the other value ("don't care" is not longer
    //covered)
    //If they disagree with true and false, return
    //failure if this is not the first time
    //If it is the first time, set the value to "don't
    //care" and keep going
    for(x <- i.keySet) {
      val newXVal: BoolVal = (i(x), j(x)) match {
        case (BoolVal.True, BoolVal.True) => BoolVal.True
        case (BoolVal.False, BoolVal.False) => BoolVal.False
        case (BoolVal.DontCare, BoolVal.DontCare) => BoolVal.DontCare

        case (y, BoolVal.DontCare) => j_covered = false; y
        case (BoolVal.DontCare, z) => i_covered = false; z

        case _ =>
          if(eqSoFar) {
            eqSoFar = false
            BoolVal.DontCare
          } else {
            return (IntMap.empty, false, false)
          }

      }
      newMap = newMap ++ IntMap((x, newXVal))
    }

    if(eqSoFar) {
      (IntMap.empty, false, false)
    } else {
      (newMap, i_covered, j_covered)
    }
  }

  /**
    * Finds a small cover for the given min terms
    * The cover is not minimal, but greedy
    */
  private def findCover(minTerms: Set[IntMap[BoolVal]], primeImplicants: Set[IntMap[BoolVal]]): Set[IntMap[BoolVal]] = {
    val cover: mutable.Set[IntMap[BoolVal]] = mutable.Set.empty

    //For each min term find the PIs that cover it
    val coverMap: mutable.Map[IntMap[BoolVal], Set[IntMap[BoolVal]]] = mutable.Map.empty
    for (term <- minTerms) {
      val coveredBy = canBeCoveredBy(term, primeImplicants)
      coverMap.update(term, coveredBy)
    }

    //Find all the min terms that have only one PI covering it
    //This PI must be in the cover
    val onlyCoveredOnce = coverMap.filter(kv => kv._2.size == 1)

    for ((_, m) <- onlyCoveredOnce) {
      //Add the PI to the cover
      cover.add(m.head)

      //Remove the covered min terms from the cover map
      val removeTerms = coverMap.foldLeft(Set.empty[IntMap[BoolVal]])((acc, termCover) => {
        val t0 = termCover._1
        val m0 = termCover._2
        acc ++ (if (m0.contains(m.head)) Set(t0) else Set.empty)
      })
      for (t0 <- removeTerms) {
        coverMap.remove(t0)
      }
    }

    //Cover the rest of the min terms greedily (choosing the PI
    //that covers as many min terms as possible)
    //Keep going until all min terms are covered
    while (coverMap.nonEmpty) {
      //Greedily choose a PI
      val toAdd = findBestPI(coverMap.toMap, primeImplicants -- cover)

      //Add the PI to the cover
      cover.add(toAdd)

      //Remove the covered min terms from the cover map
      val removeTerms = coverMap.foldLeft(Set.empty[IntMap[BoolVal]])((acc, termCover) => {
        val t0 = termCover._1
        val m0 = termCover._2
        acc ++ (if (m0.contains(toAdd)) Set(t0) else Set.empty)
      })
      for (t0 <- removeTerms) {
        coverMap.remove(t0)
      }
    }

    cover.toSet
  }

  /**
    * Find the prime implicant that covers the most
    * terms. This is a greedy approach and may not
    * lead to a minimal cover
    */
  private def findBestPI(coverMap: Map[IntMap[BoolVal], Set[IntMap[BoolVal]]], primeImplicants: Set[IntMap[BoolVal]]): IntMap[BoolVal] = {
    var bestSoFar = coverMap.head._2.head
    var bestCoverNumber = 0

    for (pi <- primeImplicants) {
      var coverNumber = 0
      for ((_, imps) <- coverMap) {
        if (imps.contains(pi)) {
          coverNumber = coverNumber + 1
        }
      }
      if (coverNumber > bestCoverNumber) {
        bestCoverNumber = coverNumber
        bestSoFar = pi
      }
    }

    bestSoFar
  }

  /**
    * Finds all the prime implicants that can cover
    * a min term and returns them in a set
    */
  private def canBeCoveredBy(term: IntMap[BoolVal], primeImplicants: Set[IntMap[BoolVal]]): Set[IntMap[BoolVal]] = {
    val coveredBy = mutable.Set.empty[IntMap[BoolVal]]
    for (pi <- primeImplicants) {
      //If any variable is not covered (either by the same
      // value or a "don't care"), the PI does not cover the term
      var isCover = true
      for ((k, v) <- term) {
        if (!(pi(k) == BoolVal.DontCare || pi(k) == v)) {
          isCover = false
        }
      }
      if (isCover) {
        coveredBy.add(pi)
      }
    }
    coveredBy.toSet
  }
}
