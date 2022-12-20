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

import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.collection.Bimap

import scala.collection.mutable
import scala.collection.immutable.IntMap

sealed trait BoolVal

object BoolVal {
  case object True extends BoolVal

  case object False extends BoolVal

  case object DontCare extends BoolVal
}

object QuineMcCluskey {
  val Global: QuineMcCluskey = new QuineMcCluskey()
}

class QuineMcCluskey {

  /**
    * The Quine-McCluskey algorithm
    * Takes the min terms as input
    * Collect prime implicants
    * and use them to find a cover,
    * then translates the cover to a
    * Type based on the environment
    * Note: the implementation does not find a
    * minimal, but instead a greedy cover
    */
  def qmc(minTerms: Set[(IntMap[BoolVal], Int)], env: Bimap[Symbol.KindedTypeVarSym, Int]): Type = {
    val primeImplicants = collectPrimeImplicants(minTerms.filter(ms => ms._2 == 0), minTerms.filter(ms => ms._2 != 0))
    val cover = findCover(minTerms, primeImplicants)
    coverToType(cover, env)
  }

  /**
    * Converting a cover to a Type by making each prime
    * implicant into an AND and OR'ing them together
    */
  private def coverToType(cover: Set[IntMap[BoolVal]], env: Bimap[Symbol.KindedTypeVarSym, Int]): Type = {
    val typeList = cover.foldLeft(List.empty[Type])((acc, m) => acc ++ List(primeImpToType(m, env)))
    if (typeList.size == 1) {
      typeList.head
    } else {
      Type.mkOr(typeList, SourceLocation.Unknown)
    }
  }

  /**
    * Converting a prime implicant to an AND Type
    * "Don't care"'s are thrown away, 0's are mapped to
    * NOTs of vars and 1's are mapped to vars
    */
  private def primeImpToType(primeImp: IntMap[BoolVal], env: Bimap[Symbol.KindedTypeVarSym, Int]): Type = {
    val typeVars = primeImp.filter(kv => kv._2 != BoolVal.DontCare).map[Type](kv => {
      val symVar = env.getBackward(kv._1).getOrElse(throw InternalCompilerException(s"unexpected unknown ID: ${kv._1}", SourceLocation.Unknown))
      if (kv._2 == BoolVal.False) {
        Type.mkNot(Type.Var(symVar, symVar.loc), symVar.loc)
      } else {
        Type.Var(symVar, symVar.loc)
      }
    }).toList
    Type.mkAnd(typeVars, SourceLocation.Unknown)
  }

  /**
    * Finds the prime implicants from the min terms.
    * The min terms are given as two sets: those with
    * size (# of "don't cares") 0 and those with larger
    * size.
    */
  private def collectPrimeImplicants(size0: Set[(IntMap[BoolVal], Int)], larger: Set[(IntMap[BoolVal], Int)]): Set[IntMap[BoolVal]] = {
    //The min terms with the current size
    var thisLevel: mutable.Set[(IntMap[BoolVal], Int)] = mutable.Set.empty
    for (s <- size0) {
      thisLevel.add(s)
    }
    var nextLevel: mutable.Set[(IntMap[BoolVal], Int)] = mutable.Set.empty
    var currentSize = 0
    val primeImplicants: mutable.Set[IntMap[BoolVal]] = mutable.Set.empty

    while (thisLevel.nonEmpty) {
      //Compare all pairs of terms on this level to see
      //whether they can be merged. If they can add them
      //to the next level. If any term cannot be merged
      //at all add it to the set of prime implicants.
      //Note: this is too many comparisons - optimally
      //we should only compare each term to those that
      //one more 1 than them.
      for ((m1, _) <- thisLevel) {
        var used = false
        for ((m2, _) <- thisLevel) {
          val newMap = offByOne(m1, m2)
          if (newMap.nonEmpty) {
            used = true
            nextLevel = nextLevel ++ Set((newMap, currentSize + 1))
          }
        }
        if (!used) {
          primeImplicants.add(m1)
        }
      }

      thisLevel = nextLevel
      nextLevel = mutable.Set.empty
      currentSize = currentSize + 1

      //Add those terms where the size is now correct
      for ((map, size) <- larger) {
        if (size == currentSize) {
          thisLevel = thisLevel ++ Set((map, size))
        }
      }
    }

    //If there are any terms that were too large to ever
    //be used, add them to the result
    for ((map, size) <- larger) {
      if (size > currentSize) {
        primeImplicants.add(map)
      }
    }

    primeImplicants.toSet
  }

  /**
    * Checks whether two terms of the same size have only
    * one variable where their values differ and returns
    * the merged term if so.
    */
  private def offByOne(i: IntMap[BoolVal], j: IntMap[BoolVal]): IntMap[BoolVal] = {
    var eqSoFar = true
    var changedVar: Int = -1

    for (x <- i.keySet) {
      if (i(x) != j(x)) {
        if (eqSoFar) {
          eqSoFar = false
          changedVar = x
        } else {
          return IntMap.empty
        }
      }
    }

    if (eqSoFar) {
      IntMap.empty
    } else {
      i.updated(changedVar, BoolVal.DontCare)
    }
  }

  /**
    * Finds a small cover for the given min terms
    * The cover is not minimal, but greedy
    */
  private def findCover(minTerms: Set[(IntMap[BoolVal], Int)], primeImplicants: Set[IntMap[BoolVal]]): Set[IntMap[BoolVal]] = {
    val cover: mutable.Set[IntMap[BoolVal]] = mutable.Set.empty

    //For each min term find the PIs that cover it
    val coverMap: mutable.Map[IntMap[BoolVal], Set[IntMap[BoolVal]]] = mutable.Map.empty
    for ((term, _) <- minTerms) {
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

    //Cover the rest of the min terms greedily (choosing the PI)
    //that covers as many min terms as possible
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
