/*
 *  Copyright 2022 Magnus Madsen
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
import org.sosy_lab.pjbdd.api.{Builders, Creator, DD}

import scala.collection.immutable.SortedSet

/**
  * Companion object of [[BddFormulaAlg]].
  */
object BddFormulaAlg {
  /**
    * Thread-safe factory for creating BDDs
    */
  val GlobalBddBuilder: Creator = Builders.bddBuilder().build()
}

/**
  * An implementation of the [[BoolAlg]] interface for [[BoolFormula]].
  */
final class BddFormulaAlg extends BoolAlg[DD] {

  override def isTrue(f: DD): Boolean = f.isTrue

  override def isFalse(f: DD): Boolean = f.isFalse

  /**
    * Checks that the children of f are F (low child) and T (high child).
    * A BDD is a DAG with two children of each node (low and high). A
    * node has a variable, where the low child is what happens if the
    * variable is assigned F and the high child is if the variable is
    * assigned T. Therefore, a BDD that represents a single variable
    * has that variable in the node and F as the low child and T as the
    * high child.
    */
  override def isVar(f: DD): Boolean =
    f.equalsTo(f.getVariable, BddFormulaAlg.GlobalBddBuilder.makeFalse(), BddFormulaAlg.GlobalBddBuilder.makeTrue())

  override def mkTrue: DD = BddFormulaAlg.GlobalBddBuilder.makeTrue()

  override def mkFalse: DD = BddFormulaAlg.GlobalBddBuilder.makeFalse()

  override def mkVar(id: Int): DD = BddFormulaAlg.GlobalBddBuilder.makeIthVar(id)

  override def mkNot(f: DD): DD = BddFormulaAlg.GlobalBddBuilder.makeNot(f)

  override def mkOr(f1: DD, f2: DD): DD = BddFormulaAlg.GlobalBddBuilder.makeOr(f1, f2)

  override def mkAnd(f1: DD, f2: DD): DD = BddFormulaAlg.GlobalBddBuilder.makeAnd(f1, f2)

  override def mkXor(f1: DD, f2: DD): DD = BddFormulaAlg.GlobalBddBuilder.makeXor(f1, f2)

  /**
    * Traverses the entire BDD and collects its variables.
    * Each node has a variable (some variables may be in many nodes),
    * so we must traverse the entire BDD to get all variables.
    */
  override def freeVars(f: DD): SortedSet[Int] = {
    if (f.isLeaf) {
      SortedSet.empty
    } else {
      SortedSet(f.getVariable) ++
        freeVars(f.getLow) ++
        freeVars(f.getHigh)
    }
  }

  /**
    * Replaces each node v in the BDD with an ITE:
    * if fn(v) then map(v.high)(fn) else map(v.low)(fn)
    * If a variable x should be replaced by a formula g,
    * then all the true paths of g should lead to whatever
    * x's high child is, and all the false paths of g should
    * lead to whatever x's low child is, which is what ITE
    * does. Mapping leaves (F or T) has no effect.
    */
  override def map(f: DD)(fn: Int => DD): DD = {
    if (f.isLeaf) {
      f
    } else {
      val currentVar = f.getVariable
      val substDD = fn(currentVar)

      val lowRes = map(f.getLow)(fn)
      val highRes = map(f.getHigh)(fn)
      BddFormulaAlg.GlobalBddBuilder.makeIte(substDD, highRes, lowRes)
    }
  }

  /**
    * BDDs are always minimal.
    */
  override def minimize(f: DD): DD = f

  /**
    * A BDD is satisfiable if it is not F, since BDDs are always minimal.
    */
  override def satisfiable(f: DD): Boolean = !f.isFalse

  override def toType(f: DD, env: Bimap[Symbol.KindedTypeVarSym, Int]): Type = {
    //createTypeFromBDDAux(f, Type.True, env)
    toTypeQMC(f, env)
  }

  //TODO: Optimize (2-level minimization)
  /**
    * Collects true paths in the BDD and ORs them together:
    * Traverses every path through the BDD and keeps track of the
    * variables encountered and their values. When a leaf is hit,
    * if it is true returns the path, otherwise returns false.
    * ORs all returned paths together.
    */
  private def createTypeFromBDDAux(dd: DD, tpe: Type, env: Bimap[Symbol.KindedTypeVarSym, Int]): Type = {
    if (dd.isLeaf) {
      return if (dd.isTrue) tpe else Type.False
    }

    val currentVar = dd.getVariable
    val typeVar = env.getBackward(currentVar) match {
      case Some(sym) => Type.Var(sym, sym.loc)
      case None => throw InternalCompilerException(s"unexpected unknown ID: $currentVar", tpe.loc)
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

  //Mutability is needed for parts of the QMC algorithm
  import scala.collection.mutable

  /**
    * Converting a BDD to a Type using the Quine-McCluskey algorithm
    */
  private def toTypeQMC(f: DD, env: Bimap[Symbol.KindedTypeVarSym, Int]): Type = {
    //Easy shortcuts if formula is true, false or a variable
    if (f.isLeaf) {
      if (f.isTrue) {
        return Type.True
      } else {
        return Type.False
      }
    }
    if (isVar(f)) {
      val id = f.getVariable
      val typeVar = env.getBackward(id).getOrElse(throw InternalCompilerException(s"unexpected unknown ID: $id", SourceLocation.Unknown))
      return Type.Var(typeVar, typeVar.loc)
    }

    //Otherwise find the cover and convert it to a Type
    val cover = qmc(f)
    coverToType(cover, env)
  }

  /**
    * Converting a cover to a Type by making each prime
    * implicant into an AND and OR'ing them together
    */
  private def coverToType(cover: Set[Map[Int, Int]], env: Bimap[Symbol.KindedTypeVarSym, Int]): Type = {
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
  private def primeImpToType(primeImp: Map[Int, Int], env: Bimap[Symbol.KindedTypeVarSym, Int]): Type = {
    val typeVars = primeImp.filter(kv => kv._2 != 2).map[Type](kv => {
      val symVar = env.getBackward(kv._1).getOrElse(throw InternalCompilerException(s"unexpected unknown ID: ${kv._1}", SourceLocation.Unknown))
      if (kv._2 == 0) {
        Type.mkNot(Type.Var(symVar, symVar.loc), symVar.loc)
      } else {
        Type.Var(symVar, symVar.loc)
      }
    }).toList
    Type.mkAnd(typeVars, SourceLocation.Unknown)
  }

  /**
    * The Quine-McCluskey algorithm
    * Collect all min terms and prime implicants
    * and use them to find a cover
    * Note: the implementation does not find a
    * minimal, but instead a greedy cover
    */
  private def qmc(f: DD): Set[Map[Int, Int]] = {
    val vars = freeVars(f)
    val minTerms = collectMinTerms(f, vars)
    val primeImplicants = collectPrimeImplicants(minTerms.filter(ms => ms._2 == 0), minTerms.filter(ms => ms._2 != 0))
    findCover(minTerms, primeImplicants)
  }

  /**
    * Collects the min terms and updates them to all
    * have the same key set (the variables in the BDD).
    * Any variables that were not already in a min term
    * are mapped to 2 ("don't care"). Also gives the
    * number of "don't care" variables in each min term.
    */
  private def collectMinTerms(f: DD, vars: SortedSet[Int]): Set[(Map[Int, Int], Int)] = {
    val terms = collectTerms(f, vars, Map.empty)
    val noEmptyTerms = terms.filter(t => t.nonEmpty)
    noEmptyTerms.map { t =>
      val keys = t.keySet
      val dontCares = vars -- keys
      val size = dontCares.size
      val dontCareMap = dontCares.foldLeft(Map.empty[Int, Int])((acc, dc) => acc ++ Map(dc -> 2))
      (t ++ dontCareMap, size)
    }
  }

  /**
    * Checks all paths in the BDD and returns a Map for each true path.
    * In the map variables that were true on the path are mapped to 1,
    * and variables that were false are mapped to 0.
    */
  private def collectTerms(f: DD, vars: SortedSet[Int], termSoFar: Map[Int, Int]): Set[Map[Int, Int]] = {
    if (f.isLeaf) {
      if (f.isTrue) {
        Set(termSoFar)
      } else {
        Set(Map.empty)
      }
    } else {
      val x = f.getVariable
      val lowRes = collectTerms(f.getLow, vars, termSoFar.updated(x, 0))
      val highRes = collectTerms(f.getHigh, vars, termSoFar.updated(x, 1))

      lowRes ++ highRes
    }
  }

  /**
    * Finds the prime implicants from the min terms.
    * The min terms are given as two sets: those with
    * size (# of "don't cares") 0 and those with larger
    * size.
    */
  private def collectPrimeImplicants(size0: Set[(Map[Int, Int], Int)], larger: Set[(Map[Int, Int], Int)]): Set[Map[Int, Int]] = {
    //The min terms with the current size
    var thisLevel: mutable.Set[(Map[Int, Int], Int)] = mutable.Set.empty
    for (s <- size0) {
      thisLevel.add(s)
    }
    var nextLevel: mutable.Set[(Map[Int, Int], Int)] = mutable.Set.empty
    var currentSize = 0
    val primeImplicants: mutable.Set[Map[Int, Int]] = mutable.Set.empty

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
  private def offByOne(i: Map[Int, Int], j: Map[Int, Int]): Map[Int, Int] = {
    var eqSoFar = true
    var changedVar: Int = -1

    for (x <- i.keySet) {
      if (i(x) != j(x)) {
        if (eqSoFar) {
          eqSoFar = false
          changedVar = x
        } else {
          return Map.empty
        }
      }
    }

    if (eqSoFar) {
      Map.empty
    } else {
      i.updated(changedVar, 2)
    }
  }

  /**
    * Finds a small cover for the given min terms
    * The cover is not minimal, but greedy
    */
  private def findCover(minTerms: Set[(Map[Int, Int], Int)], primeImplicants: Set[Map[Int, Int]]): Set[Map[Int, Int]] = {
    val cover: mutable.Set[Map[Int, Int]] = mutable.Set.empty

    //For each min term find the PIs that cover it
    val coverMap: mutable.Map[Map[Int, Int], Set[Map[Int, Int]]] = mutable.Map.empty
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
      val removeTerms = coverMap.foldLeft(Set.empty[Map[Int, Int]])((acc, termCover) => {
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
      val removeTerms = coverMap.foldLeft(Set.empty[Map[Int, Int]])((acc, termCover) => {
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
  private def findBestPI(coverMap: Map[Map[Int, Int], Set[Map[Int, Int]]], primeImplicants: Set[Map[Int, Int]]): Map[Int, Int] = {
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
  private def canBeCoveredBy(term: Map[Int, Int], primeImplicants: Set[Map[Int, Int]]): Set[Map[Int, Int]] = {
    val coveredBy = mutable.Set.empty[Map[Int, Int]]
    for (pi <- primeImplicants) {
      //If any variable is not covered (either by the same
      // value or a "don't care"), the PI does not cover the term
      var isCover = true
      for ((k, v) <- term) {
        if (!(pi(k) == 2 || pi(k) == v)) {
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
