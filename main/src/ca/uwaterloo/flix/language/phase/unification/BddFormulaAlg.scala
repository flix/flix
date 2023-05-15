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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.collection.Bimap
import org.sosy_lab.pjbdd.api.{Builders, Creator, DD}

import scala.collection.immutable.{IntMap, SortedSet}

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
final class BddFormulaAlg(implicit flix: Flix) extends BoolAlg[DD] {

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
    if(!flix.options.xnoqmc) {
      toTypeQMC(f, env)
    } else {
      createTypeFromBDDAux(f, Type.Empty, env)
    }
  }

  /**
    * Collects true paths in the BDD and ORs them together:
    * Traverses every path through the BDD and keeps track of the
    * variables encountered and their values. When a leaf is hit,
    * if it is true returns the path, otherwise returns false.
    * ORs all returned paths together.
    */
  private def createTypeFromBDDAux(dd: DD, tpe: Type, env: Bimap[Symbol.KindedTypeVarSym, Int]): Type = {
    if (dd.isLeaf) {
      return if (dd.isTrue) tpe else Type.All
    }

    val currentVar = dd.getVariable
    val typeVar = env.getBackward(currentVar) match {
      case Some(sym) => Type.Var(sym, sym.loc)
      case None => throw InternalCompilerException(s"unexpected unknown ID: $currentVar", tpe.loc)
    }

    val lowType = Type.mkApply(Type.Union, List(tpe, Type.Apply(Type.Complement, typeVar, typeVar.loc)), typeVar.loc)
    val lowRes = createTypeFromBDDAux(dd.getLow, lowType, env)
    val highType = Type.mkApply(Type.Union, List(tpe, typeVar), typeVar.loc)
    val highRes = createTypeFromBDDAux(dd.getHigh, highType, env)

    (lowRes, highRes) match {
      case (Type.All, Type.All) => Type.All
      case (Type.All, _) => highRes
      case (_, Type.All) => lowRes
      case (t1, _) => Type.mkApply(Type.Intersection, List(lowRes, highRes), t1.loc)
    }
  }

  /**
    * Converting a BDD to a Type using the Quine-McCluskey algorithm
    */
  private def toTypeQMC(f: DD, env: Bimap[Symbol.KindedTypeVarSym, Int]): Type = {
    //Easy shortcuts if formula is true, false or a variable
    if (f.isLeaf) {
      if (f.isTrue) {
        return Type.Empty
      } else {
        return Type.All
      }
    }
    if (isVar(f)) {
      val id = f.getVariable
      val typeVar = env.getBackward(id).getOrElse(throw InternalCompilerException(s"unexpected unknown ID: $id", SourceLocation.Unknown))
      return Type.Var(typeVar, typeVar.loc)
    }

    //Otherwise find the cover and convert it to a Type
    val minTerms = collectMinTerms(f, freeVars(f))
    QuineMcCluskey.qmcToType(minTerms, env)
  }

  /**
    * Collects the min terms and updates them to all
    * have the same key set (the variables in the BDD).
    * Any variables that were not already in a min term
    * are mapped to 2 ("don't care"). Also gives the
    * number of "don't care" variables in each min term.
    */
  private def collectMinTerms(f: DD, vars: SortedSet[Int]): Set[IntMap[BoolVal]] = {
    val terms = collectTerms(f, vars, IntMap.empty)
    val noEmptyTerms = terms.filter(t => t.nonEmpty)
    noEmptyTerms.map { t =>
      val keys = t.keySet
      val dontCares = vars -- keys
      val dontCareMap = dontCares.foldLeft(IntMap.empty[BoolVal])((acc, dc) => acc ++ IntMap(dc -> BoolVal.DontCare))
      t ++ dontCareMap
    }
  }

  /**
    * Checks all paths in the BDD and returns an IntMap for each true path.
    * In the map variables that were true on the path are mapped to 1,
    * and variables that were false are mapped to 0.
    */
  private def collectTerms(f: DD, vars: SortedSet[Int], termSoFar: IntMap[BoolVal]): Set[IntMap[BoolVal]] = {
    if (f.isLeaf) {
      if (f.isTrue) {
        Set(termSoFar)
      } else {
        Set(IntMap.empty)
      }
    } else {
      val x = f.getVariable
      val lowRes = collectTerms(f.getLow, vars, termSoFar.updated(x, BoolVal.False))
      val highRes = collectTerms(f.getHigh, vars, termSoFar.updated(x, BoolVal.True))

      lowRes ++ highRes
    }
  }
}
