/*
 * Copyright 2015-2016 Magnus Madsen
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

package ca.uwaterloo.flix.runtime.datastore

import ca.uwaterloo.flix.language.ast.{ExecutableAst, Symbol}
import ca.uwaterloo.flix.runtime.{Value, Interpreter, Solver}

import scala.annotation.switch
import scala.collection.mutable

import java.util

import scala.reflect.ClassTag

class IndexedLattice[ValueType <: AnyRef](val lattice: ExecutableAst.Table.Lattice, indexes: Set[Int])(implicit sCtx: Solver.SolverContext, m: ClassTag[ValueType]) extends IndexedCollection[ValueType] {
  /**
    * A map from indexes to a map from keys to rows (represented as map from keys to elements).
    */
  private val store = mutable.Map.empty[Int, mutable.Map[Key[ValueType], mutable.Map[Key[ValueType], Array[ValueType]]]]

  /**
    * The number of key columns in the lattice.
    */
  private val numberOfKeys = lattice.keys.length

  /**
    * The number of element columns in the lattice.
    */
  private val numberOfElms = lattice.values.length

  /**
    * The lattice operations associated with each lattice.
    */
  private val latticeOps: Array[ExecutableAst.Definition.Lattice] = lattice.values.map {
    case ExecutableAst.Attribute(_, tpe) => sCtx.root.lattices(tpe)
  }

  /**
    * The bottom element(s).
    */
  private val Bot: Array[ValueType] = {
    val a = new Array[AnyRef](latticeOps.length)
    for ((l, i) <- latticeOps.zipWithIndex) {
      val defn = sCtx.root.constants(l.bot)
      a(i) = Interpreter.evalCall(defn, Array.empty, sCtx.root)
    }
    a.asInstanceOf[Array[ValueType]]
  }

  /**
    * The Leq operator(s). Must be defined as Flix functions.
    */
  private val Leq: Array[ExecutableAst.Definition.Constant] = {
    val a = new Array[ExecutableAst.Definition.Constant](latticeOps.length)
    for ((l, i) <- latticeOps.zipWithIndex) {
      a(i) = sCtx.root.constants(l.leq)
    }
    a
  }

  /**
    * The Lub operator(s). Must be defined as Flix functions.
    */
  private val Lub: Array[ExecutableAst.Definition.Constant] = {
    val a = new Array[ExecutableAst.Definition.Constant](latticeOps.length)
    for ((l, i) <- latticeOps.zipWithIndex) {
      a(i) = sCtx.root.constants(l.lub)
    }
    a
  }

  /**
    * The Glb operator(s). Must be defined as Flix functions.
    */
  private val Glb: Array[ExecutableAst.Definition.Constant] = {
    val a = new Array[ExecutableAst.Definition.Constant](latticeOps.length)
    for ((l, i) <- latticeOps.zipWithIndex) {
      a(i) = sCtx.root.constants(l.glb)
    }
    a
  }

  /**
    * Initialize the store for all indexes.
    */
  for (idx <- indexes) {
    store(idx) = mutable.Map.empty
  }

  /**
   * Returns the size of the relation.
   */
  // TODO: Optimize
  def getSize: Int = scan.size

  /**
    * Processes a new inferred `fact`.
    *
    * Adds the fact to the relation. All entries in the fact must be non-null.
    *
    * Returns `true` iff the fact did not already exist in the relation.
    */
  def inferredFact(fact: Array[ValueType]): Boolean = {
    // Get the index on every column.
    val pat = util.Arrays.copyOfRange(fact, 0, numberOfKeys)
    val idx = getExactIndex(indexes, pat)
    assert(idx != 0)

    // Lookup the lattice map (create it, if it doesn't exist).
    val ikey = keyOf(idx, fact)
    val map = store(idx).getOrElseUpdate(ikey, mutable.Map.empty)
    val key = keyPart(fact)

    // Lookup the old element (create it, if it doesn't exist).
    val newElm = elmPart(fact)
    val oldElm = map.getOrElseUpdate(key, Bot)

    // Compute the lub and check if it is subsumed by the old element.
    val result = lub(newElm, oldElm)
    if (!leq(result, oldElm)) {
      // Update all indexes.
      for (idx <- indexes) {
        val ikey = keyOf(idx, fact)
        val map = store(idx).getOrElseUpdate(ikey, mutable.Map.empty)
        map(key) = result
      }
      return true
    }

    return false
  }

  /**
    * Performs a lookup of the given pattern `pat`.
    *
    * If the pattern contains `null` entries these are interpreted as free variables.
    *
    * If the pattern contains lattice elements then greatest-lower-bound is computed for these.
    *
    * Returns an iterator over the matching rows.
    */
  def lookup(pat: Array[ValueType]): Iterator[Array[ValueType]] = {
    // check if there is an exact index.
    var idx = getExactIndex(indexes, pat)
    val table = if (idx != 0) {
      // use exact index.
      val ikey = keyOf(idx, pat)
      store(idx).getOrElse(ikey, mutable.Map.empty).iterator
    } else {
      // check if there is an approximate index.
      idx = getApproximateIndex(indexes, pat)
      if (idx != 0) {
        // use approximate index.
        val ikey = keyOf(idx, pat)
        store(idx).getOrElse(ikey, mutable.Map.empty).iterator
      } else {
        // perform full table scan.
        scan
      }
    }

    table filter {
      // match the keys
      case (keys, _) => matchKey(util.Arrays.copyOf[ValueType](pat, numberOfKeys), keys.toArray)
    } map {
      case (keys, elms) =>
        val elmsCopy = elms.clone()
        // match the elements possibly changing elms2 to their greatest lower bound.
        if (matchElms(elmPart(pat), elmsCopy)) (keys, elmsCopy) else null
    } filter {
      case e => e != null
    } map {
      case (keys, elms) =>
        val result = new Array[ValueType](numberOfKeys + numberOfElms)
        System.arraycopy(keys.toArray, 0, result, 0, numberOfKeys)
        System.arraycopy(elms, 0, result, numberOfKeys, numberOfElms)
        result.asInstanceOf[Array[ValueType]]
    }
  }

  /**
    * Returns all rows in the relation using a table scan.
    */
  def scan: Iterator[(Key[ValueType], Array[ValueType])] = store(indexes.head).iterator.flatMap {
    case (key, m) => m.iterator
  }

  /**
    * Returns the key part of the given array `a`.
    */
  def keyPart(a: Array[ValueType]): Key[ValueType] = {
    (numberOfKeys: @switch) match {
      case 1 => new Key1(a(0))
      case 2 => new Key2(a(0), a(1))
      case 3 => new Key3(a(0), a(1), a(2))
      case 4 => new Key4(a(0), a(1), a(2), a(3))
      case 5 => new Key5(a(0), a(1), a(2), a(3), a(4))
      case _ => throw new RuntimeException("Internal Error. Keys longer than 5 not supported.");
    }
  }

  /**
    * Returns the element part of the given array `a`.
    */
  def elmPart(a: Array[ValueType]): Array[ValueType] = {
    return util.Arrays.copyOfRange[ValueType](a, numberOfKeys, a.length)
  }

  /**
    * Returns `true` iff all non-null entries in the given pattern `pat`
    * are equal to their corresponding entry in the given `row`.
    */
  // TODO: Optimize by changing signature
  def matchKey(pat: Array[ValueType], row: Array[ValueType]): Boolean = {
    var i = 0
    while (i < pat.length) {
      val pv = pat(i)
      if (pv != null)
        if (pv != row(i))
          return false
      i = i + 1
    }
    return true
  }

  /**
    * Returns `true` iff the pairwise greatest lower bound of
    * the given pattern `pat` and the given `row` is non-bottom.
    *
    * Modifies the given `row` in the process.
    */
  def matchElms(pat: Array[ValueType], row: Array[ValueType]): Boolean = {
    var i = 0
    while (i < pat.length) {
      val pv = pat(i)
      if (pv != null) {
        val rv = row(i)

        if (rv != pv) {
          val bot = Bot(i)
          val glb = Interpreter.evalCall(Glb(i), Array(pv, rv).asInstanceOf[Array[AnyRef]], sCtx.root).asInstanceOf[ValueType]

          if (bot == glb)
            return false

          row(i) = glb
        }
      }
      i = i + 1
    }
    return true
  }

  /**
    * Returns `true` iff `a` is pairwise less than or equal to `b`.
    */
  private def leq(a: Array[ValueType], b: Array[ValueType]): Boolean = {
    var i = 0
    while (i < a.length) {
      val v1: ValueType = a(i)
      val v2: ValueType = b(i)

      // if v1 and v2 are equal we do not need to compute leq.
      if (v1 != v2) { // TODO: use neq
        // v1 and v2 are different, must compute leq.
        val result = Interpreter.evalCall(Leq(i), Array(v1, v2).asInstanceOf[Array[AnyRef]], sCtx.root).asInstanceOf[ValueType]
        if (!Value.cast2bool(result)) {
          return false
        }
      }
      i = i + 1
    }
    return true
  }

  /**
    * Returns the pairwise least upper bound of `a` and `b`.
    */
  private def lub(a: Array[ValueType], b: Array[ValueType]): Array[ValueType] = {
    val result = new Array[ValueType](a.length)
    var i = 0
    while (i < result.length) {
      val v1: ValueType = a(i)
      val v2: ValueType = b(i)

      if (v1 == Bot(i)) // TODO: use eq
        result(i) = v2
      else if (v2 == Bot(i)) // TODO: use eq
        result(i) = v1
      else
        result(i) = Interpreter.evalCall(Lub(i), Array(v1, v2).asInstanceOf[Array[AnyRef]], sCtx.root).asInstanceOf[ValueType]

      i = i + 1
    }
    return result
  }

}
