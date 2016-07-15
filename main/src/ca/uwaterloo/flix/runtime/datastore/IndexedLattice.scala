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

import ca.uwaterloo.flix.language.ast.ExecutableAst
import ca.uwaterloo.flix.runtime.{Value, Interpreter}

import scala.annotation.switch
import scala.collection.mutable

import java.util

import scala.reflect.ClassTag

class IndexedLattice[ValueType <: AnyRef](val lattice: ExecutableAst.Table.Lattice, indexes: Set[Int], root: ExecutableAst.Root)(implicit m: ClassTag[ValueType]) extends IndexedCollection[ValueType] {
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
  private val numberOfElms = 1

  /**
    * The lattice operations associated with each lattice.
    */
  private val latticeOps: Array[ExecutableAst.Definition.Lattice] = Array(root.lattices(lattice.value.tpe))

  /**
    * The bottom element(s).
    */
  private val Bot: Array[ValueType] = {
    val a = new Array[AnyRef](latticeOps.length)
    for ((l, i) <- latticeOps.zipWithIndex) {
      val defn = root.constants(l.bot)
      a(i) = Interpreter.evalCall(defn, Array.empty, root)
    }
    a.asInstanceOf[Array[ValueType]]
  }

  /**
    * The Leq operator(s). Must be defined as Flix functions.
    */
  private val Leq: ExecutableAst.Definition.Constant = root.constants(latticeOps(0).leq)

  /**
    * The Lub operator(s). Must be defined as Flix functions.
    */
  private val Lub: ExecutableAst.Definition.Constant = root.constants(latticeOps(0).lub)

  /**
    * The Glb operator(s). Must be defined as Flix functions.
    */
  private val Glb: Array[ExecutableAst.Definition.Constant] = {
    val a = new Array[ExecutableAst.Definition.Constant](latticeOps.length)
    for ((l, i) <- latticeOps.zipWithIndex) {
      a(i) = root.constants(l.glb)
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
    val result = lub(newElm(0), oldElm(0))
    if (!leq(result(0), oldElm(0))) {
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
          val glb = glb2(pv, rv)

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
    * Returns `true` iff `x` is less than or equal to `y`.
    */
  private def leq(x: ValueType, y: ValueType): Boolean = {
    if (x eq y) {
      return true
    }

    val args = Array(x, y).asInstanceOf[Array[AnyRef]]
    val result = Interpreter.evalCall(Leq, args, root).asInstanceOf[ValueType]
    Value.cast2bool(result)
  }

  /**
    * Returns the least upper bound of `x` and `y`.
    */
  private def lub(x: ValueType, y: ValueType): Array[ValueType] = {
    // TODO
    //if (x eq y) {
    //  return x
    // }

    val args = Array(x, y).asInstanceOf[Array[AnyRef]]
    val r = Interpreter.evalCall(Lub, args, root).asInstanceOf[ValueType]

    val res = new Array[ValueType](1)
    res(0) = r
    res
  }

  /**
    * Returns the greatest lower bound of `x` and `y`..
    */
  private def glb2(x: ValueType, y: ValueType): ValueType = {
    // TODO
    //if (x eq y) {
    //  return x
    // }

    val args = Array(x, y).asInstanceOf[Array[AnyRef]]
    Interpreter.evalCall(Glb(0), args, root).asInstanceOf[ValueType]
  }

}
