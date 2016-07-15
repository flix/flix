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
    * A map from indexes to a map from keys to rows (represented as map from keys to an element):
    *
    * Index -> IndexKey -> Keys -> Elm.
    */
  private val store = mutable.Map.empty[Int, mutable.Map[Key[ValueType], mutable.Map[Key[ValueType], ValueType]]]

  /**
    * The number of key columns in the lattice.
    */
  private val numberOfKeys = lattice.keys.length

  /**
    * The lattice operations associated with each lattice.
    */
  private val latticeOps: ExecutableAst.Definition.Lattice = root.lattices(lattice.value.tpe)

  /**
    * The Bot function definition.
    */
  private val BotDefn: ExecutableAst.Definition.Constant = root.constants(latticeOps.bot)

  /**
    * The Leq function definition.
    */
  private val LeqDefn: ExecutableAst.Definition.Constant = root.constants(latticeOps.leq)

  /**
    * The Lub function definition.
    */
  private val LubDefn: ExecutableAst.Definition.Constant = root.constants(latticeOps.lub)

  /**
    * The Glb function definition.
    */
  private val GlbDefn: ExecutableAst.Definition.Constant = root.constants(latticeOps.glb)

  /**
    * The bottom element.
    */
  private val Bot: ValueType = {
    Interpreter.evalCall(BotDefn, Array.empty, root).asInstanceOf[ValueType]
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
    val key = keysOf(fact)

    // Lookup the old element (create it, if it doesn't exist).
    val newElm = elmOf(fact)
    val oldElm = map.getOrElseUpdate(key, Bot)

    // Compute the lub and check if it is subsumed by the old element.
    val result = evalLub(newElm, oldElm)
    if (!evalLeq(result, oldElm)) {
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
      case (keys, _) => matchKeys(pat, keys.toArray)
    } map {
      case (keys, elm) =>
        // compute greatest lower bounds.
        if (elmOf(pat) == null)
          (keys, elm)
         else
          (keys, evalGlb(elmOf(pat), elm))
    } filter {
      // remove null elements introduced above.
      case e => !isBot(e._2)
    } map {
      case (keys, elms) =>
        // construct the result.
        val result = new Array[ValueType](numberOfKeys + 1)
        System.arraycopy(keys.toArray, 0, result, 0, numberOfKeys)
        result(result.length - 1) = elms
        result.asInstanceOf[Array[ValueType]]
    }
  }

  /**
    * Returns all rows in the relation using a table scan.
    */
  def scan: Iterator[(Key[ValueType], ValueType)] = store(indexes.head).iterator.flatMap {
    case (key, m) => m.iterator
  }

  /**
    * Returns the key part of the given array `a`.
    */
  private def keysOf(a: Array[ValueType]): Key[ValueType] = {
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
  @inline
  private def elmOf(a: Array[ValueType]): ValueType = {
    return a(a.length - 1)
  }

  /**
    * Returns `true` iff all non-null keys in the given pattern `pat` are
    * equal to their corresponding entry in the given `row`.
    */
  private def matchKeys(pat: Array[ValueType], row: Array[ValueType]): Boolean = {
    var i = 0
    while (i < numberOfKeys) {
      val pv = pat(i)
      if (pv != null)
        if (pv != row(i))
          return false
      i = i + 1
    }
    return true
  }

  /**
    * Returns true if `x` is the bottom element.
    */
  private def isBot(x: ValueType): Boolean = x == Bot

  /**
    * Returns `true` iff `x` is less than or equal to `y`.
    */
  private def evalLeq(x: ValueType, y: ValueType): Boolean = {
    // if `x` and `y` are the same object then they must be equal.
    if (x eq y) return true

    // evaluate the partial order function passing the arguments `x` and `y`.
    val args = Array(x, y).asInstanceOf[Array[AnyRef]]
    val result = Interpreter.evalCall(LeqDefn, args, root)
    Value.cast2bool(result)
  }

  /**
    * Returns the least upper bound of `x` and `y`.
    */
  private def evalLub(x: ValueType, y: ValueType): ValueType = {
    // if `x` and `y` are the same object then there is no need to compute the lub.
    if (x eq y) return x

    // evaluate the least upper bound function passing the arguments `x` and `y`.
    val args = Array(x, y).asInstanceOf[Array[AnyRef]]
    Interpreter.evalCall(LubDefn, args, root).asInstanceOf[ValueType]
  }

  /**
    * Returns the greatest lower bound of `x` and `y`..
    */
  private def evalGlb(x: ValueType, y: ValueType): ValueType = {
    // if `x` and `y` are the same object then there is no need to compute the glb.
    if (x eq y) return x

    // evaluate the greatest lower bound function passing the arguments `x` and `y`.
    val args = Array(x, y).asInstanceOf[Array[AnyRef]]
    Interpreter.evalCall(GlbDefn, args, root).asInstanceOf[ValueType]
  }

}
