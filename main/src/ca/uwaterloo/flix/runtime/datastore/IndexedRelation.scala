package ca.uwaterloo.flix.runtime.datastore

import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.runtime.{Solver, Value}

import scala.collection.mutable

/**
 * A class that stores a relation in an indexed database. An index is a sequence of attribute offsets.
 *
 * For example, if given Set(Seq(1)) then the table has exactly one index on the 1st attribute of the relation.
 * As another example, if given Set(Seq(1, 2), Seq(0, 3)) then the relation has two indexes:
 * One index on the 1st and 2nd attributes and another index on the 0th and 3rd column index.
 *
 * @param relation the relation.
 * @param indexes the indexes.
 */
final class IndexedRelation(relation: TypedAst.Collection.Relation, indexes: Set[Seq[Int]])(implicit sCtx: Solver.SolverContext) extends IndexedCollection {
  /**
   * A map from keys, i.e. (index, value) pairs, to rows matching the key.
   */
  // TODO: Maybe change signature to Index -> Key -> Set instead of (Index, Key)
  private val store = mutable.Map.empty[(Seq[Int], Seq[Value]), mutable.Set[Array[Value]]]

  /**
   * The default index which is guaranteed to exist.
   */
  private val defaultIndex: Seq[Int] = Seq(0)

  /**
   * Processes a new inferred `fact`.
   *
   * Adds the fact to the relation. All entries in the fact must be non-null.
   *
   * Returns `true` iff the fact did not already exist in the relation.
   */
  def inferredFact(fact: Array[Value]): Boolean = {
    if (lookup(fact).isEmpty) {
      newFact(fact)
      return true
    }
    false
  }

  /**
   * Updates all indexes and tables with a new fact `f`.
   */
  private def newFact(f: Array[Value]): Unit = {
    // loop through all the indexes and update the tables.
    for (idx <- indexes + defaultIndex) {
      val key = (idx, idx map f)
      val table = store.getOrElseUpdate(key, mutable.Set.empty[Array[Value]])
      table += f
    }
  }

  /**
   * Performs a lookup of the given pattern `pat`. 
   *
   * The pattern may contain `null` entries. If so, these are interpreted as free variables.
   *
   * Returns an iterator over the matched rows.
   */
  def lookup(pat: Array[Value]): Iterator[Array[Value]] = {

    var idx = exactIndex(pat)

    if (idx != null) {
      // use exact index
      val key = indexAndKey(idx, pat)
      store.getOrElseUpdate(key, mutable.Set.empty[Array[Value]]).iterator
    } else {
      // look for useable index
      idx = approxIndex(pat)

      val table = if (idx != null) {
        // use suitable index
        val key = indexAndKey(idx, pat)
        store.getOrElseUpdate(key, mutable.Set.empty[Array[Value]]).iterator
      } else {
        scan // no suitable index. Must scan the entire table.
      }

      // table scan
      table filter {
        case row2 => matchRow(pat, row2)
      }
    }
  }


  /**
   * Returns an index matching all the non-null columns in the given pattern `pat`.
   *
   * Returns `null` if no such exact index exists.
   */
  @inline
  private def exactIndex(pat: Array[Value]): Seq[Int] = {
    val idx = pat.toSeq.zipWithIndex.collect {
      case (v, i) if v != null => i
    }

    if (indexes contains idx) {
      return idx
    }
    null
  }

  /**
   *
   */
  @inline
  private def approxIndex(pat: Array[Value]): Seq[Int] = {
    indexes.find(idx => idx.forall(i => pat(i) != null)) match {
      case None => null
      case Some(idx) => idx
    }
  }

  /**
   * Returns the (index, value) pair which constitutes a key.
   */
  @inline
  private def indexAndKey(idx: Seq[Int], pat: Array[Value]): (Seq[Int], Seq[Value]) = {
    val a = Array.ofDim[Value](idx.length)
    var i: Int = 0
    while (i < idx.length) {
      a(i) = pat(idx(i))
      i = i + 1
    }
    (idx, a.toSeq)
  }


  /**
   * Returns all rows in the relation using a table scan.
   */
  // TODO: Improve performance ...
  // TODO: Deal with duplicate properly...
  def scan: Iterator[Array[Value]] = (store map {
    case (_, rows) => rows
  }).toList.flatten.toIterator


  /**
   * Returns `true` if the given pattern `pat` matches the given `row`.
   *
   * A pattern matches if all is non-null entries are equal to the row.
   */
  @inline
  private def matchRow(pat: Array[Value], row: Array[Value]): Boolean = {
    var i = 0
    while (i < pat.length) {
      val pv = pat(i)
      if (pv != null)
        if (pv != row(i))
          return false
      i = i + 1
    }
    true
  }

}
