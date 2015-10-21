package ca.uwaterloo.flix.runtime.datastore

import java.util

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
class IndexedRelation(relation: TypedAst.Collection.Relation, indexes: Set[Seq[Int]])(implicit sCtx: Solver.SolverContext) extends IndexedCollection {
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
    // check if the fact already exists using the default index.
    val key = (defaultIndex, defaultIndex map fact)

    // check if the fact is among the rows returned by the lookup.
    val resultSet = store.getOrElse(key, mutable.Set.empty)
    for (row <- resultSet) {
      if (util.Arrays.equals(row.asInstanceOf[Array[AnyRef]], fact.asInstanceOf[Array[AnyRef]])) {
        assert(lookup(fact).nonEmpty) // TODO: use this...
        return false
      }
    }

    // otherwise we must add the fact to the relation.
    newFact(fact)
  }

  /**
   * Updates all indexes and tables with a new fact `f`.
   */
  private def newFact(f: Array[Value]): Boolean = {
    // loop through all the indexes and the default index.
    for (idx <- indexes + defaultIndex) {
      val key = (idx, idx map f)
      val table = store.getOrElseUpdate(key, mutable.Set.empty[Array[Value]])
      table += f
    }
    true
  }

  /**
   * Performs a lookup of the given pattern `pat`. 
   *
   * The pattern may contain `null` entries. If so, these are interpreted as free variables.
   *
   * Returns an iterator over the matched rows.
   */
  def lookup(pat: Array[Value]): Iterator[Array[Value]] = {
    val idx = pat.toSeq.zipWithIndex.collect {
      case (v, i) if v != null => i
    }
    val key = (idx, idx map pat)

    if (indexes contains idx) {
      // use exact index
      store.getOrElseUpdate(key, mutable.Set.empty[Array[Value]]).iterator
    } else {
      // look for useable index
      val table = indexes.find(idx => idx.forall(i => pat(i) != null)) match {
        case None => scan // no suitable index. Must scan the entire table.
        case Some(fidx) =>
          val key = (fidx, fidx map pat)
          store.getOrElseUpdate(key, mutable.Set.empty[Array[Value]]).iterator
      }

      // table scan
      table filter {
        case row2 => matchRow(pat, row2)
      }
    }
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
  def matchRow(pat: Array[Value], row: Array[Value]): Boolean = {
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
