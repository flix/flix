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
   * A map from indexes to keys to rows of values.
   */
  private val store = mutable.Map.empty[Seq[Int], mutable.Map[Seq[Value], mutable.Set[Array[Value]]]]

  /**
   * The default index which is guaranteed to exist.
   */
  private val defaultIndex: Seq[Int] = Seq(0)

  /**
   * Records the number of indexed lookups, i.e. exact lookups.
   */
  private var indexedLookups = 0

  /**
   * Records the number of indexed scans, i.e. table scans which can use an index.
   */
  private var indexedScans = 0

  /**
   * Records the number of full scans, i.e. table scans which cannot use an index.
   */
  private var fullScans = 0

  /**
   * Initialize the store for all indexes.
   */
  for (idx <- indexes + defaultIndex) {
    store(idx) = mutable.Map.empty
  }

  /**
   * Returns the size of the relation.
   */
  def getSize: Int = scan.size

  /**
   * Returns the number of indexed lookups.
   */
  def getNumberOfIndexedLookups: Int = indexedLookups

  /**
   * Returns the number of indexed scans.
   */
  def getNumberOfIndexedScans: Int = indexedScans

  /**
   * Returns the number of full scans.
   */
  def getNumberOfFullScans: Int = fullScans

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
  private def newFact(fact: Array[Value]): Unit = {
    // loop through all the indexes and update the tables.
    for (idx <- indexes + defaultIndex) {
      val key = keyOf(idx, fact)
      val table = store(idx).getOrElseUpdate(key, mutable.Set.empty[Array[Value]])
      table += fact
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
      indexedLookups += 1
      val key = keyOf(idx, pat)
      store(idx).getOrElseUpdate(key, mutable.Set.empty).iterator
    } else {
      // look for usable index
      idx = approxIndex(pat)
      val table = if (idx != null) {
        indexedScans += 1
        // use suitable index
        val key = keyOf(idx, pat)
        store(idx).getOrElseUpdate(key, mutable.Set.empty).iterator
      } else {
        fullScans += 1
        scan // no suitable index. Must scan the entire table.
      }

      // table scan
      table filter {
        case row => matchRow(pat, row)
      }
    }
  }

  /**
   * Returns the key for the given index `idx` and pattern `pat`.
   */
  @inline
  private def keyOf(idx: Seq[Int], pat: Array[Value]): Seq[Value] = {
    val a = Array.ofDim[Value](idx.length)
    var i: Int = 0
    while (i < idx.length) {
      a(i) = pat(idx(i))
      i = i + 1
    }
    a.toSeq
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
   * Returns an approximate index matching all the non-null columns in the given pattern `pat`.
   *
   * Returns `null` if no index is usable (and thus a full table scan must be performed).
   */
  @inline
  private def approxIndex(pat: Array[Value]): Seq[Int] = {
    (indexes + defaultIndex).find(idx => idx.forall(i => pat(i) != null)) match {
      case None =>
        null
      case Some(idx) =>
        idx
    }
  }

  /**
   * Returns all rows in the relation using a table scan.
   */
  def scan: Iterator[Array[Value]] = store(defaultIndex).iterator.flatMap {
    case (key, value) => value
  }

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
