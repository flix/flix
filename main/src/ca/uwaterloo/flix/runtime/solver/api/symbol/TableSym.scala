package ca.uwaterloo.flix.runtime.solver.api.symbol

/**
  * The name of a table.
  */
class TableSym(val name: String) {

  // TODO: Update equality.

  def canEqual(other: Any): Boolean = other.isInstanceOf[TableSym]

  override def equals(other: Any): Boolean = other match {
    case that: TableSym =>
      (that canEqual this) &&
        name == that.name
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(name)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
