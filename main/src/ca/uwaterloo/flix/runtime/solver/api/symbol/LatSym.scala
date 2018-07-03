package ca.uwaterloo.flix.runtime.solver.api.symbol

class LatSym(val name: String) extends TableSym {



  def canEqual(other: Any): Boolean = other.isInstanceOf[LatSym]

  override def equals(other: Any): Boolean = other match {
    case that: LatSym =>
      (that canEqual this) &&
        name == that.name
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(name)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

}
