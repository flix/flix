package ca.uwaterloo.flix.runtime.solver.api.symbol

class VarSym(val id: Int, stackoffset: Int) {

  // TODO: Update equality.

  def getStackOffset: Int = stackoffset

  def canEqual(other: Any): Boolean = other.isInstanceOf[VarSym]

  override def equals(other: Any): Boolean = other match {
    case that: VarSym =>
      (that canEqual this) &&
        id == that.id
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(id)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
