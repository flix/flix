package ca.uwaterloo.flix.runtime.solver.api.symbol

case class VarSym(id: Int, stackoffset: Int) {
  def getStackOffset: Int = stackoffset
}
