package ca.uwaterloo.flix.runtime.solver.api

case class VarSym(id: Int, stackoffset: Int) {
  def getStackOffset: Int = stackoffset
}
