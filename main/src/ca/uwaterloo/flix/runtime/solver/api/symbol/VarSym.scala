package ca.uwaterloo.flix.runtime.solver.api.symbol

/**
  * Represents a variable symbol.
  *
  * Note: Two variable symbols are considered the same variable if they are the same object.
  */
class VarSym(stackoffset: Int) {
  def getStackOffset: Int = stackoffset
}
