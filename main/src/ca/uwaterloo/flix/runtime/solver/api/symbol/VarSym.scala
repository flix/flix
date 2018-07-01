package ca.uwaterloo.flix.runtime.solver.api.symbol

/**
  * Represents a variable symbol.
  *
  * Note: Two variable symbols are considered the same variable if they are the same object.
  */
class VarSym(stackoffset: Int) {
  // TODO: We want VarSym to eventually be replaced by just an index.
  def getStackOffset: Int = stackoffset
}
