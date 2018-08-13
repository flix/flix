package ca.uwaterloo.flix.runtime.solver.api.symbol

/**
  * Represents a variable symbol.
  *
  * Note: Two variable symbols are considered the same variable if they are the same object.
  */
class VarSym(humanName: String) {
  // TODO: We want VarSym to eventually be replaced by just an index.
  private var offset: Int = -1

  def getStackOffset: Int = offset

  def setStackOffset(v: Int): Unit = {
    this.offset = v
  }

  /**
    * Returns a string representation of `this` symbol.
    */
  override def toString: String = humanName
}
