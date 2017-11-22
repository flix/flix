package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ExecutableAst.Root

/**
  * Generates bytecode for the cell classes.
  */
object GenCellClasses {

  /**
    * Returns the bytecode for the cell classes built-in to the Flix language.
    */
  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {

    // TODO: Ramin.
    //
    // Emit code for:
    //
    //   Cell$Bool.
    //   Cell$Char.
    //   ...

    Map.empty
  }

}
