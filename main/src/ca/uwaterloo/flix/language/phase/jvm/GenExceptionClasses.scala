package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ExecutableAst.Root

/**
  * Generates bytecode for the exceptions.
  */
object GenExceptionClasses {

  /**
    * Returns the bytecode for the exception classes built-in to the Flix language.
    */
  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {

    // TODO: Ramin.
    //
    // Emit code for:
    //
    //   HoleException.
    //   MatchException.
    //   SwitchException.
    //   UserException.
    //
    // And emit an interface FlixException that all of these extend.
    //
    // Note: Skip `RuleException`.

    Map.empty
  }

}
