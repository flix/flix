package ca.uwaterloo.flix.language.phase.jvm

// TODO: DOC

sealed trait VariableType

object VariableType {

  /**
    * The variable is available at offset `i` on the stack.
    */
  case class ArgumentVar(i: Int) extends VariableType

  /**
    * The variable is available in the field `f`.
    */
  case class ClosureVar(f: String) extends VariableType

  /**
    * The variable is available in the field `f`.
    *
    * This is the IFO case.
    */
  case class FieldVar(f: String) extends VariableType

}
