package ca.uwaterloo.flix.language.phase.jvm

/**
  * A common super-type for information about the representation of variable.
  */
sealed trait VariableInfo

object VariableInfo {

  /**
    * The variable is stored in the closure-captured field `name`.
    */
  case class CloField(name: String) extends VariableInfo

  /**
    * The variable is stored in the IFO argument field `name`.
    */
  case class IfoField(name: String) extends VariableInfo

}
