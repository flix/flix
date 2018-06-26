package ca.uwaterloo.flix.runtime.solver.data

sealed trait ConstraintParam

object ConstraintParam {

  case class HeadParam(sym: VarSym) extends ConstraintParam

  case class RuleParam(sym: VarSym) extends ConstraintParam

}
