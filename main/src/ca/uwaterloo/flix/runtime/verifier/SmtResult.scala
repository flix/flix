package ca.uwaterloo.flix.runtime.verifier

import com.microsoft.z3.Model

/**
  * A common super-type that represents the result of an SMT query.
  */
sealed trait SmtResult

object SmtResult {

  /**
    * The SMT query is satisfiable, i.e. it has at least one model.
    *
    * @param model a model that satisfies the SMT query.
    */
  case class Satisfiable(model: Model) extends SmtResult

  /**
    * The SMT query is unsatisfiable, i.e. it has no model.
    */
  case object Unsatisfiable extends SmtResult

  /**
    * The SMT query may or may not be satisfiable, i.e. it is unknown if there is a model.
    */
  case object Unknown extends SmtResult

}

