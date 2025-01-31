package ca.uwaterloo.flix.api.effectlock.serialization

sealed trait SVT

object SVT {

  case object Absent extends SVT

  case class SourceText(s: String) extends SVT

}
