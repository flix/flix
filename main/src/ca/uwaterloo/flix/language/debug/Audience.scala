package ca.uwaterloo.flix.language.debug

sealed trait Audience
object Audience {
  case object Internal extends Audience
  case object External extends Audience
}
