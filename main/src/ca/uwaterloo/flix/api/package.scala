package ca.uwaterloo.flix

package object api {

  case class FlixApiError(format: String) extends RuntimeException with FlixError

}
