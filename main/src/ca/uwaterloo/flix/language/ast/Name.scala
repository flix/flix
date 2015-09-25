package ca.uwaterloo.flix.language.ast

object Name {

  case class Resolved(parts: List[String]) {
    val format: String = parts.mkString(",")
  }

}
