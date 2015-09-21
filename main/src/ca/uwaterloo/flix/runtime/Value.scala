package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.lang.ast.Name

sealed trait Value

object Value {

  case object Unit

  case class Bool() extends Value

  case class Int()

  case class Str()

  case class Tag(name: Name.Resolved, ident: String, value: Value)

  case class Tuple()

  // ???
  case class Lambda()

}