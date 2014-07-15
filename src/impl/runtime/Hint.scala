package impl.runtime

case class Hint(repr: Representation)

sealed trait Representation

object Representation {
  case object Code extends Representation
  case object Data extends Representation
}