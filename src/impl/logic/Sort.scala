package impl.logic

trait Sort

object Sort {
  case class Named(s: String) extends Sort
}
