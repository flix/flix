package impl.logic

trait Sort

// TODO: Add LatticeSymbol?
@deprecated
object Sort {
  case class Named(s: String) extends Sort
}
