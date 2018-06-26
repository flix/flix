package ca.uwaterloo.flix.runtime.solver.data

sealed trait Table

object Table {

  case class Relation(sym: TableSym, attributes: Array[Attribute]) extends Table

  case class Lattice(sym: TableSym, keys: Array[Attribute], value: Attribute) extends Table {
    def attributes: List[Attribute] = keys.toList ::: value :: Nil
  }

}
