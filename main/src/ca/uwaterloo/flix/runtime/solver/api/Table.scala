package ca.uwaterloo.flix.runtime.solver.api

sealed trait Table

object Table {

  case class Relation(sym: TableSym, attributes: Array[Attribute]) extends Table

  case class Lattice(sym: TableSym, keys: Array[Attribute], value: Attribute) extends Table

}
