package ca.uwaterloo.flix.runtime.solver

import ca.uwaterloo.flix.language.ast.ExecutableAst.{Attribute, Table}
import ca.uwaterloo.flix.language.ast.{ExecutableAst, Symbol}
import ca.uwaterloo.flix.runtime.solver.datastore.ProxyObject

case class Fixedpoint(tables: Map[Symbol.TableSym, ExecutableAst.Table],
                      relations: Map[Symbol.TableSym, Iterable[List[ProxyObject]]],
                      lattices: Map[Symbol.TableSym, Iterable[(List[ProxyObject], ProxyObject)]]) {


  /**
    * Returns a map from fully-qualified relation names to a pair of an attribute list and a set of rows for that table.
    */
  def getRelations: Map[String, (List[String], Iterable[List[String]])] =
    relations.foldLeft(Map.empty[String, (List[String], Iterable[List[String]])]) {
      case (macc, (sym, rows)) =>
        tables(sym) match {
          case Table.Relation(_, attr, _) =>
            // Compute the attributes names.
            val attributes: List[String] = attr.map(_.name).toList

            // Compute the rows of the table.
            val rows: Iterable[List[String]] = relations(sym).map {
              case row => (row zip attr) map {
                case (obj, Attribute(_, tpe)) => obj.toString
              }
            }

            macc + (sym.toString -> (attributes, rows))
          case Table.Lattice(_, _, _, _) => macc // Nop
        }
    }


  /**
    * Returns a map from fully-qualified lattices names to a pair of an attribute list and a set of rows for that table.
    */
  def getLattices: Map[String, (List[String], Iterable[List[String]])] = lattices.foldLeft(Map.empty[String, (List[String], Iterable[List[String]])]) {
    case (macc, (sym, rows)) =>
      tables(sym) match {
        case Table.Relation(_, attr, _) => macc // Nop

        case Table.Lattice(_, keys, value, _) =>
          // Compute the attributes of the table.
          val attr = keys.toList ::: value :: Nil

          // Compute the attribute names.
          val attributes: List[String] = attr.map(_.name)

          // Compute the rows of the table.
          val rows: Iterable[List[String]] = lattices(sym).map {
            case (ks, v) => ((ks :: v :: Nil) zip attr) map {
              case (obj, Attribute(_, tpe)) => obj.toString
            }
          }

          macc + (sym.toString -> (attributes, rows))
      }
  }

}

