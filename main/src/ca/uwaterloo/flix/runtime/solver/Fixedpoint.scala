package ca.uwaterloo.flix.runtime.solver

import ca.uwaterloo.flix.runtime.solver.api.{Table, TableSym}
import ca.uwaterloo.flix.runtime.solver.datastore.ProxyObject

case class Fixedpoint(tables: Map[TableSym, Table],
                      relations: Map[TableSym, Iterable[List[ProxyObject]]],
                      lattices: Map[TableSym, Iterable[(List[ProxyObject], ProxyObject)]]) {

}

