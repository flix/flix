package ca.uwaterloo.flix.runtime.solver

import ca.uwaterloo.flix.runtime.solver.api.{ProxyObject, Table}
import ca.uwaterloo.flix.runtime.solver.api.symbol.TableSym

case class Fixedpoint(tables: Map[TableSym, Table],
                      relations: Map[TableSym, Iterable[List[ProxyObject]]],
                      lattices: Map[TableSym, Iterable[(List[ProxyObject], ProxyObject)]]) {

}

