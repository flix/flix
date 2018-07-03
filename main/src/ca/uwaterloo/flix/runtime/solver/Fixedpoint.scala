package ca.uwaterloo.flix.runtime.solver

import ca.uwaterloo.flix.runtime.solver.api.ProxyObject
import ca.uwaterloo.flix.runtime.solver.api.symbol.TableSym

case class Fixedpoint(relations: Map[TableSym, Iterable[List[ProxyObject]]],
                      lattices: Map[TableSym, Iterable[(List[ProxyObject], ProxyObject)]]) {

}

