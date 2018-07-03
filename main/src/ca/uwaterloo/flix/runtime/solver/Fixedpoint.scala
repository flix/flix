package ca.uwaterloo.flix.runtime.solver

import ca.uwaterloo.flix.runtime.solver.api.{ProxyObject, Table}

case class Fixedpoint(relations: Map[Table, Iterable[List[ProxyObject]]],
                      lattices: Map[Table, Iterable[(List[ProxyObject], ProxyObject)]]) {

}

