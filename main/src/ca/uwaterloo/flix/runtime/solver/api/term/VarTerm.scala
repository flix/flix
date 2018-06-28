package ca.uwaterloo.flix.runtime.solver.api.term

import ca.uwaterloo.flix.runtime.solver.api.symbol.VarSym

case class VarTerm(sym: VarSym) extends Term
