package impl.logic.forms

import impl.logic.Term

/**
 * A disjunction of conjunctions.
 */
case class DisjunctiveForm(clauses: List[List[Term]])
