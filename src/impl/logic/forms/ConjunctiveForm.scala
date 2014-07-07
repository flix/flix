package impl.logic.forms

import impl.logic.Term

/**
 * A conjunction of disjunctions.
 */
case class ConjunctiveForm(clauses: List[List[Term]])