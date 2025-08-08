package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.Name

case class PredicateAndArity(pred: Name.Pred, arity: Int)
