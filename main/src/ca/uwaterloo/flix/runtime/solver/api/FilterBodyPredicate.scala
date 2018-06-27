package ca.uwaterloo.flix.runtime.solver.api

case class FilterBodyPredicate(f: Array[AnyRef] => Boolean, terms: Array[Term.Body]) extends BodyPredicate
