package ca.uwaterloo.flix.language.backend.phase

import ca.uwaterloo.flix.language.ast.{TypedAst, Name}
import ca.uwaterloo.flix.language.ast.TypedAst.{Term, Predicate}

import scala.collection.mutable

object Indexer {

  /**
   * Returns an index selection strategy based on left-to-right evaluation of constraint rules.
   */
  def index(root: TypedAst.Root): Map[Name.Resolved, Set[Seq[Int]]] = {
    val indexes = mutable.Map.empty[Name.Resolved, Set[Seq[Int]]]

    // iterate through each rule.
    for (constraint <- root.rules) {
      // maintain set of bound variables in each rule.
      val bound = mutable.Set.empty[String]
      // iterate through each collection predicate in the body.
      for (body <- constraint.body) {
        body match {
          case Predicate.Body.Relation(name, terms, _, _) =>
            // TODO: This has to be careful with lattices.

            // compute the indices of the determinate (i.e. known) terms.
            val determinate = terms.zipWithIndex.foldLeft(Seq.empty[Int]) {
              case (xs, (t: Term.Body.Wildcard, i)) => xs
              case (xs, (t: Term.Body.Var, i)) =>
                if (bound contains t.ident.name)
                  xs :+ i
                else
                  xs
              case (xs, (t: Term.Body.Lit, i)) => xs :+ i
            }

            // if one or more terms are determinate then an index would be useful.
            if (determinate.nonEmpty) {
              val idxs = indexes.getOrElse(name, Set.empty)
              indexes(name) = idxs + determinate
            }

            // update the set of bound variables.
            bound ++= body.freeVars
          case _ => // nop
        }
      }
    }

    // return the result as an immutable map.
    indexes.toMap
  }

}
