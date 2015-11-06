package ca.uwaterloo.flix.language.backend.phase

import ca.uwaterloo.flix.language.ast.{TypedAst, Name}
import ca.uwaterloo.flix.language.ast.TypedAst.{Term, Predicate}

import scala.collection.mutable

object Indexer {

  // TODO: Ensure that everything has at least one index.

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
          case Predicate.Body.Collection(name, pterms, _, _) =>
            // determine the terms usable for indexing based on whether the predicate refers to a relation or lattice.
            val terms = root.collections(name) match {
              case r: TypedAst.Collection.Relation => pterms
              case l: TypedAst.Collection.Lattice => pterms take l.keys.length
            }

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

    // ensure every collection has at least one index.
    for ((name, collection) <- root.collections) {
      collection match {
        case r: TypedAst.Collection.Relation =>
          val idxs = indexes.getOrElse(name, Set.empty)
          indexes(name) = idxs + Seq(0) + r.attributes.indices
        case l: TypedAst.Collection.Lattice =>
          val idxs = indexes.getOrElse(name, Set.empty)
          indexes(name) = idxs + Seq(0)
      }
    }

    // return the result as an immutable map.
    indexes.toMap
  }

}
