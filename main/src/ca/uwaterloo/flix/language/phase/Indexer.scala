/*
 * Copyright 2015-2016 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.ExecutableAst.{Predicate, Term}
import ca.uwaterloo.flix.language.ast.{ExecutableAst, Symbol}

import scala.collection.mutable

object Indexer {

  // TODO: Ensure that everything has at least one index.

  /**
    * Returns an index selection strategy based on left-to-right evaluation of constraint rules.
    */
  def index(root: ExecutableAst.Root): Map[Symbol.TableSym, Set[Seq[Int]]] = {
    val indexes = mutable.Map.empty[Symbol.TableSym, Set[Seq[Int]]]

    // iterate through each rule.
    for (constraint <- root.constraints) {
      // maintain set of bound variables in each rule.
      val bound = mutable.Set.empty[String]
      // iterate through each table predicate in the body.
      for (body <- constraint.body) {
        body match {
          case Predicate.Body.Atom(name, polarity, pterms, _, _, _) =>
            // determine the terms usable for indexing based on whether the predicate refers to a relation or lattice.
            val terms = root.tables(name) match {
              case r: ExecutableAst.Table.Relation => pterms
              case l: ExecutableAst.Table.Lattice => pterms take l.keys.length
            }

            // compute the indices of the determinate (i.e. known) terms.
            val determinate = terms.zipWithIndex.foldLeft(Seq.empty[Int]) {
              case (xs, (t: Term.Body.Wild, i)) => xs
              case (xs, (t: Term.Body.Var, i)) =>
                if (bound contains t.sym.text) // TODO: Correctness
                  xs :+ i
                else
                  xs
              case (xs, (t: Term.Body.Lit, i)) => xs :+ i
              case (xs, (t: Term.Body.Cst, i)) => xs :+ i
              case (xs, (t: Term.Body.Pat, i)) => xs :+ i
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

    // ensure every table has at least one index.
    for ((name, table) <- root.tables) {
      table match {
        case r: ExecutableAst.Table.Relation =>
          val idxs = indexes.getOrElse(name, Set.empty)
          indexes(name) = idxs + Seq(0) // + r.attributes.indices // TODO
        case l: ExecutableAst.Table.Lattice =>
          val idxs = indexes.getOrElse(name, Set.empty)
          indexes(name) = idxs + l.keys.indices
      }
    }

    // user defined indexes overrides the defaults.
    for ((name, table) <- root.tables) {
      root.indexes.get(name) match {
        case None => // no user defined index.
        case Some(index) =>
          val attributes = table match {
            case r: ExecutableAst.Table.Relation => r.attributes
            case l: ExecutableAst.Table.Lattice => l.keys
          }

          indexes(name) = index.indexes.map(idx => idx.map(v => var2offset(v.name, attributes))).toSet
      }
    }

    // return the result as an immutable map.
    indexes.toMap
  }

  private def var2offset(varName: String, attributes: Array[ExecutableAst.Attribute]): Int = {
    var i = 0
    while (i < attributes.length) {
      if (varName == attributes(i).name) return i
      i = i + 1
    }
    throw new RuntimeException // TODO
  }
}
