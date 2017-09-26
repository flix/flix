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

  // TODO: This class is most likely currently broken. Rewrite from scratch!

  /**
    * Returns an index selection strategy based on left-to-right evaluation of constraint rules.
    */
  def index(root: ExecutableAst.Root): Map[Symbol.TableSym, Set[Seq[Int]]] = {
    val indexes = mutable.Map.empty[Symbol.TableSym, Set[Seq[Int]]]

    // iterate through each rule.
    for (constraint <- root.constraints) {
      // maintain set of bound variables in each rule.
      val bound = mutable.Set.empty[Symbol.VarSym]
      // iterate through each table predicate in the body.
      for (body <- constraint.body) {
        body match {
          case Predicate.Body.Atom(name, polarity, pterms, _, _) =>
            // determine the terms usable for indexing based on whether the predicate refers to a relation or lattice.
            val terms = root.tables(name) match {
              case r: ExecutableAst.Table.Relation => pterms
              case l: ExecutableAst.Table.Lattice => pterms take l.keys.length
            }

            // compute the indices of the determinate (i.e. known) terms.
            val determinate = terms.zipWithIndex.foldLeft(Seq.empty[Int]) {
              case (xs, (t: Term.Body.Wild, i)) => xs
              case (xs, (t: Term.Body.Var, i)) =>
                xs
              // TODO: Someone needs to carefully analyse this code.
              //                if (bound contains t.sym)
              //                  xs :+ i
              //                else
              //                  xs
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
            bound ++= freeVars(body)
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

  private def freeVars(body: ExecutableAst.Predicate.Body): Set[Symbol.VarSym] = body match {
    case ExecutableAst.Predicate.Body.Atom(sym, polarity, terms, index2sym, loc) => terms.foldLeft(Set.empty[Symbol.VarSym]) {
      case (sacc, term) => sacc ++ freeVars(term)
    }
    case ExecutableAst.Predicate.Body.Filter(sym, terms, loc) => terms.foldLeft(Set.empty[Symbol.VarSym]) {
      case (sacc, term) => sacc ++ freeVars(term)
    }
    case ExecutableAst.Predicate.Body.Loop(sym, term, loc) => freeVars(term)
  }

  private def freeVars(t0: ExecutableAst.Term.Head): Set[Symbol.VarSym] = t0 match {
    case ExecutableAst.Term.Head.Var(sym, tpe, loc) => Set(sym)
    case ExecutableAst.Term.Head.Lit(lit, tpe, loc) => Set.empty
    case ExecutableAst.Term.Head.Cst(sym, tpe, loc) => Set.empty
    case ExecutableAst.Term.Head.App(sym, args, tpe, loc) => args.toSet
  }

  private def freeVars(t0: ExecutableAst.Term.Body): Set[Symbol.VarSym] = t0 match {
    case ExecutableAst.Term.Body.Wild(tpe, loc) => Set.empty
    case ExecutableAst.Term.Body.Var(sym, tpe, loc) => Set(sym)
    case ExecutableAst.Term.Body.Lit(lit, tpe, loc) => Set.empty
    case ExecutableAst.Term.Body.Cst(sym, tpe, loc) => Set.empty
    case ExecutableAst.Term.Body.Pat(pat, tpe, loc) => freeVars(pat)
  }

  private def freeVars(p0: ExecutableAst.Pattern): Set[Symbol.VarSym] = p0 match {
    case ExecutableAst.Pattern.Wild(tpe, loc) => Set.empty
    case ExecutableAst.Pattern.Var(sym, tpe, loc) => Set(sym)
    case ExecutableAst.Pattern.Unit(loc) => Set.empty
    case ExecutableAst.Pattern.True(loc) => Set.empty
    case ExecutableAst.Pattern.False(loc) => Set.empty
    case ExecutableAst.Pattern.Char(lit, loc) => Set.empty
    case ExecutableAst.Pattern.Float32(lit, loc) => Set.empty
    case ExecutableAst.Pattern.Float64(lit, loc) => Set.empty
    case ExecutableAst.Pattern.Int8(lit, loc) => Set.empty
    case ExecutableAst.Pattern.Int16(lit, loc) => Set.empty
    case ExecutableAst.Pattern.Int32(lit, loc) => Set.empty
    case ExecutableAst.Pattern.Int64(lit, loc) => Set.empty
    case ExecutableAst.Pattern.BigInt(lit, loc) => Set.empty
    case ExecutableAst.Pattern.Str(lit, loc) => Set.empty
    case ExecutableAst.Pattern.Tag(sym, tag, pat, tpe, loc) => freeVars(pat)
    case ExecutableAst.Pattern.Tuple(elms, tpe, loc) => (elms flatMap freeVars).toSet
  }

}
