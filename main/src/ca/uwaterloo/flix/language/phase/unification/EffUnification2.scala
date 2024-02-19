/*
 *  Copyright 2024 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{RigidityEnv, SourceLocation, Type}
import ca.uwaterloo.flix.util.collection.Bimap
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}

import scala.collection.immutable.SortedSet

object EffUnification2 {

  /**
   * Returns the most general unifier of the all the pairwise unification problems in `l`.
   *
   * Note: A type in `l` must not contain any associated effects.
   */
  def unifyAll(l: List[(Type, Type)], renv0: RigidityEnv)(implicit flix: Flix): Result[Substitution, UnificationError] = {
    // Case 1: If the list is empty we can return immediately.
    if (l.isEmpty) {
      return Result.Ok(Substitution.empty)
    }

    // Case 2: We translate every (Type, Type) pair to a pair of Boolean effect formulas.

    var eqns: List[Equation] = Nil
    var s: Substitution = Substitution.empty

    while(eqns.nonEmpty) {

      // Discord trivial
      // Fail on conflict.
      // Find unit propagation and apply them one a time.

      for (eqn <- eqns) {
        classify(eqn) match {
          case Classification.Trivial =>
          case Classification.Ground =>
        }
      }
    }

    ??? // TODO
  }

  private def extendSubstWithGround(eqn: Equation, subst: Substitution): Substitution = eqn match {
    case Equation(Term.Var(x), t2) =>
      // TODO: Lookup in subst, extend if not found. Otherwise throw.
      ???
  }

  private case class Equation(t1: Term, t2: Term)

  def classify(eqn: Equation): Classification = ???

  private sealed trait Classification

  private object Classification {
    case object Trivial extends Classification

    /**
     * An equation of the form `x = true`, `x = false`, and their mirrored versions.
     */
    case object Ground extends Classification

  }

  private sealed trait Term {

    final def freeVars: SortedSet[Int] = this match {
      case Term.True => SortedSet.empty
      case Term.False => SortedSet.empty
      case Term.Var(x) => SortedSet(x)
      case Term.Not(f) => f.freeVars
      case Term.And(ts) => ???
      case Term.Or(ts) => ???
    }

    final def size: Int = this match {
      case Term.True => 0
      case Term.False => 0
      case Term.Var(_) => 0
      case Term.Not(t) => t.size
      case Term.And(ts) => ???
      case Term.Or(ts) => ???
    }

    override def toString: String = this match {
      case Term.True => "true"
      case Term.False => "false"
      case Term.Var(x) => s"x$x"
      case Term.Not(f) => f match {
        case Term.Var(x) => s"!x$x"
        case _ => s"!($f)"
      }
      case Term.And(ts) => ???
      case Term.Or(ts) => ???
    }

  }

  private object Term {

    case object True extends Term

    case object False extends Term

    case class Var(x: Int) extends Term

    case class Not(t: Term) extends Term

    case class And(ts: List[Term]) extends Term {
      assert(ts.length >= 2)
    }

    case class Or(ts: List[Term]) extends Term {
      assert(ts.length >= 2)
    }

    final def mkNot(t: Term): Term = ???

    final def substitute(f: Term, m: Bimap[Int, Int]): Term = f match {
      case True => True
      case False => False
      case Var(x) => m.getForward(x) match {
        case None => throw InternalCompilerException(s"Unexpected unbound variable: 'x$x'.", SourceLocation.Unknown)
        case Some(y) => Var(y)
      }
      case Not(t) => mkNot(substitute(t, m))
      case And(ts) => ???
      case Or(ts) => ???
    }
  }

}
