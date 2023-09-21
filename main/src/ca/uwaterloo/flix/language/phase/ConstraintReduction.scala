/*
 * Copyright 2023 Matthew Lutze
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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast
import ca.uwaterloo.flix.language.ast.Ast.BroadEqualityConstraint
import ca.uwaterloo.flix.language.phase.unification.{Substitution, Unification, UnificationError}
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Err, Ok}

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object ConstraintReduction {
  def reduce(econstr: Ast.BroadEqualityConstraint)(implicit flix: Flix): ReductionResult = econstr match {
    case Ast.BroadEqualityConstraint(tpe1, tpe2) =>
      val renv = ???
      val lenv = ???
      Unification.unifyTypes(tpe1, tpe2, renv, lenv) match {
        case Ok((subst, econstrs)) => ReductionResult.Success(subst, econstrs)
        case Err(e) => ReductionResult.Failure(e)
        // TODO ASSOC-TYPES unify types needs to tell me about success/defer
      }
  }

  // TODO ASSOC-TYPES can probably be narrow econstrs on output
  @tailrec
  def reduceAll(econstrs0: Queue[Ast.BroadEqualityConstraint], deferred: Queue[Ast.BroadEqualityConstraint], subst0: Substitution)(implicit flix: Flix): Result[(Substitution, List[BroadEqualityConstraint]), UnificationError] = {
    econstrs0.headOption match {
      // Case 1: All remaining constructors deferred. We are done.
      case None => Ok((subst0, deferred.toList))
      // Case 2: More econstrs to handle
      case Some(econstr0) =>
        // apply the substitution to the econstr
        val econstr = subst0(econstr0)
        reduce(econstr) match {
          // Case 2.1: Success.
          // We add old deferred econstrs to the fresh queue because the new substitution might help them reduce.
          // We add new econstrs to the deferred queue because this substitution could not help them reduce.
          case ReductionResult.Success(subst, econstrs) => reduceAll(econstrs0.tail ++ deferred, Queue.from(econstrs), subst0 @@ subst)
          // Case 2.2: Defer.
          // We could not reduce this econstr, so we defer it to see if a later substitution will help it reduce.
          case ReductionResult.Defer => reduceAll(econstrs0.tail, deferred :+ econstr, subst0)
          case ReductionResult.Failure(err) => Err(err)
        }
    }
  }

  sealed trait ReductionResult

  object ReductionResult {
    case class Success(substitution: Substitution, econstrs: List[Ast.BroadEqualityConstraint]) extends ReductionResult

    case object Defer extends ReductionResult

    case class Failure(err: UnificationError) extends ReductionResult
  }
}
