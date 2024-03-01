package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}

import scala.annotation.tailrec

object EffUnification2 {
  def unifyAll(l: List[(Type, Type)], renv0: RigidityEnv)(implicit flix: Flix): Result[Substitution, UnificationError] = {
    unifyAllHelper(l, renv0, Substitution.empty)
  }

  @tailrec
  private def unifyAllHelper(l: List[(Type, Type)], renv0: RigidityEnv, subst0: Substitution)(implicit flix: Flix): Result[Substitution, UnificationError] = l match {
    case Nil => Result.Ok(subst0)
    case (t1, t2) :: rest => EffUnification.unify(subst0(t1), subst0(t2), renv0) match {
      case Result.Ok((subst1, Nil)) => unifyAllHelper(rest, renv0, subst1 @@ subst0)
      case Result.Ok((_, _ :: _)) => throw InternalCompilerException("unexpected unresolved constraint", SourceLocation.Unknown)
      case Result.Err(e) => Result.Err(e)
    }
  }
}
