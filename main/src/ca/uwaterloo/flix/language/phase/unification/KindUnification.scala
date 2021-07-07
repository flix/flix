package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Kind, SourceLocation}
import ca.uwaterloo.flix.language.errors.KindError
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.ToOk

// MATT license
object KindUnification {
  def unify(k1: Kind, k2: Kind): Result[KindSubstitution, KindUnificationError] = (k1, k2) match {
    // Group 1: one of the kinds is a variable
    case (kvar: Kind.Var, k) => KindSubstitution(Map(kvar -> k)).toOk
    case (k, kvar: Kind.Var) => KindSubstitution(Map(kvar -> k)).toOk

    // Group 2: the kinds are the same
    case (Kind.Star, Kind.Star) => KindSubstitution.empty.toOk
    case (Kind.Bool, Kind.Bool) => KindSubstitution.empty.toOk
    case (Kind.Schema, Kind.Schema) => KindSubstitution.empty.toOk
    case (Kind.Record, Kind.Record) => KindSubstitution.empty.toOk

    // Group 3: one kind is a subkind of the other
    case (Kind.Star, Kind.Schema) => KindSubstitution.empty.toOk
    case (Kind.Star, Kind.Record) => KindSubstitution.empty.toOk
    case (Kind.Schema, Kind.Star) => KindSubstitution.empty.toOk
    case (Kind.Record, Kind.Star) => KindSubstitution.empty.toOk

      // Group 4: need to recurse
    case (Kind.Arrow(k11, k12), Kind.Arrow(k21, k22)) =>
      unify(k11, k21) match {
        case Result.Ok(subst1) => unify(k12, k22) match {
          case Result.Ok(subst2) => Result.Ok(subst2 @@ subst1)
          case Result.Err(e) => Result.Err(e)
        }
        case Result.Err(e) => Result.Err(e)
      }

    case _ => Result.Err(KindUnificationError.MismatchedKinds(k1, k2))

  }

  def unifyKindM(k1: Kind, k2: Kind, loc: SourceLocation)(implicit flix: Flix): KindInferMonad[Kind] = {
    KindInferMonad((s: KindSubstitution) => {
      val kind1 = s(k1)
      val kind2 = s(k2)
      unify(kind1, kind2) match {
        case Result.Ok(s1) =>
          val subst = s1 @@ s
          Result.Ok(subst, subst(k1))
        case Result.Err(KindUnificationError.MismatchedKinds(k1, k2)) =>
          Result.Err(KindError.MismatchedKinds(k1, k2, loc))
      }
    })
  }
}
