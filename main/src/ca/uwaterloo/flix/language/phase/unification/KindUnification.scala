/*
 * Copyright 2022 Matthew Lutze
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.Kind

object KindUnification {

  /**
    * Unifies the kinds, returning the most specific kind if possible.
    */
  def unify(k1: Kind, k2: Kind): Option[Kind] = {
    // Fast path: a reference-equality check short-circuits the common case
    // without allocating the tuple matched on below.
    if (k1 eq k2) {
      Some(k1)
    } else (k1, k2) match {
      // NB:  The order of these cases is determined by coverage analysis.

      // k ~ k = k
      case (kind1, kind2) if kind1 == kind2 =>
        Some(kind1)

      // (a1 -> b1) ~ (a2 -> b2) = (a1 ~ a2) ~ (b1 -> b2)
      case (Kind.Arrow(k11, k12), Kind.Arrow(k21, k22)) =>
        for {
          kind1 <- unify(k11, k21)
          kind2 <- unify(k12, k22)
        } yield Kind.mkArrow(kind1, kind2)

      // Wild ~ k = k
      case (Kind.Wild, k) =>
        Some(k)
      case (k, Kind.Wild) =>
        Some(k)

      // WildCaseSet ~ CaseSet(s) = CaseSet(s)
      case (Kind.WildCaseSet , Kind.CaseSet(sym)) =>
        Some(Kind.CaseSet(sym))
      case (Kind.CaseSet(sym), Kind.WildCaseSet) =>
        Some(Kind.CaseSet(sym))

      case (Kind.Error, k) => Some(k)

      case (k, Kind.Error) => Some(k)

      // else fail
      case _ => None
    }
  }
}
