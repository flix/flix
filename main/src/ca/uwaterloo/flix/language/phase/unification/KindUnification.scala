/*
 * Copyright 2022 Matthew Lutze
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
package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.Kind

object KindUnification {

  /**
    * Unifies the kinds, returning the most specific kind if possible.
    */
  def unify(k1: Kind, k2: Kind): Option[Kind] = (k1, k2) match {
    // Wild ~ k = k
    case (Kind.Wild, k) => Some(k)
    case (k, Kind.Wild) => Some(k)

    // Beef ~ Bool = Bool
    case (Kind.Beef, Kind.Bool) => Some(Kind.Bool)
    case (Kind.Bool, Kind.Beef) => Some(Kind.Bool)

    // Beef ~ Effect = Effect
    case (Kind.Beef, Kind.Effect) => Some(Kind.Effect)
    case (Kind.Effect, Kind.Beef) => Some(Kind.Effect)

    // (a1 -> b1) ~ (a2 -> b2) = (a1 ~ a2) ~ (b1 -> b2)
    case (Kind.Arrow(k11, k12), Kind.Arrow(k21, k22)) =>
      for {
        kind1 <- unify(k11, k21)
        kind2 <- unify(k12, k22)
      } yield Kind.Arrow(kind1, kind2)

    // k ~ k = k
    case (kind1, kind2) if kind1 == kind2 => Some(kind1)

    // else fail
    case _ => None
  }

  /**
    * Returns true iff the two kinds can be unified.
    */
  def unifiesWith(k1: Kind, k2: Kind): Boolean = unify(k1, k2) match {
    case None => false
    case Some(_) => true
  }

}
