/*
 * Copyright 2025 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.phase.unification.shared

/**
  * Represents the two-valued Boolean algebra B2.
  */
sealed trait Bool2

object Bool2 {
  case object False extends Bool2

  case object True extends Bool2

  object LatticeOps extends BoolLattice[Bool2] {

    override def Bot: Bool2 = False

    override def Top: Bool2 = True

    override def isBot(t: Bool2): Boolean = t == False

    override def isTop(t: Bool2): Boolean = t == True

    override def comp(t: Bool2): Bool2 = t match {
      case False => True
      case True => False
    }

    override def join(t1: Bool2, t2: Bool2): Bool2 =
      if (t1 == True || t2 == True) True else False

    override def meet(t1: Bool2, t2: Bool2): Bool2 =
      if (t1 == False || t2 == False) False else True

  }
}
