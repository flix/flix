package ca.uwaterloo.flix.language.backend.phase

import ca.uwaterloo.flix.language.backend.ir.{ReducedIR, SimplifiedIR}

object Reducer {

  object Literals {

  }

  object Expressions {

    def reduce(tast: SimplifiedIR.Expression): ReducedIR.Expression = tast match {
      case _ => ???
    }

  }

}
