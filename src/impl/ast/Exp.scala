package impl.ast

import impl.logic.Term

sealed trait Exp

object Exp {

  def desugar = ???

  def compile(e: Exp): Term = e match {
    case Exp.Match(e1, cases) => {

      def visit(cond: Exp, exp: Exp): Term = {
        compile(exp)
      }
      ???
    }
  }

  case class Var(s: String) extends Exp

  case class Wildcard() extends Exp

  case class Match(e: Exp, cases: List[(Exp, Exp)]) extends Exp

  case class Constructor0(s: String) extends Exp

  case class Constructor1(s: String, e1: Exp) extends Exp

  case class Constructor2(s: String, e1: Exp, e2: Exp) extends Exp

  case class Tuple2(e1: Exp, e2: Exp) extends Exp

}
