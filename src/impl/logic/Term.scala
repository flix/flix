package impl.logic

sealed trait Term {
  /**
   * Returns `true` iff the term has no free variables.
   */
  def isConstant: Boolean = this match {
    case Term.Constant(c) => true
    case Term.Variable(v) => false
    case Term.Constructor0(name) => true
    case Term.Constructor1(name, t1) => t1.isConstant
    case Term.Constructor2(name, t1, t2) => t1.isConstant && t2.isConstant
    case Term.Constructor3(name, t1, t2, t3) => t1.isConstant && t2.isConstant && t3.isConstant
    case Term.Constructor4(name, t1, t2, t3, t4) => t1.isConstant && t2.isConstant && t3.isConstant && t4.isConstant
    case Term.Constructor5(name, t1, t2, t3, t4, t5) => t1.isConstant && t2.isConstant && t3.isConstant && t4.isConstant && t5.isConstant
  }
}

object Term {

  /**
   * A constant term.
   */
  case class Constant(v: Value) extends Term

  /**
   * A variable term.
   */
  case class Variable(name: scala.Symbol) extends Term

  /**
   * A null-ary constructor.
   */
  case class Constructor0(name: scala.Symbol) extends Term

  /**
   * A 1-ary constructor.
   */
  case class Constructor1(name: scala.Symbol, t1: Term) extends Term

  /**
   * A 2-ary constructor.
   */
  case class Constructor2(name: scala.Symbol, t1: Term, t2: Term) extends Term

  /**
   * A 3-ary constructor.
   */
  case class Constructor3(name: scala.Symbol, t1: Term, t2: Term, t3: Term) extends Term

  /**
   * A 4-ary constructor.
   */
  case class Constructor4(name: scala.Symbol, t1: Term, t2: Term, t3: Term, t4: Term) extends Term

  /**
   * A 5-ary constructor.
   */
  case class Constructor5(name: scala.Symbol, t1: Term, t2: Term, t3: Term, t4: Term, t5: Term) extends Term

}
