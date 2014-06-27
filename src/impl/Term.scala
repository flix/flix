package impl

/**
 * A term is either a constant value, a variable or a destructor.
 */
sealed trait Term {
  def isConst: Boolean = this match {
    case Term.Constant(c) => true
    case Term.Variable(v) => false
    case Term.Destructor0(name) => true
    case Term.Destructor1(name, t1) => t1.isConst
    case Term.Destructor2(name, t1, t2) => t1.isConst && t2.isConst
    case Term.Destructor3(name, t1, t2, t3) => t1.isConst && t2.isConst && t3.isConst
    case Term.Destructor4(name, t1, t2, t3, t4) => t1.isConst && t2.isConst && t3.isConst && t4.isConst
    case Term.Destructor5(name, t1, t2, t3, t4, t5) => t1.isConst && t2.isConst && t3.isConst && t4.isConst && t5.isConst
      // TODO: Should these not rather be called constructors?
  }
}

case object Term {

  /**
   * A constant term.
   */
  case class Constant(value: Value) extends Term

  /**
   * A variable term.
   */
  case class Variable(name: Symbol) extends Term

  /**
   * A null-ary destructor.
   */
  case class Destructor0(name: Symbol) extends Term

  /**
   * A 1-ary destructor.
   */
  case class Destructor1(name: Symbol, t1: Term) extends Term

  /**
   * A 2-ary destructor.
   */
  case class Destructor2(name: Symbol, t1: Term, t2: Term) extends Term

  /**
   * A 3-ary destructor.
   */
  case class Destructor3(name: Symbol, t1: Term, t2: Term, t3: Term) extends Term

  /**
   * A 4-ary destructor.
   */
  case class Destructor4(name: Symbol, t1: Term, t2: Term, t3: Term, t4: Term) extends Term

  /**
   * A 5-ary destructor.
   */
  case class Destructor5(name: Symbol, t1: Term, t2: Term, t3: Term, t4: Term, t5: Term) extends Term

}
