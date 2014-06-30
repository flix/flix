package impl

/**
 * A term is either a constant value, a variable or a constructor.
 */
sealed trait Term {
  def isConst: Boolean = this match {
    case Term.Constant(c) => true
    case Term.Variable(v) => false
    case Term.Constructor0(name) => true
    case Term.Constructor1(name, t1) => t1.isConst
    case Term.Constructor2(name, t1, t2) => t1.isConst && t2.isConst
    case Term.Constructor3(name, t1, t2, t3) => t1.isConst && t2.isConst && t3.isConst
    case Term.Constructor4(name, t1, t2, t3, t4) => t1.isConst && t2.isConst && t3.isConst && t4.isConst
    case Term.Constructor5(name, t1, t2, t3, t4, t5) => t1.isConst && t2.isConst && t3.isConst && t4.isConst && t5.isConst
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

  // TODO: Variables and Predicates should have different symbols.
  
  /**
   * A null-ary constructor.
   */
  case class Constructor0(name: Symbol) extends Term

  /**
   * A 1-ary constructor.
   */
  case class Constructor1(name: Symbol, t1: Term) extends Term

  /**
   * A 2-ary constructor.
   */
  case class Constructor2(name: Symbol, t1: Term, t2: Term) extends Term

  /**
   * A 3-ary constructor.
   */
  case class Constructor3(name: Symbol, t1: Term, t2: Term, t3: Term) extends Term

  /**
   * A 4-ary constructor.
   */
  case class Constructor4(name: Symbol, t1: Term, t2: Term, t3: Term, t4: Term) extends Term

  /**
   * A 5-ary constructor.
   */
  case class Constructor5(name: Symbol, t1: Term, t2: Term, t3: Term, t4: Term, t5: Term) extends Term

}
