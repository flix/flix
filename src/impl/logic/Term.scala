package impl.logic

import impl.logic.Symbol.VariableSymbol

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

  /**
   * Optionally returns `this` term as a value.
   */
  def asValue(env: Map[VariableSymbol, Value]): Option[Value] = this match {
    case Term.Constant(v) => Some(v)
    case Term.Variable(s) => env.get(s)
    case Term.Constructor0(s) => Some(Value.Constructor0(s))
    case Term.Constructor1(s, t1) =>
      for (v1 <- t1.asValue(env))
      yield Value.Constructor1(s, v1)

    case Term.Constructor2(s, t1, t2) =>
      for (v1 <- t1.asValue(env);
           v2 <- t2.asValue(env))
      yield Value.Constructor2(s, v1, v2)

    case Term.Constructor3(s, t1, t2, t3) =>
      for (v1 <- t1.asValue(env);
           v2 <- t2.asValue(env);
           v3 <- t3.asValue(env))
      yield Value.Constructor3(s, v1, v2, v3)

    case Term.Constructor4(s, t1, t2, t3, t4) =>
      for (v1 <- t1.asValue(env);
           v2 <- t2.asValue(env);
           v3 <- t3.asValue(env);
           v4 <- t4.asValue(env))
      yield Value.Constructor4(s, v1, v2, v3, v4)

    case Term.Constructor5(s, t1, t2, t3, t4, t5) =>
      for (v1 <- t1.asValue(env);
           v2 <- t2.asValue(env);
           v3 <- t3.asValue(env);
           v4 <- t4.asValue(env);
           v5 <- t5.asValue(env)
      )
      yield Value.Constructor5(s, v1, v2, v3, v4, v5)
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
  case class Variable(name: Symbol.VariableSymbol) extends Term

  /**
   * A null-ary constructor.
   */
  case class Constructor0(name: Symbol.NamedSymbol) extends Term

  /**
   * A 1-ary constructor.
   */
  case class Constructor1(name: Symbol.NamedSymbol, t1: Term) extends Term

  /**
   * A 2-ary constructor.
   */
  case class Constructor2(name: Symbol.NamedSymbol, t1: Term, t2: Term) extends Term

  /**
   * A 3-ary constructor.
   */
  case class Constructor3(name: Symbol.NamedSymbol, t1: Term, t2: Term, t3: Term) extends Term

  /**
   * A 4-ary constructor.
   */
  case class Constructor4(name: Symbol.NamedSymbol, t1: Term, t2: Term, t3: Term, t4: Term) extends Term

  /**
   * A 5-ary constructor.
   */
  case class Constructor5(name: Symbol.NamedSymbol, t1: Term, t2: Term, t3: Term, t4: Term, t5: Term) extends Term

}
