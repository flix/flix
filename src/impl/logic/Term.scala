package impl.logic

import impl.runtime.{Functions, Error}

sealed trait Term {
  /**
   * Returns `true` iff the term is a value, i.e. has no free variables.
   */
  def isValue: Boolean = asValue.nonEmpty

  /**
   * Optionally returns the term as a value (under the empty environment.)
   */
  def asValue: Option[Value] = asValue(Map.empty)

  /**
   * Optionally returns the term as a value under the given environment `env`.
   *
   * Returns `None` if the term has free variables under the given environment.
   */
  def asValue(env: Map[Symbol.VariableSymbol, Value]): Option[Value] = this match {
    case Term.Constant(v) => Some(v)
    case Term.Variable(s) => env.get(s)
    case Term.Apply(s, args) => asValue(args, env) map (xs => Functions.evaluate(s, xs))
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

  /**
   * Optionally returns the given list of terms `ts` as a list of values under the given environment `env`.
   */
  def asValue(ts: List[Term], env: Map[Symbol.VariableSymbol, Value]): Option[List[Value]] = (ts :\ Option(List.empty[Value])) {
    case (t, None) => None
    case (t, Some(xs)) => t.asValue(env) map (v => v :: xs)
  }

  /**
   * Returns the term as a value under the empty environment.
   *
   * Throws an exception if the term is not a value.
   */
  def toValue: Value = toValue(Map.empty)

  /**
   * Returns the term as a value under the given environment `env`.
   *
   * Throws an exception if the term is not a value.
   */
  def toValue(env: Map[Symbol.VariableSymbol, Value]): Value = asValue(env) match {
    case None => throw Error.NonValueTerm(this)
    case Some(v) => v
  }

  /**
   * Returns the term where all occurences of the variable symbol `s` has been replaced by the term `t`.
   */
  def substitute(x: Symbol.VariableSymbol, t: Term): Term = this match {
    case Term.Constant(v) => Term.Constant(v)
    case Term.Variable(y) if x == y => t
    case Term.Variable(y) => Term.Variable(y)
    case Term.Apply(s, ts) => Term.Apply(s, ts)
    case Term.Constructor0(s) => Term.Constructor0(s)
    case Term.Constructor1(s, t1) => Term.Constructor1(s, t1.substitute(x, t))
    case Term.Constructor2(s, t1, t2) => Term.Constructor2(s, t1.substitute(x, t), t2.substitute(x, t))
    case Term.Constructor3(s, t1, t2, t3) => Term.Constructor3(s, t1.substitute(x, t), t2.substitute(x, t), t3.substitute(x, t))
    case Term.Constructor4(s, t1, t2, t3, t4) => Term.Constructor4(s, t1.substitute(x, t), t2.substitute(x, t), t3.substitute(x, t), t4.substitute(x, t))
    case Term.Constructor5(s, t1, t2, t3, t4, t5) => Term.Constructor5(s, t1.substitute(x, t), t2.substitute(x, t), t3.substitute(x, t), t4.substitute(x, t), t5.substitute(x, t))
  }

  /**
   * Returns the set of (free) variables in the term.
   */
  def variables: Set[Symbol.VariableSymbol] = this match {
    case Term.Constant(v) => Set.empty
    case Term.Variable(s) => Set(s)
    case Term.Apply(s, ts) => ts.flatMap(t => t.variables).toSet
    case Term.Constructor0(s) => Set.empty
    case Term.Constructor1(s, t1) => t1.variables
    case Term.Constructor2(s, t1, t2) => t1.variables ++ t2.variables
    case Term.Constructor3(s, t1, t2, t3) => t1.variables ++ t2.variables ++ t3.variables
    case Term.Constructor4(s, t1, t2, t3, t4) => t1.variables ++ t2.variables ++ t3.variables ++ t4.variables
    case Term.Constructor5(s, t1, t2, t3, t4, t5) => t1.variables ++ t2.variables ++ t3.variables ++ t4.variables ++ t5.variables
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
   * A function application term.
   */
  case class Apply(name: Symbol.FunctionSymbol, terms: List[Term]) extends Term

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
