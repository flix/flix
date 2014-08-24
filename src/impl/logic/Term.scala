package impl.logic

import impl.logic.Symbol.NamedSymbol
import impl.runtime.{Error, Functions}

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
   * Optionally returns the term as a bool (under the empty environment.)
   */
  def asBool: Option[Value.Bool] = this match {
    case Term.Bool(b) => Some(Value.Bool(b))
    case _ => None
  }

  /**
   * Optionally returns the term as a value under the given environment `env`.
   *
   * Returns `None` if the term has free variables under the given environment.
   */
  def asValue(env: Map[Symbol.VariableSymbol, Value]): Option[Value] = this match {
    case Term.Bool(b) => Some(Value.Bool(b))
    case Term.Int(i) => Some(Value.Int(i))
    case Term.Str(s) => Some(Value.Str(s))
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
   * Returns the term as a boolean under the empty environment.
   *
   * Throws an exception if the term is not a bool value.
   */
  def toBool: Value.Bool = asBool.getOrElse(throw Error.TypeError2(Type.Bool, this))

  /**
   * Returns the term where all occurences (up to lambda- and let terms) of the given variable `x` has been replaced by `y`.
   */
  def rename(x: Symbol.VariableSymbol, y: Symbol.VariableSymbol): Term = this match {
    case Term.Unit =>     this
    case Term.Bool(b) =>  this
    case Term.Int(i) =>   this
    case Term.Str(s) =>   this

    case Term.Variable(s) if s == x => Term.Variable(y)
    case Term.Variable(s) => this

    case Term.Abs(s, typ, t) if s == x =>   this
    case Term.Abs(s, typ, t) =>             Term.Abs(s, typ, t.rename(x, y))
    case Term.App(t1, t2) =>                Term.App(t1.rename(x, y), t2.rename(x, y))

    case Term.Let(s, t1, t2) if s == x =>   this
    case Term.Let(s, t1, t2) =>             Term.Let(s, t1.rename(x, y), t2.rename(x, y))

    case Term.IfThenElse(t1, t2, t3) =>     Term.IfThenElse(t1.rename(x, y), t2.rename(x, y), t3.rename(x, y))
    case Term.UnaryOp(op, t1) =>            Term.UnaryOp(op, t1.rename(x, y))
    case Term.BinaryOp(op, t1, t2) =>       Term.BinaryOp(op, t1.rename(x, y), t2.rename(x, y))
    case Term.Tagged(s, t, typ) =>          Term.Tagged(s, t.rename(x, y), typ)
    case Term.Tuple2(t1, t2) =>             Term.Tuple2(t1.rename(x, y), t2.rename(x, y))
    case Term.Tuple3(t1, t2, t3) =>         Term.Tuple3(t1.rename(x, y), t2.rename(x, y), t3.rename(x, y))
    case Term.Tuple4(t1, t2, t3, t4) =>     Term.Tuple4(t1.rename(x, y), t2.rename(x, y), t3.rename(x, y), t4.rename(x, y))
    case Term.Tuple5(t1, t2, t3, t4, t5) => Term.Tuple5(t1.rename(x, y), t2.rename(x, y), t3.rename(x, y), t4.rename(x, y), t5.rename(x, y))
  }

  /**
   * Returns the term where all occurences of the variable symbol `s` has been replaced by the term `t`.
   */
  def substitute(x: Symbol.VariableSymbol, t: Term): Term = this match {
    case Term.Bool(b) => Term.Bool(b)
    case Term.Int(i) => Term.Int(i)
    case Term.Str(s) => Term.Str(s)
    case Term.Variable(y) if x == y => t
    case Term.Variable(y) => Term.Variable(y)
    case Term.Apply(s, ts) => Term.Apply(s, ts)

    case Term.App(t1, t2) => Term.App(t1.substitute(x, t), t2.substitute(x, t))
    case Term.UnaryOp(op, t1) => Term.UnaryOp(op, t1.substitute(x, t))
    case Term.BinaryOp(op, t1, t2) => Term.BinaryOp(op, t1.substitute(x, t), t2.substitute(x, t))
    case Term.IfThenElse(t1, t2, t3) => Term.IfThenElse(t1.substitute(x, t), t2.substitute(x, t), t3.substitute(x, t))
    case Term.Constructor0(s) => Term.Constructor0(s)
    case Term.Constructor1(s, t1) => Term.Constructor1(s, t1.substitute(x, t))
    case Term.Constructor2(s, t1, t2) => Term.Constructor2(s, t1.substitute(x, t), t2.substitute(x, t))
    case Term.Constructor3(s, t1, t2, t3) => Term.Constructor3(s, t1.substitute(x, t), t2.substitute(x, t), t3.substitute(x, t))
    case Term.Constructor4(s, t1, t2, t3, t4) => Term.Constructor4(s, t1.substitute(x, t), t2.substitute(x, t), t3.substitute(x, t), t4.substitute(x, t))
    case Term.Constructor5(s, t1, t2, t3, t4, t5) => Term.Constructor5(s, t1.substitute(x, t), t2.substitute(x, t), t3.substitute(x, t), t4.substitute(x, t), t5.substitute(x, t))
  }

  /**
   * Returns the set of free variables in the term.
   */
  def freeVariables: Set[Symbol.VariableSymbol] = this match {
    case Term.Bool(b) => Set.empty
    case Term.Int(i) => Set.empty
    case Term.Str(s) => Set.empty
    case Term.Variable(s) => Set(s)
    case Term.Apply(s, ts) => ts.flatMap(t => t.freeVariables).toSet
    case Term.App(t1, t2) => t1.freeVariables ++ t2.freeVariables
    case Term.UnaryOp(op, t) => t.freeVariables
    case Term.BinaryOp(op, t1, t2) => t1.freeVariables ++ t2.freeVariables
    case Term.IfThenElse(t1, t2, t3) => t1.freeVariables ++ t2.freeVariables ++ t3.freeVariables
    case Term.Constructor0(s) => Set.empty
    case Term.Constructor1(s, t1) => t1.freeVariables
    case Term.Constructor2(s, t1, t2) => t1.freeVariables ++ t2.freeVariables
    case Term.Constructor3(s, t1, t2, t3) => t1.freeVariables ++ t2.freeVariables ++ t3.freeVariables
    case Term.Constructor4(s, t1, t2, t3, t4) => t1.freeVariables ++ t2.freeVariables ++ t3.freeVariables ++ t4.freeVariables
    case Term.Constructor5(s, t1, t2, t3, t4, t5) => t1.freeVariables ++ t2.freeVariables ++ t3.freeVariables ++ t4.freeVariables ++ t5.freeVariables
  }

}

object Term {

  /**
   * The Unit term.
   */
  case object Unit extends Term

  /**
   * A boolean constant term.
   */
  case class Bool(b: scala.Boolean) extends Term

  /**
   * An integer constant term.
   */
  case class Int(i: scala.Int) extends Term

  /**
   * A string constant term.
   */
  case class Str(s: java.lang.String) extends Term

  /**
   * A variable term.
   */
  case class Variable(name: Symbol.VariableSymbol) extends Term

  /**
   * A lambda term.
   */
  case class Abs(s: Symbol.VariableSymbol, typ: Type, t: Term) extends Term

  /**
   * An application term.
   */
  case class App(t1: Term, t2: Term) extends Term

  /**
   * A let binding term.
   */
  case class Let(name: Symbol.VariableSymbol, t1: Term, t2: Term) extends Term

  /**
   * An if-then-else term.
   */
  case class IfThenElse(t1: Term, t2: Term, t3: Term) extends Term

  /**
   * A unary operator term.
   */
  case class UnaryOp(op: UnaryOperator, t1: Term) extends Term

  /**
   * A binary operator term.
   */
  case class BinaryOp(op: BinaryOperator, t1: Term, t2: Term) extends Term

  /**
   * A case term.
   */
  case class Case(t: Term, cases: Map[NamedSymbol, Term]) extends Term

  /**
   * A tagged term.
   */
  case class Tagged(name: Symbol.NamedSymbol, t: Term, typ: Type) extends Term

  /**
   * A 2-tuple term.
   */
  case class Tuple2(t1: Term, t2: Term) extends Term

  /**
   * A 3-tuple term.
   */
  case class Tuple3(t1: Term, t2: Term, t3: Term) extends Term

  /**
   * A 4-tuple term.
   */
  case class Tuple4(t1: Term, t2: Term, t3: Term, t4: Term) extends Term

  /**
   * A 5-tuple term.
   */
  case class Tuple5(t1: Term, t2: Term, t3: Term, t4: Term, t5: Term) extends Term

  /**
   * A function application term.
   */
  @deprecated("", "")
  case class Apply(name: Symbol.FunctionSymbol, terms: List[Term]) extends Term

  /**
   * A null-ary constructor.
   */
  @deprecated("", "")
  case class Constructor0(name: Symbol.NamedSymbol) extends Term

  /**
   * A 1-ary constructor.
   */
  @deprecated("", "")
  case class Constructor1(name: Symbol.NamedSymbol, t1: Term) extends Term

  /**
   * A 2-ary constructor.
   */
  @deprecated("", "")
  case class Constructor2(name: Symbol.NamedSymbol, t1: Term, t2: Term) extends Term

  /**
   * A 3-ary constructor.
   */
  @deprecated("", "")
  case class Constructor3(name: Symbol.NamedSymbol, t1: Term, t2: Term, t3: Term) extends Term

  /**
   * A 4-ary constructor.
   */
  @deprecated("", "")
  case class Constructor4(name: Symbol.NamedSymbol, t1: Term, t2: Term, t3: Term, t4: Term) extends Term

  /**
   * A 5-ary constructor.
   */
  @deprecated("", "")
  case class Constructor5(name: Symbol.NamedSymbol, t1: Term, t2: Term, t3: Term, t4: Term, t5: Term) extends Term

}
