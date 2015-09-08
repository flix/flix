package impl.logic

import ca.uwaterloo.flix.lang.ast.{BinaryOperator, UnaryOperator}

sealed trait Term {
  /**
   * Returns `this` term where all occurences (up to lambda- and let terms)
   * of the given variable `x` has been replaced by the variable `y`.
   */
  def rename(x: Symbol.VariableSymbol, y: Symbol.VariableSymbol): Term = substitute(x, Term.Var(y))

  /**
   * Returns `this` term where all occurences (up to lambda- and match terms)
   * of the given variable `x` has been replaced by the term `t`.
   */
  def substitute(x: Symbol.VariableSymbol, t: Term): Term = this match {
    case Term.Unit => Term.Unit
    case Term.Bool(b) => Term.Bool(b)
    case Term.Int(i) => Term.Int(i)
    case Term.Str(s) => Term.Str(s)
    case Term.Set(xs) => Term.Set(xs map (_.substitute(x, t)))

    case Term.Var(s) if s == x => t
    case Term.Var(s) => Term.Var(s)

    case Term.Abs(s, typ, t1) if s == x => this
    case Term.Abs(s, typ, t1) => Term.Abs(s, typ, t1.substitute(x, t))
    case Term.App(t1, t2) => Term.App(t1.substitute(x, t), t2.substitute(x, t))

    case Term.IfThenElse(t1, t2, t3) => Term.IfThenElse(t1.substitute(x, t), t2.substitute(x, t), t3.substitute(x, t))
    case Term.Match(t1, rules) => Term.Match(t1.substitute(x, t), rules map {
      case (p, t2) if p.freeVars contains x => (p, t2)
      case (p, t2) => (p, t2.substitute(x, t))
    })

    case Term.UnaryOp(op, t1) => Term.UnaryOp(op, t1.substitute(x, t))
    case Term.BinaryOp(op, t1, t2) => Term.BinaryOp(op, t1.substitute(x, t), t2.substitute(x, t))

    case Term.Tag(s, t1, typ) => Term.Tag(s, t1.substitute(x, t), typ)
    case Term.Tuple2(t1, t2) => Term.Tuple2(t1.substitute(x, t), t2.substitute(x, t))
    case Term.Tuple3(t1, t2, t3) => Term.Tuple3(t1.substitute(x, t), t2.substitute(x, t), t3.substitute(x, t))
    case Term.Tuple4(t1, t2, t3, t4) => Term.Tuple4(t1.substitute(x, t), t2.substitute(x, t), t3.substitute(x, t), t4.substitute(x, t))
    case Term.Tuple5(t1, t2, t3, t4, t5) => Term.Tuple5(t1.substitute(x, t), t2.substitute(x, t), t3.substitute(x, t), t4.substitute(x, t), t5.substitute(x, t))

    case Term.ScalaFunction(_) => this
    case Term.Native(_) => this
  }

  /**
   * Returns `this` term where all occurences of variables in the given
   * environment `env` have been replaced by their respective values.
   */
  def substitute(env: Map[Symbol.VariableSymbol, Value]): Term = env.foldLeft(this) {
    case (t, (x, v)) => t.substitute(x, v.toTerm)
  }

  /**
   * Returns `this` term where all occurences of variables in the given
   * environment `env` have been replaced by their respective values.
   */
  def substitute2(env: Map[Symbol.VariableSymbol, Term]): Term = env.foldLeft(this) {
    case (t, (x, v)) => t.substitute(x, v)
  }

  /**
   * Returns the set of free variables in the term.
   */
  def freeVariables: Set[Symbol.VariableSymbol] = this match {
    case Term.Unit => Set.empty
    case Term.Bool(b) => Set.empty
    case Term.Int(i) => Set.empty
    case Term.Str(s) => Set.empty
    case Term.Set(xs) => xs flatMap (_.freeVariables)

    case Term.Var(s) => Set(s)
    case Term.Abs(x, typ, t) => t.freeVariables - x
    case Term.App(t1, t2) => t1.freeVariables ++ t2.freeVariables

    case Term.UnaryOp(op, t) => t.freeVariables
    case Term.BinaryOp(op, t1, t2) => t1.freeVariables ++ t2.freeVariables
    case Term.IfThenElse(t1, t2, t3) => t1.freeVariables ++ t2.freeVariables ++ t3.freeVariables
    case Term.Match(t, rules) => t.freeVariables -- rules.flatMap(_._1.freeVars)

    case Term.Tag(s, t, typ) => t.freeVariables
    case Term.Tuple2(t1, t2) => t1.freeVariables ++ t2.freeVariables
    case Term.Tuple3(t1, t2, t3) => t1.freeVariables ++ t2.freeVariables ++ t3.freeVariables
    case Term.Tuple4(t1, t2, t3, t4) => t1.freeVariables ++ t2.freeVariables ++ t3.freeVariables ++ t4.freeVariables
    case Term.Tuple5(t1, t2, t3, t4, t5) => t1.freeVariables ++ t2.freeVariables ++ t3.freeVariables ++ t4.freeVariables ++ t5.freeVariables

    case Term.ScalaFunction(_) => Set.empty
  }

  /**
   * Optionally returns the term as a value.
   *
   * NB: *Does not* perform any computation to reduce the term to a value.
   */
  def asValue: Option[Value] = this match {
    case Term.Unit => Some(Value.Unit)
    case Term.Bool(b) => Some(Value.Bool(b))
    case Term.Int(i) => Some(Value.Int(i))
    case Term.Str(s) => Some(Value.Str(s))
    case Term.Set(xs) =>
      if (xs.isEmpty)
        Some(Value.Set(Set.empty))
      else
        xs.map(_.asValue).foldLeft(Option(Set.empty[Value])) {
          case (Some(ys), Some(z)) => Some(ys + z)
          case _ => None
        }.map(Value.Set)
    case Term.Abs(s, typ, t) => Some(Value.Abs(s, typ, t))
    case Term.Tag(n, t, typ) => t.asValue.map(Value.Tag(n, _, typ))
    case Term.Tuple2(t1, t2) =>
      for (v1 <- t1.asValue;
           v2 <- t2.asValue)
      yield Value.Tuple2(v1, v2)
    case Term.Tuple3(t1, t2, t3) =>
      for (v1 <- t1.asValue;
           v2 <- t2.asValue;
           v3 <- t3.asValue)
      yield Value.Tuple3(v1, v2, v3)
    case Term.Tuple4(t1, t2, t3, t4) =>
      for (v1 <- t1.asValue;
           v2 <- t2.asValue;
           v3 <- t3.asValue;
           v4 <- t4.asValue)
      yield Value.Tuple4(v1, v2, v3, v4)
    case Term.Tuple5(t1, t2, t3, t4, t5) =>
      for (v1 <- t1.asValue;
           v2 <- t2.asValue;
           v3 <- t3.asValue;
           v4 <- t4.asValue;
           v5 <- t5.asValue)
      yield Value.Tuple5(v1, v2, v3, v4, v5)

    case _ => None
  }
}

object Term {

  // TODO: Remove Bool and Str. (and their operations)
  // TODO: Try to simplify this language as much as possible.

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
   * A set constant term.
   */
  case class Set(xs: scala.collection.immutable.Set[Term]) extends Term

  /**
   * A variable term.
   */
  case class Var(name: Symbol.VariableSymbol) extends Term

  /**
   * A lambda term.
   */
  case class Abs(s: Symbol.VariableSymbol, typ: Type, t: Term) extends Term

  /**
   * An application term.
   */
  case class App(t1: Term, t2: Term) extends Term

  /**
   * A if-then-else term.
   */
  case class IfThenElse(t1: Term, t2: Term, t3: Term) extends Term

  /**
   * A match term.
   */
  case class Match(t: Term, rules: List[(Pattern, Term)]) extends Term

  /**
   * A unary operator term.
   */
  case class UnaryOp(op: UnaryOperator, t1: Term) extends Term

  /**
   * A binary operator term.
   */
  case class BinaryOp(op: BinaryOperator, t1: Term, t2: Term) extends Term

  // TODO: Consider introducing "typed" nodes

  /**
   * A tagged term.
   */
  case class Tag(name: Symbol.NamedSymbol, t: Term, typ: Type.Sum) extends Term

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
   * TODO: Introduce new terms starting here.
   */
//  case class BNot(t: Term) extends Term
//
//  case class BOr(t: Term) extends Term
//
//  case class BAnd(t1: Term, t2: Term) extends Term
//
//  case class IAdd(t1: Term, t2: Term) extends Term
//
//  case class ISub(t1: Term, t2: Term) extends Term
//



  /**
   * A native Scala/Java object.
   */
  case class Native(obj: Any) extends Term

  /**
   * A native Scala function.
   */
  case class ScalaFunction(fn: (Value => Value)) extends Term

}
