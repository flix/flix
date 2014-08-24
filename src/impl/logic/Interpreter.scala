package impl.logic

import impl.logic.Symbol.{VariableSymbol => VSym}
import impl.runtime.Error

object Interpreter {

  /**
   * A big-step evaluator for lambda terms.
   */
  // TODO: Env not used. Should return a value.
  def evaluate(t: Term, env: Map[VSym, Term]): Term = t match {
    case Term.Bool(b) => t
    case Term.Int(i) => t
    case Term.String(s) => t
    case Term.Constructor0(s) => t
    case Term.Constructor1(s, t1) => t
    case Term.Constructor2(s, t1, t2) => t
    case Term.Constructor3(s, t1, t2, t3) => t
    case Term.Constructor4(s, t1, t2, t3, t4) => t
    case Term.Constructor5(s, t1, t2, t3, t4, t5) => t
    case Term.Variable(s) => env.getOrElse(s, throw Error.UnboundVariable(s))
    case Term.Apply(s, t1) => t
    // Abstraction
    case Term.Abs(s, t1) => t
    // Application
    case Term.App(Term.Abs(s, t1), t2) => evaluate(t1.substitute(s, t2), env)
    case Term.App(t1, t2) => evaluate(Term.App(evaluate(t1, env), t2), env)
    // Unary Operator
    case Term.UnaryOp(op, t1) =>
      val v = evaluate(t, env)
      ???

    // Binary Operator
    case Term.BinaryOp(BinaryOperator.Equal, t1, t2) =>
      val v1 = evaluate(t1, env)
      val v2 = evaluate(t2, env)
      Term.Bool(v1 == v2)

    case Term.BinaryOp(op, t1, t2) => ???
    // If-Then-Else
    case Term.Ite(t1, t2, t3) =>
      val Value.Bool(c) = evaluate(t1, env).toBool
      if (c)
        evaluate(t2, env)
      else
        evaluate(t3, env)
  }

  /**
   * Returns the result of applying the unary operator `op` to the given value `v`.
   */
  def apply(op: UnaryOperator, v: Value): Value = op match {
    case UnaryOperator.Not => Value.Bool(!v.toBool)
    case UnaryOperator.UnaryPlus => Value.Int(v.toInt)
    case UnaryOperator.UnaryMinus => Value.Int(-v.toInt)
  }

  /**
   * Returns the result of applying the binary operator `op` to the given values `v1` and `v2`.
   */
  def apply(op: BinaryOperator, v1: Value, v2: Value): Value = op match {
    case BinaryOperator.Plus => Value.Int(v1.toInt + v2.toInt)
    case BinaryOperator.Minus => Value.Int(v1.toInt - v2.toInt)
    case BinaryOperator.Times => Value.Int(v1.toInt * v2.toInt)
    case BinaryOperator.Divide => Value.Int(v1.toInt / v2.toInt)
    case BinaryOperator.Modulo => Value.Int(v1.toInt % v2.toInt)
    case BinaryOperator.Less => Value.Bool(v1.toInt < v2.toInt)
    case BinaryOperator.LessEqual => Value.Bool(v1.toInt <= v2.toInt)
    case BinaryOperator.Greater => Value.Bool(v1.toInt > v2.toInt)
    case BinaryOperator.GreaterEqual => Value.Bool(v1.toInt >= v2.toInt)
    case BinaryOperator.Equal => Value.Bool(v1 == v2)
    case BinaryOperator.NotEqual => Value.Bool(v1 != v2)
  }
}
