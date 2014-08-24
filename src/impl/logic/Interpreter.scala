package impl.logic

import impl.logic.Symbol.{VariableSymbol => VSym}

object Interpreter {

  /**
   * A big-step evaluator for lambda terms.
   */
  def evaluate(t: Term, env: Map[VSym, Term]): Value = t match {
    case Term.Unit => Value.Unit
    case Term.Bool(b) => Value.Bool(b)
    case Term.Int(i) => Value.Int(i)
    case Term.Str(s) => Value.Str(s)

    case Term.Variable(s) => ???
    case Term.Abs(s, t1) => ???
    case Term.App(Term.Abs(s, t1), t2) => ???

    case Term.IfThenElse(t1, t2, t3) =>
      val Value.Bool(b) = evaluate(t1, env)
      if (b)
        evaluate(t2, env)
      else
        evaluate(t3, env)

    case Term.UnaryOp(op, t1) =>
      val v1 = evaluate(t, env)
      apply(op, v1)

    case Term.BinaryOp(op, t1, t2) =>
      val v1 = evaluate(t1, env)
      val v2 = evaluate(t2, env)
      apply(op, v1, v2)


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
