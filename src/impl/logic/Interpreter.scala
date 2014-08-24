package impl.logic

import impl.logic.Symbol.{VariableSymbol => VSym}

object Interpreter {

  /**
   * A big-step evaluator for lambda terms.
   */
  def evaluate(t: Term, env: Map[VSym, Value]): Value = t match {
    case Term.Unit => Value.Unit
    case Term.Bool(b) => Value.Bool(b)
    case Term.Int(i) => Value.Int(i)
    case Term.Str(s) => Value.Str(s)

    case Term.Variable(s) => env(s)
    case Term.Abs(s, typ, t1) => Value.Abs(s, typ, t1)
    case Term.App(t1, t2) =>
      val Value.Abs(x, _, t3) = evaluate(t1, env)
      val argVal = evaluate(t2, env)
      val freshVar = Symbol.freshVariableSymbol(x)
      evaluate(t3.rename(x, freshVar), env + (freshVar -> argVal))

    case Term.Let(x, t1, t2) =>
      val v1 = evaluate(t1, env)
      val y = Symbol.freshVariableSymbol(x)
      evaluate(t2.rename(x, y), env + (y -> v1))

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

    case Term.Tagged(s, t1, typ) =>
      val v1 = evaluate(t1, env)
      Value.Tagged(s, v1)

    case Term.Tuple2(t1, t2) =>
      val v1 = evaluate(t1, env)
      val v2 = evaluate(t2, env)
      Value.Tuple2(v1, v2)

    case Term.Tuple3(t1, t2, t3) =>
      val v1 = evaluate(t1, env)
      val v2 = evaluate(t2, env)
      val v3 = evaluate(t3, env)
      Value.Tuple3(v1, v2, v3)

    case Term.Tuple4(t1, t2, t3, t4) =>
      val v1 = evaluate(t1, env)
      val v2 = evaluate(t2, env)
      val v3 = evaluate(t3, env)
      val v4 = evaluate(t4, env)
      Value.Tuple4(v1, v2, v3, v4)

    case Term.Tuple5(t1, t2, t3, t4, t5) =>
      val v1 = evaluate(t1, env)
      val v2 = evaluate(t2, env)
      val v3 = evaluate(t3, env)
      val v4 = evaluate(t4, env)
      val v5 = evaluate(t5, env)
      Value.Tuple5(v1, v2, v3, v4, v5)
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
