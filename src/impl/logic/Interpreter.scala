package impl.logic

import impl.runtime.Error

object Interpreter {

  /**
   * A big-step evaluator for lambda terms.
   */
  // TODO: Env not used.
  def reduce(t: Term, env: Map[Symbol.VariableSymbol, Term]): Term = t match {
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
    case Term.App(Term.Abs(s, t1), t2) => reduce(t1.substitute(s, t2), env)
    case Term.App(t1, t2) => reduce(Term.App(reduce(t1, env), t2), env)
    // Unary Operator
    case Term.UnaryOp(op, t1) => ???
    // Binary Operator
    case Term.BinaryOp(BinaryOperator.Eq, t1, t2) =>
      val v1 = reduce(t1, env)
      val v2 = reduce(t2, env)
      Term.Bool(v1 == v2)

    case Term.BinaryOp(op, t1, t2) => ???
    // If-Then-Else
    case Term.Ite(t1, t2, t3) =>
      val Value.Bool(c) = reduce(t1, env).toBool
      if (c)
        reduce(t2, env)
      else
        reduce(t3, env)
  }

}
