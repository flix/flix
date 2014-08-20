package impl.logic

import impl.runtime.Error

object Interpreter {

  /**
   * A big-step evaluator for lambda terms.
   */
  def reduce(t: Term, env: Map[Symbol.VariableSymbol, Term]): Term = t match {

    case Term.Bool(b) => t
    case Term.Int(i) => t
    case Term.String(s) => t
    case Term.Constructor0(s) => t
    case Term.Constructor1(s, t1) => t


    case Term.Variable(s) => env.get(s) match {
      case None => throw Error.UnboundVariable(s)
      case Some(t2) => t2
    }
    case Term.Apply(_, _) => t

    case Term.Abs(s, t1) => t

    case Term.App(Term.Abs(s, t1), t2) =>
      t1.substitute(s, t2)

    case Term.App(t1, t2) =>
      Term.App(reduce(t1, env), t2)

    case Term.Ite(t1, t2, t3) => reduce(t1, env).asValue match {
      case None => throw Error.TypeError2(Type.Bool, t1)
      case Some(Value.Bool(true)) => reduce(t2, env)
      case Some(Value.Bool(false)) => reduce(t3, env)
    }
  }

}
