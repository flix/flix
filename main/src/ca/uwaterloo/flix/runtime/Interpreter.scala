package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.lang.ast.TypedAst.{Expression, Literal, Type, FormalArg}
import ca.uwaterloo.flix.lang.ast.{ParsedAst, BinaryOperator, UnaryOperator}

object Interpreter {
  type Env = Map[ParsedAst.Ident, Value]

  //TODO(mhyee): eval needs to take a Root. Implement Expression.Ref. Update tests.
//def eval(expr: Expression, root: TypedAst.Root, env: Env = Map()): Value = {
  def eval(expr: Expression, env: Env = Map()): Value = {
    expr match {
      case Expression.Lit(literal, _) => evalLit(literal)
      case Expression.Var(ident, _) =>
        assert(env.contains(ident), s"Expected variable ${ident.name} to be bound.")
        env(ident)
      case Expression.Ref(name, tpe) => ???
      case Expression.Lambda(formals, _, body, _) => Value.Closure(formals, body, env)
      case Expression.Apply(exp, args, _) => eval(exp, env) match {
          case Value.Closure(formals, body, closureEnv) =>
            val evalArgs = args.map(x => eval(x, env))
            val newEnv = closureEnv ++ formals.map(_.ident).zip(evalArgs).toMap
            eval(body, newEnv)
          case _ => assert(false, "Expected a function."); Value.Unit
        }
      case Expression.Unary(op, exp, _) => apply(op, eval(exp, env))
      case Expression.Binary(op, exp1, exp2, _) => apply(op, eval(exp1, env), eval(exp2, env))
      case Expression.IfThenElse(exp1, exp2, exp3, tpe) =>
        val cond = eval(exp1, env).toBool
        if (cond) eval(exp2, env) else eval(exp3, env)
      case Expression.Let(ident, value, body, tpe) =>
        // TODO(mhyee): Right now Let only supports a single binding. Does it make sense to allow a list of bindings?
        val func = Expression.Lambda(List(FormalArg(ident, value.tpe)), tpe,
          body, Type.Function(List(value.tpe), tpe))
        val desugared = Expression.Apply(func, List(value), tpe)
        eval(desugared, env)
      case Expression.Match(exp, rules, _) => ??? // TODO(mhyee): Implement this. Need to write a Unify function
      case Expression.Tag(name, ident, exp, _) => Value.Tag(name, ident.name, eval(exp, env))
      case Expression.Tuple(elms, _) => Value.Tuple(elms.map(e => eval(e, env)))
      case Expression.Error(location, tpe) => location.path match {
        case Some(path) => throw new RuntimeException(s"Error at $path:${location.line}:${location.column}")
        case None => throw new RuntimeException("Error at unknown location")
      }
    }
  }

  private def evalLit(lit: Literal): Value = lit match {
    case Literal.Unit => Value.Unit
    case Literal.Bool(b) => Value.Bool(b)
    case Literal.Int(i) => Value.Int(i)
    case Literal.Str(s) => Value.Str(s)
    case Literal.Tag(name, ident, innerLit, _) => Value.Tag(name, ident.name, evalLit(innerLit))
    case Literal.Tuple(elms, _) => Value.Tuple(elms.map(evalLit))
  }

  private def apply(op: UnaryOperator, v: Value): Value = op match {
    case UnaryOperator.Not => Value.Bool(!v.toBool)
    case UnaryOperator.UnaryPlus => Value.Int(+v.toInt)
    case UnaryOperator.UnaryMinus => Value.Int(-v.toInt)
  }

  private def apply(op: BinaryOperator, v1: Value, v2: Value): Value = op match {
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
    case BinaryOperator.And => Value.Bool(v1.toBool && v2.toBool)
    case BinaryOperator.Or => Value.Bool(v1.toBool || v2.toBool)
    case BinaryOperator.Minimum => Value.Int(math.min(v1.toInt, v2.toInt))
    case BinaryOperator.Maximum => Value.Int(math.max(v1.toInt, v2.toInt))
    case BinaryOperator.Union | BinaryOperator.Subset =>
      assert(false, "Can't have union or subset operators."); Value.Unit
  }
}
