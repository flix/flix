package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.lang.ast.TypedAst.{Expression, Literal, Type}
import ca.uwaterloo.flix.lang.ast.{BinaryOperator, UnaryOperator}

object Interpreter {
  def eval(expr: Expression): Value = {
    def evalLit(lit: Literal): Value = lit match {
      case Literal.Unit => Value.Unit
      case Literal.Bool(b) => Value.Bool(b)
      case Literal.Int(i) => Value.Int(i)
      case Literal.Str(s) => Value.Str(s)
      case Literal.Tag(ident, innerLit, _) => ???
      case Literal.Tuple(elms, _) => Value.Tuple(elms.map(evalLit))
    }

    def applyUnary(op: UnaryOperator, v: Value): Value = op match {
      case UnaryOperator.Not => Value.Bool(!v.toBool)
      case UnaryOperator.UnaryPlus => Value.Int(+v.toInt)
      case UnaryOperator.UnaryMinus => Value.Int(-v.toInt)
    }

    def applyBinary(op: BinaryOperator, v1: Value, v2: Value): Value = op match {
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
    }

    expr match {
      case Expression.Lit(literal, _) => evalLit(literal)
      case Expression.Var(name, tpe) => ???
      case Expression.Ref(name, tpe) => ???
      case Expression.Lambda(formals, retTpe, body, tpe) => ???
      case Expression.Apply(exp, args, tpe) => ???
      case Expression.Unary(op, exp, _) => applyUnary(op, eval(exp))
      case Expression.Binary(op, exp1, exp2, _) => applyBinary(op, eval(exp1), eval(exp2))
      case Expression.IfThenElse(exp1, exp2, exp3, tpe) =>
        val cond = eval(exp1).toBool
        if (cond) eval(exp2) else eval(exp3)
      case Expression.Let(ident, value, body, tpe) => ???
      case Expression.Match(exp, rules, tpe) => ???
      case Expression.Tag(name, ident, exp, tpe) => ???
      case Expression.Tuple(elms, _) => Value.Tuple(elms.map(eval))
      case Expression.Ascribe(e, tpe) => ???
      case Expression.Error(location, tpe) => ???
    }
  }
}
