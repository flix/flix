package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.lang.ast.TypedAst.{Expression, Literal}

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

    expr match {
      case Expression.Lit(literal, _) => evalLit(literal)
      case Expression.Var(name, tpe) => ???
      case Expression.Ref(name, tpe) => ???
      case Expression.Lambda(formals, retTpe, body, tpe) => ???
      case Expression.Apply(exp, args, tpe) => ???
      case Expression.Unary(op, exp, tpe) => ???  // TODO
      case Expression.Binary(op, exp1, exp2, tpe) => ???  // TODO
      case Expression.IfThenElse(exp1, exp2, e3, tpe) => ??? // TODO
      case Expression.Let(ident, value, body, tpe) => ???
      case Expression.Match(exp, rules, tpe) => ???
      case Expression.Tag(name, ident, exp, tpe) => ???
      case Expression.Tuple(elms, _) => Value.Tuple(elms.map(eval))
      case Expression.Ascribe(e, tpe) => ???
      case Expression.Error(location, tpe) => ???
    }
  }
}
