package impl.ast2

import impl.logic._

object Translation {

  def compile(decl: Ast.Declaration) = ???

  def compile(exp: Ast.Expression): Term = exp match {
    case Ast.Expression.Var(x) => Term.Var(Symbol.VariableSymbol(x))

    case Ast.Expression.IfThenElse(e1, e2, e3) => ???

    case Ast.Expression.Match(e, rules) => ???

    case Ast.Expression.Unary(op, e1) =>
      val t1 = compile(e1)
      op match {
        //case UnaryOperator.Not => Term.BNot(t1)
        case UnaryOperator.UnaryPlus => t1
        case UnaryOperator.UnaryMinus => Term.BinaryOp(BinaryOperator.Minus, Term.Int(0), t1)
      }

    case Ast.Expression.Binary(e1, op, e2) =>
      val t1 = compile(e1)
      val t2 = compile(e2)
      op match {
        case BinaryOperator.Plus => Term.BinaryOp(op, t1, t2)
        case BinaryOperator.Minus => Term.BinaryOp(op, t1, t2)
        case BinaryOperator.Times => Term.BinaryOp(op, t1, t2)
        case BinaryOperator.Divide => Term.BinaryOp(op, t1, t2)

        case BinaryOperator.Equal => Term.BinaryOp(op, t1, t2)
        case BinaryOperator.NotEqual => Term.UnaryOp(UnaryOperator.Not, Term.BinaryOp(BinaryOperator.Equal, t1, t2))

        case BinaryOperator.Greater => Term.BinaryOp(op, t1, t2)
        //case BinaryOperator.GreaterEqual => Term.BinaryOp
        case BinaryOperator.Less => Term.BinaryOp(BinaryOperator.Greater, t2, t1)

      }
  }

  /**
   * Compiles an ast pattern to a core pattern.
   */
  private def compile(pattern: Ast.MatchPattern): Pattern = pattern match {
    case Ast.MatchPattern.Wildcard => Pattern.Wildcard
    case Ast.MatchPattern.AmbigiousName(name) => Pattern.Var(Symbol.VariableSymbol(name))
    //case Ast.Pattern.Tag(name, p1) => Pattern.Tag(compile(name), compile(p1))
    case Ast.MatchPattern.Tuple(Seq(p1, p2)) => Pattern.Tuple2(compile(p1), compile(p2))
    case Ast.MatchPattern.Tuple(Seq(p1, p2, p3)) => Pattern.Tuple3(compile(p1), compile(p2), compile(p3))
    case Ast.MatchPattern.Tuple(Seq(p1, p2, p3, p4)) => Pattern.Tuple4(compile(p1), compile(p2), compile(p3), compile(p4))
    case Ast.MatchPattern.Tuple(Seq(p1, p2, p3, p4, p5)) => Pattern.Tuple5(compile(p1), compile(p2), compile(p3), compile(p4), compile(p5))
    case Ast.MatchPattern.Tuple(elms) => throw new RuntimeException("Tuples with more than 5 elements are not yet supported.")
  }


  /**
   * Compiles an ast type to a core type.
   */
  private def compile(typ: Ast.Type): Type = typ match {
    // case Ast.Type.Tag(name) => Type.Tag(), TODO
    case Ast.Type.Set(typ1) => Type.Set(compile(typ1))
    //case Ast.Type.Map(keys, values) => throw CompilerException("Map types are currently not supported.")
    case Ast.Type.AmbiguousName(name) => throw new RuntimeException(s"Unresolved named type: $name.")
  }

}
