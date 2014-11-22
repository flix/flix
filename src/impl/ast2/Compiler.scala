package impl.ast2

import impl.logic._

class Compiler(ast: Ast.Root) {

  def run(): Unit = {

  }

  // environments
  // Typ Env
  // ValEnv
  // FunEnv
  // etc. etc.

  // TODO: Check
  // -unresolved references
  // -ambigious decls
  // -patterns with the same variable
  // -recursive types, calls, etc.


  object Desugaring {

  }

  object Environments {

    def typenv(name: Ast.Name, node: Ast): Map[Ast.Name, Ast.Declaration.TypeDecl] = ???

    // step 1: Check the FQN. Step 2. Check if the name is an extension.
    def lookupType: Unit = ???

    def merge: Unit = ???

  }

  object Linking {

  }

  object Translation {

    def compile(decl: Ast.Declaration) = ???

    def compile(exp: Ast.Expression): Term = exp match {
      case Ast.Expression.Var(x) => Term.Var(Symbol.VariableSymbol(x))

      case Ast.Expression.BoolLit(literal) => Term.Bool(literal)
      case Ast.Expression.IntLit(literal) => Term.Int(literal)
      case Ast.Expression.StrLit(literal) => Term.Str(literal)

      case Ast.Expression.IfThenElse(e1, e2, e3) => ???

      case Ast.Expression.Match(e, rules) => ???

      case Ast.Expression.Unary(op, e1) =>
        // Eliminates unary plus and minus.
        val t1 = compile(e1)
        op match {
          case UnaryOperator.Not => Term.UnaryOp(UnaryOperator.Not, t1)
          case UnaryOperator.UnaryPlus => t1
          case UnaryOperator.UnaryMinus => Term.BinaryOp(BinaryOperator.Minus, Term.Int(0), t1)
        }

      case Ast.Expression.Binary(op, e1, e2) =>
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
    private def compile(pattern: Ast.Pattern): Pattern = pattern match {
      case Ast.Pattern.Wildcard => Pattern.Wildcard
      case Ast.Pattern.Var(name) => Pattern.Var(Symbol.VariableSymbol(name))
      case Ast.Pattern.Bool(literal) => Pattern.Bool(literal)
      case Ast.Pattern.Int(literal) => Pattern.Int(literal)
      case Ast.Pattern.Str(literal) => Pattern.Str(literal)
      case Ast.Pattern.Tag(name, p1) => Pattern.Tag(compile(name), compile(p1))
      case Ast.Pattern.Tuple(Seq(p1, p2)) => Pattern.Tuple2(compile(p1), compile(p2))
      case Ast.Pattern.Tuple(Seq(p1, p2, p3)) => Pattern.Tuple3(compile(p1), compile(p2), compile(p3))
      case Ast.Pattern.Tuple(Seq(p1, p2, p3, p4)) => Pattern.Tuple4(compile(p1), compile(p2), compile(p3), compile(p4))
      case Ast.Pattern.Tuple(Seq(p1, p2, p3, p4, p5)) => Pattern.Tuple5(compile(p1), compile(p2), compile(p3), compile(p4), compile(p5))
      case Ast.Pattern.Tuple(elms) => throw new CompilerException("Tuples with more than 5 elements are not yet supported.")
    }


    /**
     * Compiles an ast type to a core type.
     */
    private def compile(typ: Ast.Type): Type = typ match {
      case Ast.Type.Unit => Type.Unit
      case Ast.Type.Bool => Type.Bool
      case Ast.Type.Int => Type.Int
      case Ast.Type.Str => Type.Str
      // case Ast.Type.Tag(name) => Type.Tag(), TODO
      case Ast.Type.Set(typ1) => Type.Set(compile(typ1))
      case Ast.Type.Map(keys, values) => throw CompilerException("Map types are currently not supported.")
      case Ast.Type.NameRef(name) => throw CompilerException(s"Unresolved named type: $name.")
    }

    /**
     * Compiles an ast name into a named symbol.
     */
    // TODO: Need internal visitor.
    private def compile(name: Ast.Name): Symbol.NamedSymbol = name match {
      //      case Ast.Name.Simple(x) => Symbol.VariableSymbol(x)
      case Ast.Name.Qualified(prefix, suffix) => ???
    }
  }


  case class CompilerException(msg: String) extends RuntimeException(msg)

}
