package impl.ast2

import impl.logic._

import scala.collection.immutable

class Compiler(ast: Ast.Root) {

  def compile(a: Ast): Ast = {
    ???
  }



  // TODO: Check
  // -unresolved references
  // -ambigious decls
  // -patterns with the same variable
  // -recursive types, calls, etc.

  object Desugaring {

    //    def desugar[A <: Ast](ast: A): A = ast match {
    //      case Ast.Expression.Binary(e1, op, e2) => ???
    //
    //      // Desugar infix expression.
    //      case Ast.Expression.Infix(e1, name, e2) =>
    //        val es1 = desugar(e1)
    //        val es2 = desugar(e2)
    //        Ast.Expression.Call(Ast.Expression.VarOrNameRef(name), immutable.Seq(es1, es2))
    //
    //
    //      // Desugar relational type.
    //      case Ast.Type.Rel(elms) =>
    //        val elms2 = elms.map(desugar)
    //        Ast.Type.Set(Ast.Type.Tuple(elms2))
    //    }

    def desugar(a: Ast.Expression): Ast.Expression = a match {


      case Ast.Expression.Error => a
    }

    def desugar(a: Ast.Type): Ast.Type = a match {
      case Ast.Type.Bool => a

      case Ast.Type.Rel(elms) =>
        val elms2 = elms.map(desugar)
        Ast.Type.Set(Ast.Type.Tuple(elms2))
    }

  }

  object Linking {


    /**
     * Returns a map from fully qualified names to types.
     */
    //def typenv(context: Ast.Name, node: Ast): Unit = {
    //      node match {
    //        case Ast.Declaration.NameSpace(name, decl) => {
    //          // recurse
    //          // merge maps
    //          // add prefix to each entry in the map
    //        }
    //        case Ast.Declaration.TypeDecl(name, typ) => Map(Ast.Name.Simple(name) -> typ)
    //      }

    //   ???
    // }

    /**
     *
     */
    //def resolvesTypestypes


    // environments
    // Typ Env
    // ValEnv
    // FunEnv
    // etc. etc.

  }

  object Translation {

    def compile(decl: Ast.Declaration) = ???

    def compile(exp: Ast.Expression): Term = exp match {
      case Ast.Expression.Var(x) => Term.Var(Symbol.VariableSymbol(x))

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
    private def compile(pattern: Ast.Pattern): Pattern = pattern match {
      case Ast.Pattern.Wildcard => Pattern.Wildcard
      case Ast.Pattern.Var(name) => Pattern.Var(Symbol.VariableSymbol(name))
      case Ast.Pattern.Bool(literal) => Pattern.Bool(literal)
      case Ast.Pattern.Int(literal) => Pattern.Int(literal)
      case Ast.Pattern.Str(literal) => Pattern.Str(literal)
      //case Ast.Pattern.Tag(name, p1) => Pattern.Tag(compile(name), compile(p1))
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
      //case Ast.Type.Map(keys, values) => throw CompilerException("Map types are currently not supported.")
      case Ast.Type.NameRef(name) => throw CompilerException(s"Unresolved named type: $name.")
    }

    /**
     * Compiles an ast name into a named symbol.
     */
  }


  case class CompilerException(msg: String) extends RuntimeException(msg)

}
